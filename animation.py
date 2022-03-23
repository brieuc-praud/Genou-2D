import matplotlib.pyplot as plt
import matplotlib.animation as animation

fichier_entree = "simulation.dat"
fichier_parametres = "parametres.dat" #pour lire les longueurs à vide des ligaments
couleur_traction = (1.0, 0.0, 0.0 )#rouge
couleur_compression = (0.0 , 0.0 , 1.0)#bleu
couleur_neutre = (0.0, 0.0, 1.0)#bleu

xmin = -5
xmax = 30
ymin = -5
ymax = 30

fig = plt.figure("Articulation du genou") # initialise la figure
plt.title("Modèle 4 barres")

with open(fichier_parametres, "r") as fichier:
    donnees = fichier.readline().split() #On split la première ligne du fichier
    l0_LCA = float(donnees[0])
    l0_LCP = float(donnees[2])

plt.xlim(xmin, xmax)
plt.ylim(ymin, ymax)


class barre:
    def __init__(self, x1, y1, x2, y2):
        self._ligne, = plt.plot([], [], color=couleur_neutre, linewidth=5)
        self._x1 = x1
        self._y1 = y1
        self._x2 = x2
        self._y2 = y2
    def tracer(self, i):
        self._ligne.set_data( [ self._x1[i], self._x2[i] ], [ self._y1[i], self._y2[i] ])

class ligament(barre):
    def __init__(self, x1, y1, x2, y2, l0):
        super().__init__(x1, y1, x2, y2)
        self._l0 = l0
    def _longueur(self, i):
        return ( (self._x2[i] - self._x1[i])**2 + (self._y2[i] - self._y1[i])**2 )**0.5
    def allongement(self, i):
        return self._longueur(i) - self._l0
    def tracer(self, i):
        super().tracer(i)
    def colorer(self, i, allongement_max):
        t = 0.5*( self.allongement(i) + allongement_max ) / allongement_max
        self._ligne.set_color(  tuple(map(lambda i, j: i*t + (1-t)*j, couleur_compression, couleur_traction))  )
        
def animer(i):
    #tracer le modèle
    femur.tracer(i)
    tibia.tracer(i)
    LCA.tracer(i)
    LCP.tracer(i)
    LCA.colorer(i, allongement_max)
    LCP.colorer(i, allongement_max)


#lecture fichier donnees
with open(fichier_entree, "r") as fichier:
    xb=list()
    yb=list()
    xc=list()
    yc=list()
    xa=list()
    ya=list()
    xd=list()
    yd=list()
    for ligne in fichier:
        donnees = ligne.split()
        xb.append(float(donnees[0]))
        yb.append(float(donnees[1]))
        xc.append(float(donnees[2]))
        yc.append(float(donnees[3]))
        xa.append(float(donnees[4]))
        ya.append(float(donnees[5]))
        xd.append(float(donnees[6]))
        yd.append(float(donnees[7]))

n = len(xa)

#Les 4 barres
femur = barre(xb, yb, xc, yc)
tibia = barre(xd, yd, xa, ya)
LCP = ligament(xc, yc, xd, yd, l0_LCP)
LCA = ligament(xa, ya, xb, yb, l0_LCA)#le ligament antérieur est devant le ligament postérieur

#On détermine ici l'allongement maximal afin de l'avoir comme référence pour définir la couleur des ligaments
allongement_max = max( abs(LCA.allongement(0)) , abs(LCP.allongement(0)) )
for i in range(1,n):
    allongement = max( abs(LCA.allongement(i)) , abs(LCP.allongement(i)) )
    if allongement > allongement_max:
        allongement_max = allongement

anim = animation.FuncAnimation(fig, animer, frames=n, interval=5000//n, repeat=True) #5000//n => l'animation se fait sur 5 secondes (à condition que 5000//n > 1)


plt.show()
