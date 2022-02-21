import matplotlib.pyplot as plt
import matplotlib.animation as animation

fichier_entree = "simulation.dat"
couleur_ligament_detendu = (0.0 , 0.0 , 1.0)#(0.27, 0.51, 0.71)
couleur_ligament_tendu = (1.0, 0.0, 0.0 )#(0.70, 0.13, 0.13)

xmin = -5
xmax = 30
ymin = -5
ymax = 30

fig = plt.figure("Articulation du genou") # initialise la figure
plt.title("Modèle 4 barres")

plt.xlim(xmin, xmax)
plt.ylim(ymin, ymax)


class barre:
    def __init__(self, x1, y1, x2, y2):
        self._ligne, = plt.plot([], [], color=couleur_ligament_detendu, linewidth=5)
        self._x1 = x1
        self._y1 = y1
        self._x2 = x2
        self._y2 = y2
    def tracer(self, i):
        self._ligne.set_data( [ self._x1[i], self._x2[i] ], [ self._y1[i], self._y2[i] ])

class ligament(barre):
    def __init__(self, x1, y1, x2, y2):
        super().__init__(x1, y1, x2, y2)
        self._l_min = self._longueur(0)
        self._l_max = self._longueur(0)
        for i in range(1,len(x1)):
            l = self._longueur(i)
            if l > self._l_max :
                self._l_max = l
            if l < self._l_min :
                self._l_min = l
    def _longueur(self, i):
        return ( (self._x2[i] - self._x1[i])**2 + (self._y2[i] - self._y1[i])**2 )**0.5
    def _couleur(self, i):
        t = (self._longueur(i) - self._l_min) / (self._l_max - self._l_min)
        return tuple(map(lambda i, j: i*t + (1-t)*j, couleur_ligament_detendu, couleur_ligament_tendu))
    def tracer(self, i):
        super().tracer(i)
        self._ligne.set_color(self._couleur(i))

def animer(i):
    #tracer le modèle
    femur.tracer(i)
    tibia.tracer(i)
    LCA.tracer(i)
    LCP.tracer(i)


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
LCA = ligament(xa, ya, xb, yb)
LCP = ligament(xc, yc, xd, yd)


anim = animation.FuncAnimation(fig, animer, frames=n, interval=5000//n, repeat=True) #5000/len(xa) => l'animation se fait sur 5 secondes


plt.show()
