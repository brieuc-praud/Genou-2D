import matplotlib.pyplot as plt
import matplotlib.animation as animation

fichier_entree = "simulation.dat"

xmin = -5
xmax = 30
ymin = -5
ymax = 30



fig = plt.figure("Articulation du genou") # initialise la figure
plt.title("Modèle 4 barres")

ligne, = plt.plot([], [])#ligne qui représentera notre modèle

plt.xlim(xmin, xmax)
plt.ylim(ymin, ymax)


#lecture fichier donnees
with open(fichier_entree, "r") as f:
    xb=list()
    yb=list()
    xc=list()
    yc=list()
    xa=list()
    ya=list()
    xd=list()
    yd=list()
    for line in f:
        data = line.split()
        xb.append(float(data[0]))
        yb.append(float(data[1]))
        xc.append(float(data[2]))
        yc.append(float(data[3]))
        xa.append(float(data[4]))
        ya.append(float(data[5]))
        xd.append(float(data[6]))
        yd.append(float(data[7]))


def animer(i):
    ligne.set_data([xb[i],xc[i],xd[i],xa[i],xb[i]],[yb[i],yc[i],yd[i],ya[i],yb[i]])
    return ligne,

anim = animation.FuncAnimation(fig, animer, frames=len(xa), interval=50, repeat=True)


plt.show()