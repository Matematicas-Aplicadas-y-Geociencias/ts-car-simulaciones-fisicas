import numpy as np
import matplotlib
import matplotlib.pyplot as plt
matplotlib.use("Agg")


data = np.loadtxt("solucion.dat")

x = data[:, 0]
y = data[:, 1]
T = data[:, 2]

nx = np.unique(x).size
ny = np.unique(y).size

Tgrid = T.reshape((ny, nx))

plt.figure()
plt.imshow(
	Tgrid,
	origin="lower",
	cmap="jet",  # bajos: azul oscuro, altos: rojo
	vmin=T.min(),
	vmax=T.max(),
)
plt.colorbar(label="Temperatura")
plt.title("Distribución de temperatura")
plt.xlabel("x")
plt.ylabel("y")
plt.tight_layout()
plt.savefig("calor2D_plate_blue_red.png", dpi=200)
