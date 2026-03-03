import numpy as np
import matplotlib
matplotlib.use("TkAgg")
import matplotlib.pyplot as plt

data = np.loadtxt("solucion.dat")

x = data[:, 0]
y = data[:, 1]
T = data[:, 2]

nx = int(max(x))
ny = int(max(y))

Tgrid = T.reshape((ny, nx))

plt.figure()
plt.imshow(Tgrid, origin="lower")
plt.colorbar(label="Temperatura")
plt.title("Distribución de temperatura")
plt.xlabel("x")
plt.ylabel("y")
plt.show()

plt.tight_layout()
plt.savefig("malla.png", dpi=300)
print("Guardado: malla.png")
