import matplotlib.pyplot as plt
import numpy as np
import os

os.makedirs('../proyecto/figures', exist_ok=True)

data = []
with open('solucion.dat', 'r') as f:
    for line in f:
        parts = line.split()
        if len(parts) == 3:
            try:
                data.append([float(p) for p in parts])
            except ValueError:
                continue

data = np.array(data)
x = np.unique(data[:, 0])
y = np.unique(data[:, 1])
nx = len(x)
ny = len(y)

T = data[:, 2].reshape(nx, ny).T

plt.figure(figsize=(10, 5))
plt.pcolormesh(x, y, T, shading='auto', cmap='hot')
plt.colorbar(label='Temperatura')
plt.xlabel('x')
plt.ylabel('y')
plt.title('Heatmap - Calor 2D')
plt.savefig('../proyecto/figures/calor2D_acc_smoke.png')
print("Imagen guardada en ../proyecto/figures/calor2D_acc_smoke.png")
