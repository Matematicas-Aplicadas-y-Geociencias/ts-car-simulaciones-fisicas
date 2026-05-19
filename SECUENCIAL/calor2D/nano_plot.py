from pathlib import Path

import matplotlib

matplotlib.use("Agg")
import matplotlib.pyplot as plt
import numpy as np

case_dir = Path(__file__).resolve().parent
tablas_dir = case_dir / "resultados" / "tablas"
figuras_dir = case_dir / "resultados" / "figuras"
data_path = tablas_dir / "resultado_malla.dat"
out_path = figuras_dir / "calor2D_plate_blue_red.png"

data = np.loadtxt(data_path)
x = data[:, 0]
y = data[:, 1]
T = data[:, 2]

x_pts = np.unique(x)
y_pts = np.unique(y)
nx = x_pts.size
ny = y_pts.size

# Orden Fortran: bucle jj, luego ii en calor2D.f90
Tgrid = T.reshape((ny, nx))

# Sin extent, imshow usa 0..nx y 0..ny (indices), no lx y ly del archivo.
extent = [x_pts[0], x_pts[-1], y_pts[0], y_pts[-1]]

fig, ax = plt.subplots(figsize=(8, 4))
im = ax.imshow(
    Tgrid,
    origin="lower",
    extent=extent,
    aspect="equal",
    cmap="jet",
    vmin=T.min(),
    vmax=T.max(),
)
fig.colorbar(im, label="Temperatura [°C]")
ax.set_title("Distribución de temperatura")
ax.set_xlabel("x [m]")
ax.set_ylabel("y [m]")
fig.tight_layout()
figuras_dir.mkdir(parents=True, exist_ok=True)
fig.savefig(out_path, dpi=200)
