from pathlib import Path
import numpy as np
import matplotlib

matplotlib.use("Agg")
import matplotlib.pyplot as plt

case_dir = Path(__file__).resolve().parent
tablas_dir = case_dir / "resultados" / "tablas"
figuras_dir = case_dir / "resultados" / "figuras"
data_path = tablas_dir / "resultado_tabla.dat"
out_path = figuras_dir / "temperatura_1d.png"

data = np.loadtxt(data_path)
x = data[:, 0]
T = data[:, 1]

fig, ax = plt.subplots(figsize=(6, 4))
ax.plot(x, T, marker="o", linewidth=1.5, markersize=3)
ax.set_xlabel("x [m]")
ax.set_ylabel("T [°C]")
ax.set_title("Perfil de temperatura 1D")
ax.grid(True, alpha=0.3)

fig.tight_layout()
figuras_dir.mkdir(parents=True, exist_ok=True)
fig.savefig(out_path, dpi=200)
