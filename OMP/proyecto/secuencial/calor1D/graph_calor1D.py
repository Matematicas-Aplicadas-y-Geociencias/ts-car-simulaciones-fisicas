from pathlib import Path
import numpy as np
import matplotlib

matplotlib.use("Agg")
import matplotlib.pyplot as plt

repo = Path(__file__).resolve().parents[2]
data_path = repo / "secuencial" / "calor1D" / "fort.101"
out_path = (
    repo / "docs" / "figures" / "secuencial" / "calor1D" / "temperatura_1d.png"
)

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
out_path.parent.mkdir(parents=True, exist_ok=True)
fig.savefig(out_path, dpi=200)
