"""
plot_benchmark.py — Grafica los resultados del benchmark de calor2D.

Lee benchmark_results.csv y genera dos paneles:
  - Tiempo de ejecución vs número de procesadores
  - Speedup real vs ideal
"""

import csv
import os
import matplotlib.pyplot as plt

WORK_DIR  = os.path.dirname(os.path.abspath(__file__))
DATA_FILE = os.path.join(WORK_DIR, "benchmark_results.csv")
OUT_FILE  = os.path.join(WORK_DIR, "benchmark_plot.png")


def load_data():
    threads, times_mean, times_stddev, times_min, times_max = [], [], [], [], []
    with open(DATA_FILE) as f:
        for row in csv.DictReader(f):
            threads.append(int(row["threads"]))
            times_mean.append(float(row["time_mean"]))
            times_stddev.append(float(row["time_stddev"]))
            times_min.append(float(row["time_min"]))
            times_max.append(float(row["time_max"]))
    return threads, times_mean, times_stddev, times_min, times_max


def main():
    threads, times_mean, times_stddev, times_min, times_max = load_data()
    t_s     = times_mean[0]                     # tiempo serial promedio (1 hilo)
    speedup = [t_s / t for t in times_mean]
    effic   = [sp / p * 100 for sp, p in zip(speedup, threads)]
    
    # Barras de error para speedup (propagación de incertidumbre)
    # Si t = t_mean ± stddev, entonces S = t_s/t => dS/dt = -t_s/t²
    # Error en speedup ≈ (t_s / t_mean²) * stddev
    speedup_err = [(t_s / (t**2)) * std for t, std in zip(times_mean, times_stddev)]
    effic_err   = [err / p for err, p in zip(speedup_err, threads)]

    fig, axes = plt.subplots(1, 3, figsize=(15, 5))
    fig.suptitle("Benchmark calor2D 128×128 (OpenMP)", fontsize=13, fontweight="bold")

    # ------------------------------------------------------------------
    # Panel 1 — Tiempo vs procesadores (con barras de error)
    # ------------------------------------------------------------------
    ax = axes[0]
    ax.errorbar(threads, times_mean, yerr=times_stddev, 
                fmt="o-", color="steelblue", ecolor="steelblue", 
                linewidth=2, markersize=6, capsize=4, capthick=1.5, alpha=0.8)
    
    # Sombreado min-max como región de incertidumbre
    ax.fill_between(threads, times_min, times_max, alpha=0.15, color="steelblue", label="Rango [min, max]")
    
    ax.set_xlabel("Número de procesadores")
    ax.set_ylabel("Tiempo (s)")
    ax.set_title("Tiempo de ejecución")
    ax.set_xticks(threads)
    ax.grid(True, alpha=0.35)
    ax.legend(fontsize=8)

    # Anota el tiempo serial
    ax.annotate(
        f"$t_s$ = {t_s:.2f} s",
        xy=(threads[0], times_mean[0]),
        xytext=(threads[1] + 0.3, times_mean[0]),
        fontsize=9,
        arrowprops=dict(arrowstyle="->", color="gray"),
    )

    # ------------------------------------------------------------------
    # Panel 2 — Speedup (con barras de error)
    # ------------------------------------------------------------------
    ax = axes[1]
    ax.errorbar(threads, speedup, yerr=speedup_err,
                fmt="o-", color="darkorange", ecolor="darkorange",
                linewidth=2, markersize=6, capsize=4, capthick=1.5, alpha=0.8,
                label="Speedup real")
    ax.plot(threads, threads, "--", color="gray", linewidth=1.5,
            alpha=0.7, label="Speedup ideal")
    ax.set_xlabel("Número de procesadores")
    ax.set_ylabel("Speedup  $S = t_s / t_p$")
    ax.set_title("Speedup")
    ax.legend(fontsize=9)
    ax.set_xticks(threads)
    ax.grid(True, alpha=0.35)

    # ------------------------------------------------------------------
    # Panel 3 — Eficiencia (con barras de error)
    # ------------------------------------------------------------------
    ax = axes[2]
    ax.errorbar(threads, effic, yerr=effic_err,
                fmt="o-", color="seagreen", ecolor="seagreen",
                linewidth=2, markersize=6, capsize=4, capthick=1.5, alpha=0.8)
    ax.axhline(100, color="gray", linestyle="--", linewidth=1.5,
               alpha=0.7, label="Eficiencia ideal (100%)")
    ax.set_xlabel("Número de procesadores")
    ax.set_ylabel("Eficiencia  $E = S/p$ (%)")
    ax.set_title("Eficiencia paralela")
    ax.legend(fontsize=9)
    ax.set_xticks(threads)
    ax.set_ylim(0, 110)
    ax.grid(True, alpha=0.35)

    plt.tight_layout()
    plt.savefig(OUT_FILE, dpi=150, bbox_inches="tight")
    print(f"Gráfica guardada en: {OUT_FILE}")
    plt.show()


if __name__ == "__main__":
    main()
