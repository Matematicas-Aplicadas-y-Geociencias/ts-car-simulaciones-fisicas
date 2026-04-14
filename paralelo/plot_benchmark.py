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
    threads, times = [], []
    with open(DATA_FILE) as f:
        for row in csv.DictReader(f):
            threads.append(int(row["threads"]))
            times.append(float(row["time_s"]))
    return threads, times


def main():
    threads, times = load_data()
    t_s     = times[0]                          # tiempo serial (1 hilo)
    speedup = [t_s / t for t in times]
    effic   = [sp / p * 100 for sp, p in zip(speedup, threads)]

    fig, axes = plt.subplots(1, 3, figsize=(15, 5))
    fig.suptitle("Benchmark calor2D 128×128 (OpenMP)", fontsize=13, fontweight="bold")

    # ------------------------------------------------------------------
    # Panel 1 — Tiempo vs procesadores
    # ------------------------------------------------------------------
    ax = axes[0]
    ax.plot(threads, times, "o-", color="steelblue", linewidth=2, markersize=6)
    ax.set_xlabel("Número de procesadores")
    ax.set_ylabel("Tiempo (s)")
    ax.set_title("Tiempo de ejecución")
    ax.set_xticks(threads)
    ax.grid(True, alpha=0.35)

    # Anota el tiempo serial
    ax.annotate(
        f"$t_s$ = {t_s:.2f} s",
        xy=(threads[0], times[0]),
        xytext=(threads[1] + 0.3, times[0]),
        fontsize=9,
        arrowprops=dict(arrowstyle="->", color="gray"),
    )

    # ------------------------------------------------------------------
    # Panel 2 — Speedup
    # ------------------------------------------------------------------
    ax = axes[1]
    ax.plot(threads, speedup, "o-", color="darkorange", linewidth=2,
            markersize=6, label="Speedup real")
    ax.plot(threads, threads, "--", color="gray", linewidth=1.5,
            alpha=0.7, label="Speedup ideal")
    ax.set_xlabel("Número de procesadores")
    ax.set_ylabel("Speedup  $S = t_s / t_p$")
    ax.set_title("Speedup")
    ax.legend(fontsize=9)
    ax.set_xticks(threads)
    ax.grid(True, alpha=0.35)

    # ------------------------------------------------------------------
    # Panel 3 — Eficiencia
    # ------------------------------------------------------------------
    ax = axes[2]
    ax.plot(threads, effic, "o-", color="seagreen", linewidth=2, markersize=6)
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
