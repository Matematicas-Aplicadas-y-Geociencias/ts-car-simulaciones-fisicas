#!/usr/bin/env python3
"""Grafica comparativa de aceleracion entre dos archivos .dat.

Cada archivo de entrada debe tener tres columnas:

    hilos tiempo speedup
"""

from __future__ import annotations

import argparse
from pathlib import Path


ROOT_DIR = Path(__file__).resolve().parent.parent
DOCS_DIR = ROOT_DIR.parent / "docs"
TABLES_DIR = DOCS_DIR / "tables"
FIGURES_DIR = DOCS_DIR / "figures"


def parse_args() -> argparse.Namespace:
    parser = argparse.ArgumentParser(
        description="Construye tabla y grafica comparativa de speedup."
    )
    parser.add_argument("--ours", required=True, help="Archivo .dat de nuestra version.")
    parser.add_argument("--reference", required=True, help="Archivo .dat de referencia.")
    parser.add_argument(
        "--output-prefix",
        default="comparacion_speedup_final",
        help="Prefijo para .dat y .png de salida.",
    )
    parser.add_argument(
        "--title",
        default="Comparacion de aceleracion",
        help="Titulo de la grafica.",
    )
    parser.add_argument(
        "--ours-label",
        default="Paralelizaci\u00f3n 2D",
        help="Etiqueta de la primera curva.",
    )
    parser.add_argument(
        "--reference-label",
        default="Paralelizaci\u00f3n 2D - Referencia (main branch)",
        help="Etiqueta de la segunda curva.",
    )
    parser.add_argument(
        "--time-output-prefix",
        default=None,
        help="Prefijo opcional para guardar tambien la grafica comparativa de tiempos.",
    )
    parser.add_argument(
        "--time-title",
        default=None,
        help="Titulo opcional para la grafica comparativa de tiempos.",
    )
    return parser.parse_args()


def read_dat(path: Path) -> dict[int, tuple[float, float]]:
    rows: dict[int, tuple[float, float]] = {}
    with path.open("r", encoding="utf-8") as handle:
        for line in handle:
            stripped = line.strip()
            if not stripped or stripped.startswith("#"):
                continue
            threads_text, time_text, speedup_text = stripped.split()[:3]
            rows[int(threads_text)] = (float(time_text), float(speedup_text))
    return rows


def main() -> None:
    args = parse_args()
    ours_path = Path(args.ours).resolve()
    reference_path = Path(args.reference).resolve()

    ours = read_dat(ours_path)
    reference = read_dat(reference_path)
    threads = sorted(set(ours) & set(reference))
    if not threads:
        raise SystemExit("No hay hilos comunes entre los archivos de entrada.")

    TABLES_DIR.mkdir(parents=True, exist_ok=True)
    out_dat = TABLES_DIR / f"{args.output_prefix}.dat"
    with out_dat.open("w", encoding="utf-8") as handle:
        handle.write("# hilos tiempo_nuestra speedup_nuestra tiempo_referencia speedup_referencia\n")
        for thread in threads:
            our_time, our_speedup = ours[thread]
            ref_time, ref_speedup = reference[thread]
            handle.write(
                f"{thread} {our_time:.6f} {our_speedup:.6f} "
                f"{ref_time:.6f} {ref_speedup:.6f}\n"
            )

    try:
        import matplotlib.pyplot as plt
    except ImportError as exc:
        raise SystemExit("No se pudo importar matplotlib.") from exc

    ideal = threads
    ours_speedup = [ours[thread][1] for thread in threads]
    reference_speedup = [reference[thread][1] for thread in threads]

    plt.figure(figsize=(8, 5))
    plt.plot(threads, ideal, "--", color="tab:red", linewidth=1.5, label="Ideal")
    plt.plot(
        threads,
        ours_speedup,
        "-o",
        color="black",
        linewidth=1.8,
        markersize=6,
        label=args.ours_label,
    )
    plt.plot(
        threads,
        reference_speedup,
        "-s",
        color="tab:blue",
        linewidth=1.8,
        markersize=5,
        label=args.reference_label,
    )
    plt.xlabel("Hilos")
    plt.ylabel("Aceleracion")
    plt.title(args.title)
    plt.xticks(threads)
    plt.grid(True, alpha=0.3)
    plt.legend()
    plt.tight_layout()

    FIGURES_DIR.mkdir(parents=True, exist_ok=True)
    out_png = FIGURES_DIR / f"{args.output_prefix}.png"
    plt.savefig(out_png, dpi=200)
    plt.close()

    print(f"Tabla comparativa: {out_dat}")
    print(f"Grafica comparativa: {out_png}")

    if args.time_output_prefix:
        ours_time = [ours[thread][0] for thread in threads]
        reference_time = [reference[thread][0] for thread in threads]

        plt.figure(figsize=(8, 5))
        plt.plot(
            threads,
            ours_time,
            "-o",
            color="black",
            linewidth=1.8,
            markersize=6,
            label=args.ours_label,
        )
        plt.plot(
            threads,
            reference_time,
            "-s",
            color="tab:blue",
            linewidth=1.8,
            markersize=5,
            label=args.reference_label,
        )
        plt.xlabel("Hilos")
        plt.ylabel("Tiempo [s]")
        if args.time_title is None:
            args.time_title = "Comparacion de tiempos"
        plt.title(args.time_title)
        plt.xticks(threads)
        plt.grid(True, alpha=0.3)
        plt.legend()
        plt.tight_layout()

        out_time_png = FIGURES_DIR / f"{args.time_output_prefix}.png"
        plt.savefig(out_time_png, dpi=200)
        plt.close()
        print(f"Grafica de tiempos: {out_time_png}")


if __name__ == "__main__":
    main()
