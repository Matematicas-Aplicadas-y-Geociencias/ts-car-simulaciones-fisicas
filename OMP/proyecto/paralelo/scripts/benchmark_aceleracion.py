#!/usr/bin/env python3
"""Benchmark de aceleracion OpenMP para calor2D.

Compila el caso paralelo, ejecuta varias corridas por cantidad de hilos,
promedia el tiempo de pared y genera:

- un archivo .dat con hilos, tiempo promedio y aceleracion real
- una grafica con la curva ideal y la curva real
"""

from __future__ import annotations

import argparse
import os
import statistics
import subprocess
import sys
import time
from pathlib import Path


ROOT_DIR = Path(__file__).resolve().parent.parent
CODE_DIR = ROOT_DIR / "src" / "calor2d"
DOCS_DIR = ROOT_DIR.parent / "docs"
TABLES_DIR = DOCS_DIR / "tables"
FIGURES_DIR = DOCS_DIR / "figures"
BIN_DIR = ROOT_DIR / "bin"


def parse_args() -> argparse.Namespace:
    parser = argparse.ArgumentParser(
        description=(
            "Compila los .f90 indicados, ejecuta varias corridas por numero "
            "de hilos y construye la grafica de aceleracion ideal vs real."
        )
    )
    parser.add_argument(
        "--code-dir",
        default=str(CODE_DIR),
        help=(
            "Carpeta donde viven los archivos fuente y donde se ejecutara "
            "el binario. Default: paralelo/src/calor2d."
        ),
    )
    parser.add_argument(
        "--sources",
        nargs="+",
        default=["calor2D_utils.f90", "calor2D_parallel.f90"],
        help=(
            "Lista de archivos .f90 a compilar, en el orden deseado. "
            "Default: calor2D_utils.f90 calor2D_parallel.f90."
        ),
    )
    parser.add_argument(
        "--min-threads",
        type=int,
        default=1,
        help="Numero minimo de hilos desde donde empezar. Default: 1.",
    )
    parser.add_argument(
        "--max-threads",
        type=int,
        default=8,
        help="Numero maximo de hilos a medir. Default: 8.",
    )
    parser.add_argument(
        "--runs",
        type=int,
        default=10,
        help="Numero de corridas por cada punto real. Default: 10.",
    )
    parser.add_argument(
        "--warmups",
        type=int,
        default=0,
        help="Corridas de calentamiento por cantidad de hilos; no se guardan. Default: 0.",
    )
    parser.add_argument(
        "--stat",
        choices=["mean", "median", "best"],
        default="mean",
        help="Estadistico usado para la tabla: mean, median o best. Default: mean.",
    )
    parser.add_argument(
        "--output-prefix",
        default="aceleracion_ideal_vs_real",
        help="Prefijo para los archivos de salida en docs/tables y docs/figures.",
    )
    parser.add_argument(
        "--exe-name",
        default="calor2D_bench",
        help="Nombre del ejecutable temporal a generar dentro de paralelo/bin.",
    )
    parser.add_argument(
        "--plot-title",
        default=None,
        help="Titulo opcional para la grafica. Si no se indica, se genera uno simple.",
    )
    return parser.parse_args()


def read_dat_simple(path: Path) -> dict[int, tuple[float, float]]:
    rows: dict[int, tuple[float, float]] = {}
    with path.open("r", encoding="utf-8") as handle:
        for line in handle:
            stripped = line.strip()
            if not stripped or stripped.startswith("#"):
                continue
            parts = stripped.split()
            rows[int(parts[0])] = (float(parts[1]), float(parts[2]))
    return rows


def compile_program(code_dir: Path, sources: list[str], exe_path: Path) -> None:
    command = [
        "gfortran",
        "-O3",
        "-fopenmp",
        *sources,
        "-o",
        str(exe_path),
    ]
    result = subprocess.run(
        command,
        cwd=code_dir,
        capture_output=True,
        text=True,
        check=False,
    )
    if result.returncode != 0:
        sys.stderr.write("Fallo la compilacion.\n")
        sys.stderr.write(f"Directorio: {code_dir}\n")
        sys.stderr.write(f"Fuentes: {' '.join(sources)}\n")
        if result.stdout:
            sys.stderr.write(result.stdout)
        if result.stderr:
            sys.stderr.write(result.stderr)
        raise SystemExit(result.returncode)


def run_once(code_dir: Path, exe_path: Path, threads: int) -> tuple[float, str]:
    env = os.environ.copy()
    env["OMP_NUM_THREADS"] = str(threads)

    start = time.perf_counter()
    result = subprocess.run(
        [str(exe_path)],
        cwd=code_dir,
        capture_output=True,
        text=True,
        env=env,
        check=False,
    )
    elapsed = time.perf_counter() - start

    if result.returncode != 0:
        sys.stderr.write(
            f"La corrida con {threads} hilo(s) fallo con codigo "
            f"{result.returncode}.\n"
        )
        if result.stdout:
            sys.stderr.write(result.stdout)
        if result.stderr:
            sys.stderr.write(result.stderr)
        raise SystemExit(result.returncode)

    return elapsed, result.stdout.strip()


def benchmark(
    code_dir: Path,
    exe_path: Path,
    min_threads: int,
    max_threads: int,
    runs: int,
    warmups: int,
    stat: str,
    baseline_time: float | None = None,
) -> list[tuple[int, float, float]]:
    results: list[tuple[int, float, float]] = []

    for threads in range(min_threads, max_threads + 1):
        for warmup_idx in range(1, warmups + 1):
            elapsed, summary = run_once(code_dir, exe_path, threads)
            print(
                f"[hilos={threads:2d} warmup={warmup_idx:2d}/{warmups}] "
                f"{elapsed:8.3f} s"
            )
            if summary:
                print(f"  {summary}")

        samples: list[float] = []
        for run_idx in range(1, runs + 1):
            elapsed, summary = run_once(code_dir, exe_path, threads)
            samples.append(elapsed)
            print(
                f"[hilos={threads:2d} corrida={run_idx:2d}/{runs}] "
                f"{elapsed:8.3f} s"
            )
            if summary:
                print(f"  {summary}")

        if stat == "median":
            mean_time = statistics.median(samples)
        elif stat == "best":
            mean_time = min(samples)
        else:
            mean_time = statistics.fmean(samples)
        if baseline_time is None:
            baseline_time = mean_time

        speedup = baseline_time / mean_time
        results.append((threads, mean_time, speedup))

        print(
            f"{stat} hilos={threads:2d}: {mean_time:.3f} s, "
            f"aceleracion={speedup:.3f}"
        )

    return results


def save_data(
    results: list[tuple[int, float, float]],
    output_prefix: str,
    existing: dict[int, tuple[float, float]] | None = None,
) -> Path:
    TABLES_DIR.mkdir(parents=True, exist_ok=True)
    data_path = TABLES_DIR / f"{output_prefix}.dat"
    # Fusionar: los resultados nuevos sobreescriben filas existentes
    merged: dict[int, tuple[float, float]] = dict(existing) if existing else {}
    for threads, mean_time, speedup in results:
        merged[threads] = (mean_time, speedup)
    with data_path.open("w", encoding="utf-8") as handle:
        for threads in sorted(merged):
            mean_time, speedup = merged[threads]
            handle.write(f"{threads} {mean_time:.6f} {speedup:.6f}\n")
    return data_path


def save_plot(
    results: list[tuple[int, float, float]],
    output_prefix: str,
    runs: int,
    plot_title: str | None,
) -> Path:
    try:
        import matplotlib.pyplot as plt
    except ImportError as exc:
        raise SystemExit(
            "No se pudo importar matplotlib. El archivo .dat se genero, "
            "pero la grafica no pudo construirse."
        ) from exc

    threads = [row[0] for row in results]
    real_speedup = [row[2] for row in results]
    ideal_speedup = threads

    plt.figure(figsize=(8, 5))
    plt.plot(
        threads,
        ideal_speedup,
        "--",
        color="tab:red",
        linewidth=1.5,
        label="Ideal",
    )
    plt.plot(
        threads,
        real_speedup,
        "-o",
        color="black",
        linewidth=1.8,
        markersize=6,
        label="Aceleracion real",
    )
    plt.xlabel("Hilos")
    plt.ylabel("Aceleracion")
    if plot_title is None:
        plot_title = f"Aceleracion ideal vs real ({runs} corridas por punto)"
    plt.title(plot_title)
    plt.xticks(threads)
    plt.grid(True, alpha=0.3)
    plt.legend()
    plt.tight_layout()

    FIGURES_DIR.mkdir(parents=True, exist_ok=True)
    plot_path = FIGURES_DIR / f"{output_prefix}.png"
    plt.savefig(plot_path, dpi=200)
    plt.close()
    return plot_path


def main() -> None:
    args = parse_args()

    if args.min_threads < 1:
        raise SystemExit("--min-threads debe ser >= 1.")
    if args.max_threads < args.min_threads:
        raise SystemExit("--max-threads debe ser >= --min-threads.")
    if args.runs < 1:
        raise SystemExit("--runs debe ser >= 1.")
    if args.warmups < 0:
        raise SystemExit("--warmups debe ser >= 0.")
    if not args.sources:
        raise SystemExit("Debes indicar al menos un archivo en --sources.")

    code_dir = Path(args.code_dir).resolve()
    if not code_dir.is_dir():
        raise SystemExit(f"No existe el directorio: {code_dir}")
    BIN_DIR.mkdir(parents=True, exist_ok=True)

    missing_sources = [src for src in args.sources if not (code_dir / src).is_file()]
    if missing_sources:
        missing_text = ", ".join(missing_sources)
        raise SystemExit(f"No se encontraron estos archivos en {code_dir}: {missing_text}")

    exe_path = BIN_DIR / args.exe_name

    print("Compilando caso paralelo...")
    compile_program(code_dir, args.sources, exe_path)
    # Si min_threads > 1, leer el dat existente para obtener baseline (t1) y filas previas
    existing: dict[int, tuple[float, float]] | None = None
    preloaded_baseline: float | None = None
    if args.min_threads > 1:
        dat_path = TABLES_DIR / f"{args.output_prefix}.dat"
        if dat_path.is_file():
            existing = read_dat_simple(dat_path)
            if 1 in existing:
                preloaded_baseline = existing[1][0]
                print(f"Baseline cargada del archivo existente: t1={preloaded_baseline:.3f} s")
            else:
                raise SystemExit(
                    f"No se encontro hilos=1 en {dat_path}. "
                    "Necesitas la medicion de 1 hilo para calcular la aceleracion."
                )
        else:
            raise SystemExit(
                f"--min-threads={args.min_threads} > 1 pero no existe {dat_path}. "
                "Corre primero desde hilos=1."
            )

    print(
        f"Ejecutando benchmark de {args.min_threads} a {args.max_threads} hilos, "
        f"{args.runs} corridas por punto, "
        f"{args.warmups} warmup(s), stat={args.stat}..."
    )
    results = benchmark(
        code_dir,
        exe_path,
        args.min_threads,
        args.max_threads,
        args.runs,
        args.warmups,
        args.stat,
        baseline_time=preloaded_baseline,
    )

    data_path = save_data(results, args.output_prefix, existing=existing)
    print(f"Datos guardados en: {data_path}")

    plot_path = save_plot(results, args.output_prefix, args.runs, args.plot_title)
    print(f"Grafica guardada en: {plot_path}")


if __name__ == "__main__":
    main()
