#!/usr/bin/env python3
"""Benchmark OpenACC para Thomas aplicado al problema de conduccion 2D.

El script compila la misma fuente en modo serial y en modo OpenACC, ejecuta
varias corridas por caso, valida checksum/residuo contra el caso serial y
genera tablas y graficas reproducibles para el reporte.
"""

from __future__ import annotations

import argparse
import os
import re
import statistics
import subprocess
import sys
import time
from dataclasses import dataclass
from pathlib import Path


ROOT_DIR = Path(__file__).resolve().parent.parent
REPO_ROOT = ROOT_DIR.parent
CODE_DIR = ROOT_DIR / "paralelo" / "calor2D"
BIN_DIR = ROOT_DIR / "build"
PROJECT_DIR = REPO_ROOT / "PROYECTO"
TABLES_DIR = PROJECT_DIR / "tables"
FIGURES_DIR = PROJECT_DIR / "figures"
LOGS_DIR = PROJECT_DIR / "logs"

FLOAT_RE = r"[-+]?(?:\d+(?:\.\d*)?|\.\d+)(?:[Ee][-+]?\d+)?"
RESIDUO_RE = re.compile(rf"^residuo=\s*({FLOAT_RE})$", re.MULTILINE)
CHECKSUM_RE = re.compile(rf"^checksum=\s*({FLOAT_RE})$", re.MULTILINE)


@dataclass(frozen=True)
class BuildTarget:
    name: str
    backend: str
    flags: tuple[str, ...]


@dataclass(frozen=True)
class RunCase:
    name: str
    backend: str
    exe_path: Path
    acc_num_cores: int


@dataclass
class RunSummary:
    case: RunCase
    time_s: float
    speedup: float
    residuo: float
    checksum: float
    checksum_diff: float
    samples: list[tuple[float, float, float]]


def parse_args() -> argparse.Namespace:
    parser = argparse.ArgumentParser(
        description=(
            "Compila y mide el solver de conduccion 2D con Thomas en serial "
            "y OpenACC."
        )
    )
    parser.add_argument(
        "--code-dir",
        default=str(CODE_DIR),
        help="Directorio con las fuentes Fortran. Default: ACC/paralelo/calor2D.",
    )
    parser.add_argument(
        "--sources",
        nargs="+",
        default=["calor2D_acc_utils.f90", "calor2D_acc.f90"],
        help="Fuentes a compilar, en orden. Default: calor2D_acc_utils.f90 calor2D_acc.f90.",
    )
    parser.add_argument(
        "--compiler",
        default="nvfortran",
        help="Compilador Fortran. Default: nvfortran.",
    )
    parser.add_argument("--nx", type=int, default=480, help="Puntos en x. Default: 480.")
    parser.add_argument("--ny", type=int, default=240, help="Puntos en y. Default: 240.")
    parser.add_argument(
        "--itermax",
        type=int,
        default=10000,
        help="Iteraciones fijas del benchmark. Default: 10000.",
    )
    parser.add_argument(
        "--cases",
        nargs="+",
        choices=["serial", "multicore", "gpu"],
        default=["serial", "multicore"],
        help="Casos a medir. Default: serial multicore.",
    )
    parser.add_argument(
        "--cores",
        nargs="+",
        type=int,
        default=[1, 2, 4, 8],
        help="Valores de ACC_NUM_CORES para el backend multicore. Default: 1 2 4 8.",
    )
    parser.add_argument(
        "--runs",
        type=int,
        default=5,
        help="Corridas medidas por caso. Default: 5.",
    )
    parser.add_argument(
        "--warmups",
        type=int,
        default=1,
        help="Corridas de calentamiento por caso. Default: 1.",
    )
    parser.add_argument(
        "--stat",
        choices=["mean", "median", "best"],
        default="mean",
        help="Estadistico reportado. Default: mean.",
    )
    parser.add_argument(
        "--output-prefix",
        default="acc_thomas_conduccion",
        help="Prefijo para tablas, logs y grafica.",
    )
    parser.add_argument(
        "--checksum-rtol",
        type=float,
        default=1.0e-9,
        help="Tolerancia relativa del checksum contra serial. Default: 1e-9.",
    )
    parser.add_argument(
        "--checksum-atol",
        type=float,
        default=1.0e-9,
        help="Tolerancia absoluta del checksum contra serial. Default: 1e-9.",
    )
    parser.add_argument(
        "--skip-plot",
        action="store_true",
        help="Solo genera tablas, sin construir la grafica.",
    )
    return parser.parse_args()


def ensure_args(args: argparse.Namespace) -> None:
    if args.nx < 3 or args.ny < 3:
        raise SystemExit("--nx y --ny deben ser >= 3.")
    if args.itermax < 1:
        raise SystemExit("--itermax debe ser >= 1.")
    if args.runs < 1:
        raise SystemExit("--runs debe ser >= 1.")
    if args.warmups < 0:
        raise SystemExit("--warmups debe ser >= 0.")
    if "serial" not in args.cases:
        raise SystemExit("Incluye el caso serial para calcular speedup y validar checksum.")
    if any(core < 1 for core in args.cores):
        raise SystemExit("Todos los valores de --cores deben ser >= 1.")


def check_gpu_available() -> None:
    result = subprocess.run(
        ["nvaccelinfo"],
        capture_output=True,
        text=True,
        check=False,
    )
    text = f"{result.stdout}\n{result.stderr}"
    if result.returncode != 0 or "No accelerators found" in text:
        raise SystemExit(
            "Se pidio --cases gpu, pero nvaccelinfo no encontro aceleradores. "
            "Ejecuta sin gpu o corre en una maquina con GPU NVIDIA visible."
        )


def build_targets(args: argparse.Namespace) -> list[BuildTarget]:
    base_flags = (
        "-O3",
        "-Mpreprocess",
        f"-DNX={args.nx}",
        f"-DNY={args.ny}",
        f"-DITERMAX={args.itermax}",
    )

    targets: list[BuildTarget] = []
    if "serial" in args.cases:
        targets.append(BuildTarget("serial", "serial", base_flags))
    if "multicore" in args.cases:
        targets.append(
            BuildTarget(
                "acc_multicore",
                "multicore",
                (*base_flags, "-acc=multicore", "-Minfo=accel"),
            )
        )
    if "gpu" in args.cases:
        check_gpu_available()
        targets.append(
            BuildTarget("acc_gpu", "gpu", (*base_flags, "-acc=gpu", "-Minfo=accel"))
        )
    return targets


def compile_target(
    compiler: str,
    code_dir: Path,
    sources: list[str],
    target: BuildTarget,
    output_prefix: str,
) -> Path:
    exe_path = BIN_DIR / f"{target.name}_{target.backend}"
    command = [compiler, *target.flags, *sources, "-o", str(exe_path)]
    result = subprocess.run(
        command,
        cwd=code_dir,
        capture_output=True,
        text=True,
        check=False,
    )

    LOGS_DIR.mkdir(parents=True, exist_ok=True)
    log_path = LOGS_DIR / f"{output_prefix}_{target.name}_compile.log"
    with log_path.open("w", encoding="utf-8") as handle:
        handle.write("$ " + " ".join(command) + "\n\n")
        if result.stdout:
            handle.write(result.stdout)
        if result.stderr:
            handle.write(result.stderr)

    if result.returncode != 0:
        sys.stderr.write(f"Fallo la compilacion de {target.name}.\n")
        sys.stderr.write(f"Log: {log_path}\n")
        if result.stdout:
            sys.stderr.write(result.stdout)
        if result.stderr:
            sys.stderr.write(result.stderr)
        raise SystemExit(result.returncode)

    return exe_path


def make_run_cases(
    targets: dict[str, Path],
    cores: list[int],
) -> list[RunCase]:
    cases = [RunCase("serial", "serial", targets["serial"], 0)]
    if "acc_multicore" in targets:
        for core in cores:
            cases.append(
                RunCase(f"acc_multicore_{core}", "multicore", targets["acc_multicore"], core)
            )
    if "acc_gpu" in targets:
        cases.append(RunCase("acc_gpu", "gpu", targets["acc_gpu"], 0))
    return cases


def parse_solver_output(stdout: str) -> tuple[float, float]:
    residuo_match = RESIDUO_RE.search(stdout)
    checksum_match = CHECKSUM_RE.search(stdout)
    if not residuo_match or not checksum_match:
        raise ValueError("No se pudo leer residuo/checksum de la salida del solver.")
    return float(residuo_match.group(1)), float(checksum_match.group(1))


def run_once(code_dir: Path, case: RunCase) -> tuple[float, float, float, str]:
    env = os.environ.copy()
    if case.backend == "multicore":
        env["ACC_NUM_CORES"] = str(case.acc_num_cores)

    start = time.perf_counter()
    result = subprocess.run(
        [str(case.exe_path)],
        cwd=code_dir,
        capture_output=True,
        text=True,
        env=env,
        check=False,
    )
    elapsed = time.perf_counter() - start

    if result.returncode != 0:
        sys.stderr.write(f"La corrida {case.name} fallo con codigo {result.returncode}.\n")
        if result.stdout:
            sys.stderr.write(result.stdout)
        if result.stderr:
            sys.stderr.write(result.stderr)
        raise SystemExit(result.returncode)

    try:
        residuo, checksum = parse_solver_output(result.stdout)
    except ValueError as exc:
        sys.stderr.write(result.stdout)
        if result.stderr:
            sys.stderr.write(result.stderr)
        raise SystemExit(str(exc)) from exc

    return elapsed, residuo, checksum, result.stdout.strip()


def choose_stat(samples: list[float], stat: str) -> float:
    if stat == "median":
        return statistics.median(samples)
    if stat == "best":
        return min(samples)
    return statistics.fmean(samples)


def benchmark_case(
    code_dir: Path,
    case: RunCase,
    runs: int,
    warmups: int,
    stat: str,
) -> tuple[float, float, float, list[tuple[float, float, float]]]:
    for warmup_idx in range(1, warmups + 1):
        elapsed, residuo, checksum, _ = run_once(code_dir, case)
        print(
            f"[{case.name} warmup={warmup_idx}/{warmups}] "
            f"{elapsed:.6f} s residuo={residuo:.6e} checksum={checksum:.6e}"
        )

    samples: list[tuple[float, float, float]] = []
    for run_idx in range(1, runs + 1):
        elapsed, residuo, checksum, _ = run_once(code_dir, case)
        samples.append((elapsed, residuo, checksum))
        print(
            f"[{case.name} corrida={run_idx}/{runs}] "
            f"{elapsed:.6f} s residuo={residuo:.6e} checksum={checksum:.6e}"
        )

    times = [sample[0] for sample in samples]
    residuos = [sample[1] for sample in samples]
    checksums = [sample[2] for sample in samples]
    return (
        choose_stat(times, stat),
        choose_stat(residuos, stat),
        choose_stat(checksums, stat),
        samples,
    )


def validate_checksums(
    summaries: list[RunSummary],
    reference_checksum: float,
    rtol: float,
    atol: float,
) -> None:
    for summary in summaries:
        limit = atol + rtol*abs(reference_checksum)
        if summary.checksum_diff > limit:
            raise SystemExit(
                f"Checksum fuera de tolerancia en {summary.case.name}: "
                f"diff={summary.checksum_diff:.6e}, limite={limit:.6e}"
            )


def save_tables(summaries: list[RunSummary], output_prefix: str) -> tuple[Path, Path]:
    TABLES_DIR.mkdir(parents=True, exist_ok=True)
    summary_path = TABLES_DIR / f"{output_prefix}.dat"
    raw_path = TABLES_DIR / f"{output_prefix}_raw.dat"

    with summary_path.open("w", encoding="utf-8") as handle:
        handle.write(
            "# caso backend acc_num_cores tiempo_s speedup residuo checksum diff_checksum\n"
        )
        for summary in summaries:
            handle.write(
                f"{summary.case.name} {summary.case.backend} "
                f"{summary.case.acc_num_cores} {summary.time_s:.9f} "
                f"{summary.speedup:.9f} {summary.residuo:.16e} "
                f"{summary.checksum:.16e} {summary.checksum_diff:.16e}\n"
            )

    with raw_path.open("w", encoding="utf-8") as handle:
        handle.write("# caso corrida tiempo_s residuo checksum\n")
        for summary in summaries:
            for idx, (elapsed, residuo, checksum) in enumerate(summary.samples, start=1):
                handle.write(
                    f"{summary.case.name} {idx} {elapsed:.9f} "
                    f"{residuo:.16e} {checksum:.16e}\n"
                )

    return summary_path, raw_path


def save_plot(summaries: list[RunSummary], output_prefix: str) -> Path:
    mpl_config_dir = Path("/tmp/matplotlib-funciones-github")
    mpl_config_dir.mkdir(parents=True, exist_ok=True)
    os.environ.setdefault("MPLCONFIGDIR", str(mpl_config_dir))

    try:
        import matplotlib.pyplot as plt
    except ImportError as exc:
        raise SystemExit("No se pudo importar matplotlib.") from exc

    labels = [summary.case.name.replace("acc_", "acc\n") for summary in summaries]
    speedups = [summary.speedup for summary in summaries]
    times = [summary.time_s for summary in summaries]

    FIGURES_DIR.mkdir(parents=True, exist_ok=True)
    fig, axes = plt.subplots(1, 2, figsize=(10, 4.5))

    axes[0].bar(labels, times, color="tab:blue")
    axes[0].set_ylabel("Tiempo [s]")
    axes[0].set_title("Tiempo medio")
    axes[0].grid(axis="y", alpha=0.25)

    axes[1].bar(labels, speedups, color="black")
    axes[1].set_ylabel("Speedup vs serial")
    axes[1].set_title("Aceleracion")
    axes[1].grid(axis="y", alpha=0.25)

    for axis in axes:
        axis.tick_params(axis="x", labelrotation=30)

    fig.suptitle("Thomas OpenACC en conduccion 2D")
    fig.tight_layout()
    plot_path = FIGURES_DIR / f"{output_prefix}.png"
    fig.savefig(plot_path, dpi=200)
    plt.close(fig)
    return plot_path


def main() -> None:
    args = parse_args()
    ensure_args(args)

    code_dir = Path(args.code_dir).resolve()
    if not code_dir.is_dir():
        raise SystemExit(f"No existe el directorio: {code_dir}")

    missing_sources = [source for source in args.sources if not (code_dir / source).is_file()]
    if missing_sources:
        raise SystemExit(
            f"No se encontraron fuentes en {code_dir}: {', '.join(missing_sources)}"
        )

    BIN_DIR.mkdir(parents=True, exist_ok=True)

    print("Compilando objetivos...")
    target_paths: dict[str, Path] = {}
    for target in build_targets(args):
        target_paths[target.name] = compile_target(
            args.compiler,
            code_dir,
            args.sources,
            target,
            args.output_prefix,
        )
        print(f"  {target.name}: {target_paths[target.name]}")

    run_cases = make_run_cases(target_paths, args.cores)
    summaries: list[RunSummary] = []
    baseline_time: float | None = None
    reference_checksum: float | None = None

    for case in run_cases:
        print(f"Ejecutando {case.name}...")
        time_s, residuo, checksum, samples = benchmark_case(
            code_dir,
            case,
            args.runs,
            args.warmups,
            args.stat,
        )
        if case.name == "serial":
            baseline_time = time_s
            reference_checksum = checksum

        if baseline_time is None or reference_checksum is None:
            raise SystemExit("El caso serial debe ejecutarse primero.")

        speedup = baseline_time/time_s
        checksum_diff = abs(checksum - reference_checksum)
        summaries.append(
            RunSummary(
                case=case,
                time_s=time_s,
                speedup=speedup,
                residuo=residuo,
                checksum=checksum,
                checksum_diff=checksum_diff,
                samples=samples,
            )
        )
        print(
            f"{args.stat} {case.name}: {time_s:.6f} s, "
            f"speedup={speedup:.6f}, diff_checksum={checksum_diff:.6e}"
        )

    validate_checksums(
        summaries,
        reference_checksum if reference_checksum is not None else 0.0,
        args.checksum_rtol,
        args.checksum_atol,
    )

    summary_path, raw_path = save_tables(summaries, args.output_prefix)
    print(f"Tabla resumen: {summary_path}")
    print(f"Muestras crudas: {raw_path}")

    if not args.skip_plot:
        plot_path = save_plot(summaries, args.output_prefix)
        print(f"Grafica: {plot_path}")


if __name__ == "__main__":
    main()
