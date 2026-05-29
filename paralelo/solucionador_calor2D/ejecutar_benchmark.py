"""
ejecutar_benchmark.py — Mide el tiempo de ejecución de calor2D (malla NX×NY)
con OpenMP usando OMP_NUM_THREADS = 1 (serial), 2, 3, ... hasta THREADS_LIST.

Recompila el solver para cada caso y lo ejecuta NUM_RUNS veces por nº de hilos.
Salida: ../resultados/benchmark_resultados.csv
        (columnas: threads, time_mean, time_stddev, time_min, time_max)
"""

import subprocess
import os
import csv
import re
import statistics
import time as time_module

# ---------------------------------------------------------------------------
# Configuración
# ---------------------------------------------------------------------------
NX = 256
NY = 256
THREADS_LIST = list(range(1, 11))     # 1 = serial, 2.. = paralelo
NUM_RUNS     = 5                       # repetir cada caso 5 veces
OUTPUT_CSV   = "benchmark_resultados.csv"
BINARY       = "./calor2D_bench"
MOD_SRC      = "mod_utiles.f90"
MOD_TMP      = "mod_utiles_bench.f90"      # módulo temporal que se genera al vuelo
WORK_DIR     = os.path.dirname(os.path.abspath(__file__))         # solucionador_calor2D/
RESULTS_DIR  = os.path.join(os.path.dirname(WORK_DIR), "resultados")


# ---------------------------------------------------------------------------
# 1. Generar mod_utiles_bench.f90 (copia de mod_utiles.f90 con nx=NX, ny=NY)
#    Es un archivo TEMPORAL de compilación: se regenera en cada ejecución a
#    partir de mod_utiles.f90, por eso no se versiona en git.
# ---------------------------------------------------------------------------
def create_module():
    src_path = os.path.join(WORK_DIR, MOD_SRC)
    dst_path = os.path.join(WORK_DIR, MOD_TMP)

    with open(src_path) as f:
        content = f.read()

    # Reemplaza sólo los valores numéricos de nx y ny en la línea de parámetros
    content = re.sub(r'\bnx\s*=\s*\d+', f'nx = {NX}', content)
    content = re.sub(r'\bny\s*=\s*\d+', f'ny = {NY}', content)

    with open(dst_path, "w") as f:
        f.write(content)

    print(f"Módulo {NX}×{NY} generado: {MOD_TMP}")


# ---------------------------------------------------------------------------
# 2. Compilar
# ---------------------------------------------------------------------------
def compile_case():
    cmd = [
        "gfortran", "-O2", "-fopenmp",
        MOD_TMP, "tridiagonal.f90", "calor2D_paralelo.f90",
        "-o", BINARY,
    ]
    print("Compilando:", " ".join(cmd))
    result = subprocess.run(cmd, cwd=WORK_DIR, capture_output=True, text=True)
    if result.returncode != 0:
        print("ERROR de compilación:")
        print(result.stderr)
        raise RuntimeError("Compilación fallida")
    print("Compilación exitosa.\n")


# ---------------------------------------------------------------------------
# 3. Ejecutar un caso NUM_RUNS veces y devolver estadísticas
#    Usa /usr/bin/time -f "%e" para medir el tiempo de reloj
# ---------------------------------------------------------------------------
def run_case(n_threads: int) -> dict:
    """Ejecuta NUM_RUNS veces y retorna dict con estadísticas."""
    env = os.environ.copy()
    env["OMP_NUM_THREADS"] = str(n_threads)

    times = []
    for run in range(NUM_RUNS):
        cmd = ["/usr/bin/time", "-f", "%e", BINARY]
        result = subprocess.run(
            cmd,
            cwd=WORK_DIR,
            env=env,
            capture_output=True,
            text=True,
        )

        # /usr/bin/time -f "%e" escribe el tiempo en stderr como último renglón
        stderr_lines = [line for line in result.stderr.splitlines() if line.strip()]
        elapsed = float(stderr_lines[-1].strip())
        times.append(elapsed)

    # Calcular estadísticas
    mean_time = statistics.mean(times)
    stddev = statistics.stdev(times) if len(times) > 1 else 0.0
    min_time = min(times)
    max_time = max(times)

    return {
        "mean": mean_time,
        "stddev": stddev,
        "min": min_time,
        "max": max_time,
    }


# ---------------------------------------------------------------------------
# main
# ---------------------------------------------------------------------------
def main():
    print(f"Benchmark calor2D {NX}×{NY}  —  hilos: {THREADS_LIST[0]}..{THREADS_LIST[-1]}")
    print("=" * 55)

    create_module()
    compile_case()

    results = []

    for n in THREADS_LIST:
        label = "serial (1 hilo)" if n == 1 else f"{n:2d} hilos"
        print(f"  OMP_NUM_THREADS={n:2d}  ({label})  ({NUM_RUNS} runs)", end=" ", flush=True)
        stats = run_case(n)
        print(f"  media: {stats['mean']:8.3f}s  ± {stats['stddev']:6.3f}s  [min: {stats['min']:7.3f}s, max: {stats['max']:7.3f}s]")
        results.append({
            "threads": n,
            "time_mean": stats["mean"],
            "time_stddev": stats["stddev"],
            "time_min": stats["min"],
            "time_max": stats["max"],
        })

    # -----------------------------------------------------------------------
    # 4. Guardar resultados en CSV
    # -----------------------------------------------------------------------
    os.makedirs(RESULTS_DIR, exist_ok=True)
    out_path = os.path.join(RESULTS_DIR, OUTPUT_CSV)
    with open(out_path, "w", newline="") as f:
        writer = csv.DictWriter(f, fieldnames=["threads", "time_mean", "time_stddev", "time_min", "time_max"])
        writer.writeheader()
        writer.writerows(results)

    print(f"\nResultados guardados en: {out_path}")

    # Resumen en pantalla
    t_s = results[0]["time_mean"]   # tiempo serial promedio (1 hilo)
    print(f"\n{'Hilos':>7}  {'Tiempo (s)':>13}  {'Desvío':>9}  {'Speedup':>9}  {'Eficiencia':>11}")
    print("-" * 62)
    for r in results:
        sp = t_s / r["time_mean"]
        ef = sp / r["threads"] * 100
        print(f"{r['threads']:>7}  {r['time_mean']:>13.3f}  {r['time_stddev']:>9.3f}  {sp:>9.3f}  {ef:>10.1f}%")


if __name__ == "__main__":
    main()
