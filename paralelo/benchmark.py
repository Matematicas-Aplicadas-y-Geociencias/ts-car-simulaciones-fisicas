"""
benchmark.py — Mide el tiempo de ejecución de calor2D (128×128) con OpenMP
usando OMP_NUM_THREADS = 1 (serial), 2, 3, ..., 20 hilos.

Salida: benchmark_results.csv  con columnas  threads, time_s
"""

import subprocess
import os
import csv
import re

# ---------------------------------------------------------------------------
# Configuración
# ---------------------------------------------------------------------------
NX = 128
NY = 128
THREADS_LIST = list(range(1, 21))     # 1 = serial, 2..20 = paralelo
OUTPUT_CSV   = "benchmark_results.csv"
BINARY       = "./calor2D_bench"
MOD_SRC      = "mod_utiles.f90"
MOD_TMP      = "mod_utiles_bench.f90"
WORK_DIR     = os.path.dirname(os.path.abspath(__file__))


# ---------------------------------------------------------------------------
# 1. Crear mod_utiles_bench.f90 con nx=128, ny=128
# ---------------------------------------------------------------------------
def create_module_128():
    src_path = os.path.join(WORK_DIR, MOD_SRC)
    dst_path = os.path.join(WORK_DIR, MOD_TMP)

    with open(src_path) as f:
        content = f.read()

    # Reemplaza sólo los valores numéricos de nx y ny en la línea de parámetros
    content = re.sub(r'\bnx\s*=\s*\d+', f'nx = {NX}', content)
    content = re.sub(r'\bny\s*=\s*\d+', f'ny = {NY}', content)

    with open(dst_path, "w") as f:
        f.write(content)

    print(f"Módulo 128×128 creado: {MOD_TMP}")


# ---------------------------------------------------------------------------
# 2. Compilar
# ---------------------------------------------------------------------------
def compile_case():
    cmd = [
        "gfortran", "-O2", "-fopenmp",
        MOD_TMP, "tridiagonal.f90", "calor2D.f90",
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
# 3. Ejecutar un caso y devolver el tiempo transcurrido (segundos)
#    Usa /usr/bin/time -f "%e" para medir el tiempo de reloj
# ---------------------------------------------------------------------------
def run_case(n_threads: int) -> float:
    env = os.environ.copy()
    env["OMP_NUM_THREADS"] = str(n_threads)

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
    return elapsed


# ---------------------------------------------------------------------------
# main
# ---------------------------------------------------------------------------
def main():
    print(f"Benchmark calor2D {NX}×{NY}  —  hilos: {THREADS_LIST[0]}..{THREADS_LIST[-1]}")
    print("=" * 55)

    create_module_128()
    compile_case()

    results = []

    for n in THREADS_LIST:
        label = "serial (1 hilo)" if n == 1 else f"{n:2d} hilos"
        print(f"  OMP_NUM_THREADS={n:2d}  ({label})  ...", end=" ", flush=True)
        t = run_case(n)
        print(f"{t:8.3f} s")
        results.append({"threads": n, "time_s": t})

    # -----------------------------------------------------------------------
    # 4. Guardar resultados en CSV
    # -----------------------------------------------------------------------
    out_path = os.path.join(WORK_DIR, OUTPUT_CSV)
    with open(out_path, "w", newline="") as f:
        writer = csv.DictWriter(f, fieldnames=["threads", "time_s"])
        writer.writeheader()
        writer.writerows(results)

    print(f"\nResultados guardados en: {OUTPUT_CSV}")

    # Resumen en pantalla
    t_s = results[0]["time_s"]   # tiempo serial (1 hilo)
    print(f"\n{'Hilos':>7}  {'Tiempo (s)':>11}  {'Speedup':>9}  {'Eficiencia':>11}")
    print("-" * 46)
    for r in results:
        sp = t_s / r["time_s"]
        ef = sp / r["threads"] * 100
        print(f"{r['threads']:>7}  {r['time_s']:>11.3f}  {sp:>9.3f}  {ef:>10.1f}%")


if __name__ == "__main__":
    main()
