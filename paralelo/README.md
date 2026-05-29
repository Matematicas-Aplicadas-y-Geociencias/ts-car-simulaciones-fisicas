# Paralelo — Cómputo de alto rendimiento (OpenMP)

Material de la parte **paralela** del curso de HPC: ejercicios introductorios de
OpenMP, un solver de la ecuación de calor 2D paralelizado, y un benchmark que
mide el *speedup* y la *eficiencia* del solver en función del número de hilos.

## Estructura

```
paralelo/
├── README.md
├── ejercicios_openmp/          # Ejercicios introductorios de OpenMP
│   ├── holaMundo.f90
│   ├── suma_regiones_paralelas.f90
│   ├── suma_bucle_paralelo.f90
│   └── prod-matriz-vector.f90
├── solucionador_calor2D/       # Solver de la ecuación de calor 2D + benchmark
│   ├── calor2D.f90             # Versión secuencial autocontenida
│   ├── calor2D_paralelo.f90    # Solver paralelo (OpenMP) — el que mide el benchmark
│   ├── mod_utiles.f90          # Módulo: parámetros de malla + residuo + salida VTK
│   ├── tridiagonal.f90         # Solver tridiagonal (Thomas)
│   ├── ejecutar_benchmark.py   # Corre el benchmark y escribe el CSV
│   └── graficar_benchmark.py   # Grafica tiempo / speedup / eficiencia
├── resultados/                 # Salidas CSV del benchmark
│   ├── benchmark_resultados.csv
│   └── benchmark_resultados_version_inicial.csv
└── graficas/                   # Figuras PNG del benchmark
```

## Cómo ejecutar

### Solver de calor 2D (OpenMP)
```bash
cd solucionador_calor2D
gfortran -O2 -fopenmp mod_utiles.f90 tridiagonal.f90 calor2D_paralelo.f90 -o calor2D_paralelo
OMP_NUM_THREADS=4 ./calor2D_paralelo
```

### Benchmark completo (tiempo + gráficas)
```bash
cd solucionador_calor2D
python ejecutar_benchmark.py     # -> ../resultados/benchmark_resultados.csv
python graficar_benchmark.py     # -> ../graficas/benchmark_<NX>x<NY>_<NUM_RUNS>_ejecuciones.png
```

### Ejercicios de OpenMP
```bash
cd ejercicios_openmp
gfortran -fopenmp holaMundo.f90 -o holaMundo && OMP_NUM_THREADS=4 ./holaMundo
```

## Resumen de cada archivo

### `ejercicios_openmp/`
- **holaMundo.f90** — "Hola Mundo" de OpenMP: abre una región paralela y cada hilo
  imprime su `id`; usa `omp_get_num_threads` / `omp_get_thread_num`.
- **suma_regiones_paralelas.f90** — Suma con cláusula `reduction(+:suma)` sobre los
  `id` de hilo dentro de una región paralela con `num_threads(65)`. Ilustra las
  cláusulas de una región paralela (`private`, `shared`, `reduction`, `num_threads`).
- **suma_bucle_paralelo.f90** — Suma de los primeros N=2048 números con `!$omp do`
  y `reduction(+:suma)`: reparto del bucle entre hilos.
- **prod-matriz-vector.f90** — Producto matriz–vector (2×2) paralelizado con
  `!$omp do` sobre las filas.

### `solucionador_calor2D/`
- **calor2D.f90** — Versión **secuencial autocontenida** del problema de calor 2D
  (malla fija 60×30, `do while` con criterio de paro por norma euclidiana < `1e-5`,
  imprime la malla por pantalla). No tiene directivas OpenMP.
- **calor2D_paralelo.f90** — Solver **paralelo (OpenMP)** de la ecuación de calor 2D
  por el método ADI (barridos alternados en x e y). Paraleliza con `!$omp parallel do`
  la copia de `tt`, ambos barridos por bandas de líneas y el cálculo del residuo
  (`reduction(+:residuo)`); itera hasta que el residuo < `1e-4`. Usa el módulo
  `utiles` y es el que compila y mide `ejecutar_benchmark.py`.
- **mod_utiles.f90** — Módulo `utiles`: define los parámetros de malla
  (`nx`, `ny`, `itermax`), la subrutina `residuo_temp` (laplaciano del campo) y
  `postproceso_vtk` (escribe la temperatura y el residuo en formato VTK binario).
- **tridiagonal.f90** — Subrutina `tri`: resuelve un sistema tridiagonal `A x = r`
  por eliminación gaussiana + sustitución hacia atrás (algoritmo de Thomas).
- **ejecutar_benchmark.py** — Genera al vuelo `mod_utiles_bench.f90` (copia de
  `mod_utiles.f90` con la malla NX×NY del benchmark), compila el solver con
  `-O2 -fopenmp`, lo ejecuta `NUM_RUNS` veces por cada número de hilos midiendo el
  tiempo con `/usr/bin/time`, y guarda media/desviación/min/max en
  `../resultados/benchmark_resultados.csv`.
- **graficar_benchmark.py** — Lee el CSV de resultados y dibuja tres paneles
  (tiempo de ejecución, speedup real vs ideal, eficiencia paralela) con barras de
  error; guarda la figura en `../graficas/`.

### `resultados/`
- **benchmark_resultados.csv** — Resultados vigentes (malla 256×256, 5 ejecuciones,
  1–10 hilos): columnas `threads, time_mean, time_stddev, time_min, time_max`.
- **benchmark_resultados_version_inicial.csv** — Primer intento (formato antiguo,
  columnas `threads, time_s`, 1–20 hilos) con la anomalía de tiempo en 8 hilos.

### `graficas/`
Evolución del trabajo de paralelización (de más antigua a más reciente):
- **benchmark_128x128_intento_inicial_anomalia_8_hilos.png** — Primer intento;
  pico anómalo de tiempo (~58 s) en 8 hilos.
- **benchmark_128x128_segundo_intento_anomalia_8_hilos.png** — Segundo intento, aún
  con la anomalía en 8 hilos.
- **benchmark_128x128_paralelizando_copia_de_tt.png** — Tras paralelizar la copia
  de `tt`; desaparece la anomalía, speedup pico ≈ 3.3.
- **benchmark_128x128_paralelizando_tt_y_residuo.png** — Añadiendo la
  paralelización del residuo; speedup pico ≈ 3.8.
- **benchmark_128x128_paralelizando_tt_residuo_sin_postproceso.png** — Igual que el
  anterior pero comentando la llamada a postproceso; speedup pico ≈ 4.2.
- **benchmark_128x128_con_barras_de_error.png** — Corrida 128×128 con barras de
  error (media ± desviación y rango min–max).
- **benchmark_256x256_1_ejecucion_1a20_hilos.png** — Malla 256×256, 1 ejecución,
  1–20 hilos.
- **benchmark_256x256_5_ejecuciones_1a10_hilos.png** — Resultado final: 256×256,
  5 ejecuciones, 1–10 hilos (corresponde a `benchmark_resultados.csv`).

## Nota sobre `mod_utiles_bench.f90` (eliminado)

Se eliminó `mod_utiles_bench.f90` porque **no es código fuente, sino un artefacto
generado**: `ejecutar_benchmark.py` lo crea automáticamente en cada corrida como
copia de `mod_utiles.f90` cambiando únicamente los valores de `nx`/`ny` a la malla
del benchmark. Versionarlo era redundante y propenso a quedar desincronizado con
`mod_utiles.f90`. El script lo regenera solo, así que no hace falta conservarlo.
