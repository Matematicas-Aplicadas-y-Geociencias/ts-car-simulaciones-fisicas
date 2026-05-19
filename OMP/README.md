# OMP

## `paralelo/calor2D/`

Fuentes OpenMP del solver 2D (`calor2D_parallel.f90`, `calor2D_utils.f90`, `residuo.f90`, etc.).

## `paralelo/openmp_basics/`

Ejemplos introductorios de OpenMP.

## `scripts/`

- `benchmark_aceleracion.py` — compila en `build/`, escribe en `PROYECTO/`
- `plot_comparacion_speedup.py`

```bash
python3 OMP/scripts/benchmark_aceleracion.py --max-threads 8 --runs 10
```
