# OMP

Solo dos carpetas de código:

| Carpeta | Contenido |
|---------|-----------|
| **`calor2D/`** | Solver 2D OpenMP, scripts, `build/`, `tables/`, `figures/`, `resultados/` |
| **`openmp_basics/`** | Ejemplos introductorios (`hello_openmp`, producto punto, etc.) |

Todo lo del caso 2D (fuentes, benchmark, comparación con secuencial) está en **`OMP/calor2D/`** — ver `calor2D/README.md`.

```bash
python3 OMP/calor2D/scripts/benchmark_aceleracion.py --max-threads 8 --runs 10
```
