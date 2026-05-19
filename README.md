# FUNCIONES_GITHUB

Simulación de calor/Laplace (1D/2D): secuencial, OpenMP y OpenACC.

## Árbol (5 carpetas)

```
FUNCIONES_GITHUB/
├── SECUENCIAL/     Código secuencial compartido (1D + 2D)
├── OMP/            Paralelización OpenMP + scripts de benchmark
├── ACC/            Paralelización OpenACC/GPU + scripts de benchmark
├── REFERENCIA/     Copia congelada de origin/main (no editar)
└── PROYECTO/       Reporte LaTeX, bibliografía, figuras y tablas
```

| Qué buscas | Dónde |
|------------|--------|
| Compilar calor 1D/2D secuencial | `SECUENCIAL/calor1D/`, `SECUENCIAL/calor2D/` |
| Código OpenMP activo | `OMP/paralelo/calor2D/`, `OMP/paralelo/openmp_basics/` |
| Código OpenACC activo | `ACC/paralelo/calor2D/` |
| Editar el reporte | `PROYECTO/report.tex` → PDF en `PROYECTO/report.pdf` |
| Comparar con el repo del curso | `REFERENCIA/` |
| Benchmark OpenMP | `python3 OMP/scripts/benchmark_aceleracion.py` |
| Benchmark OpenACC | `python3 ACC/scripts/benchmark_acc_thomas.py` |

## Reglas

1. **Un solo secuencial:** solo en `SECUENCIAL/`, no copiar en OMP ni ACC.
2. **REFERENCIA** se actualiza solo desde `main` (`REFERENCIA/README.md`).
3. **PROYECTO** concentra todo lo del PDF; los benchmarks escriben `tables/`, `figures/`, `logs/`.
4. Binarios locales en `OMP/build/` y `ACC/build/` (ignorados por git).

## LaTeX en Cursor

Auto-guardado + compilación al cambiar el `.tex`. Raíz: `PROYECTO/report.tex`.
