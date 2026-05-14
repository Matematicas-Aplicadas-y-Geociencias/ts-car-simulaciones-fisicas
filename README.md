# FUNCIONES_GITHUB

Repositorio de trabajo para ejemplos y experimentos de computo cientifico secuencial/paralelo, con foco actual en solucion de ecuaciones tipo calor/Laplace por diferencias finitas y OpenMP.

## Estructura principal

- `OMP/`: arquitectura enfocada a flujo secuencial + paralelizacion OpenMP.
   - `OMP/secuencial/calor2D/`: base secuencial para el caso 2D.
   - `OMP/paralelo/calor2D/`: solver y utilidades para la variante OMP.
   - `OMP/paralelo/openmp_basics/`: ejemplos basicos de OpenMP.
   - `OMP/scripts/`: scripts de benchmark y limpieza asociados al flujo OMP.
- `ACC/`: arquitectura enfocada a flujo secuencial + paralelizacion ACC/GPU.
   - `ACC/secuencial/calor2D/`: base secuencial para comparacion en ACC.
   - `ACC/paralelo/calor2D/`: fuentes de la variante ACC/GPU.
   - `ACC/ejemplos/`: ejemplos pequenos de ACC.
- `secuencial/`: implementaciones base (1D y 2D).
- `paralelo/src/calor2d/`: solver 2D paralelo y utilidades numericas.
- `paralelo/src/openmp_basics/`: ejemplos pequenos de OpenMP.
- `paralelo/reference/calor2d/`: version de referencia (comparacion/base).
- `paralelo/scripts/`: scripts de benchmark y automatizacion local.
- `paralelo/resultados/binarios/`: ejecutables/resultados binarios historicos.
- `paralelo/bin/`: binarios de compilacion local.
- `docs/`: documentacion formal (reporte tecnico, figuras y tablas).
- `scripts/`: envoltorios de reproducibilidad a nivel repositorio.

Nota: la reestructura OMP/ACC es no destructiva; se conservaron las rutas
originales para no perder resultados ni referencias del reporte en `docs/`.

## Ejecucion rapida (benchmark paralelo)

1. Requisitos:
   - `gfortran` con OpenMP
   - Python 3
   - `matplotlib` (para la grafica)

2. Correr:

```powershell
python "paralelo/scripts/benchmark_aceleracion.py" --max-threads 8 --runs 10
```

3. Salidas:
   - datos: `docs/tables/aceleracion_ideal_vs_real.dat`
   - figura: `docs/figures/aceleracion_ideal_vs_real.png`

## Documentacion formal

El documento tecnico principal esta en:

- `docs/report.tex`

La version `README.adoc` se conserva como referencia historica del curso.
