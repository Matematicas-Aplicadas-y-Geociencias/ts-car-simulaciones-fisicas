# ACC Workspace

This tree groups the sequential baseline and ACC/GPU-oriented sources for the 2D heat solver.

## Layout

- `secuencial/calor2D/`
  - `calor2D.f90`
  - `tridiagonal.f90`
- `paralelo/calor2D/`
  - `calor2D.f90`
  - `calor2D_por_fila.f90`
  - `mod_utiles.f90`
  - `tridiagonal.f90`
- `ejemplos/`
  - small ACC examples (currently `ejemploArreglo.f90`)
- `proyecto/`
  - report copy for ACC planning:
    - `report.tex`
    - `report.pdf`

## Notes

- This workspace includes sequential base + ACC/GPU variants and a local report copy for future ACC-specific evolution.
