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

## Notes

- This structure is intentionally non-destructive: original folders remain unchanged.
- Documentation artifacts in `docs/` are preserved and not relocated.
