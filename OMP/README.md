# OMP Workspace

This tree groups the sequential and OpenMP-oriented sources for the 2D heat solver.

## Layout

- `secuencial/calor2D/`
  - `calor2D.f90`
  - `tridiagonal.f90`
- `paralelo/calor2D/`
  - `calor2D_parallel.f90`
  - `calor2D_secuencial.f90`
  - `calor2D_utils.f90`
  - `residuo.f90`
  - `tridiagonal.f90`
- `paralelo/openmp_basics/`
  - OpenMP learning examples (`*.f90`)
- `scripts/`
  - Benchmark and helper scripts used in OMP flow

## Notes

- This structure is intentionally non-destructive: original folders remain unchanged.
- Documentation artifacts in `docs/` are preserved and not relocated.
