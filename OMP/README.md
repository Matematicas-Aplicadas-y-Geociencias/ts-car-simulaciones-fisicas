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
- `proyecto/`
  - Consolidated legacy project tree moved from repository root.
  - Includes: `docs/`, `paralelo/`, `secuencial/`, `scripts/`, `resources/`, `misc/`.
  - Main report currently used: `proyecto/docs/report.tex`.

## Notes

- Root-level cleanup moved former top-level directories into `proyecto/` to keep repository root minimal.
