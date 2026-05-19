# ACC

## `paralelo/calor2D/`

Fuentes OpenACC/GPU (`calor2D_acc.f90`, `calor2D_acc_utils.f90`, etc.).

## `scripts/`

`benchmark_acc_thomas.py` — compila en `build/`, escribe en `PROYECTO/`.

```bash
python3 ACC/scripts/benchmark_acc_thomas.py --nx 60 --ny 30 --itermax 20 --runs 1 --cores 1 2
```

## `ejemplos/`, `legacy/`

- `ejemplos/` — ejemplos pequeños (`ejemploArreglo.f90`)
- `legacy/acc_legacy/` — respaldo histórico (no borrar; distinto de `ejemplos/`)

Reporte: `PROYECTO/report.tex`.
