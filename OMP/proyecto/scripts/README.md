# Scripts de reproducibilidad

Esta carpeta contiene comandos de alto nivel para repetir experimentos sin depender de rutas internas.

## Limpieza de artefactos generados

```powershell
powershell -ExecutionPolicy Bypass -File "scripts/clean_artifacts.ps1"
```

Elimina binarios Fortran y salidas temporales de LaTeX sin tocar fuentes.

## Benchmark paralelo (calor2D)

```powershell
powershell -ExecutionPolicy Bypass -File "scripts/run_benchmark.ps1" -MaxThreads 8 -Runs 10
```

Este script envuelve a:

```powershell
python "paralelo/scripts/benchmark_aceleracion.py" --max-threads 8 --runs 10
```

Salidas esperadas:

- `docs/tables/*.dat`
- `docs/figures/*.png`

## Tasks de VS Code

En la paleta de comandos / Run Task:

- `repo: clean artifacts`
- `paralelo: benchmark`
