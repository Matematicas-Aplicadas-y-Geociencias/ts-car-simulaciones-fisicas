# Organizacion de `paralelo/`

## Arbol recomendado

- `src/calor2d/`: c\'odigo fuente del caso 2D (secuencial/paralelo y utilidades).
- `src/openmp_basics/`: ejemplos simples de OpenMP.
- `reference/calor2d/`: copia exacta del codigo en la rama `referencia` (solo fuentes).
- `scripts/`: scripts para compilacion/benchmark.
- `bin/`: ejecutables y modulos compilados localmente.
- `resultados/binarios/`: ejecutables hist\'oricos y artefactos no-fuente.

## Nota

No editar manualmente `reference/calor2d/`; cuando se requiera actualizar, volver a extraer desde la rama `referencia`.
