# REFERENCIA

Contenido de `origin/main` extraído con `git archive` (**sin** conversión CRLF).
No editar: línea base para comparar con `OMP/` y `ACC/`.

Actualizar desde `main`:

```bash
git fetch origin main
rm -rf REFERENCIA/*
git -c core.autocrlf=false archive origin/main | tar -x -C REFERENCIA
```

Este `README.md` es el único archivo añadido localmente; el resto coincide con `main`.
