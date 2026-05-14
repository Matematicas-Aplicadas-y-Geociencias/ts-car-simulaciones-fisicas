# FUNCIONES_GITHUB

Repositorio de trabajo para simulaciones fisicas de calor/Laplace, reorganizado
en dos espacios principales:

- `OMP/`: flujo secuencial + paralelizacion OpenMP.
- `ACC/`: flujo secuencial + paralelizacion ACC/GPU.

## Ubicaciones clave

- Reporte tecnico principal (version actual):
  - `OMP/proyecto/docs/report.tex`
  - `OMP/proyecto/docs/report.pdf`
- Copia de referencia del reporte para ACC:
  - `ACC/proyecto/report.tex`
  - `ACC/proyecto/report.pdf`

## Nota de reestructura

La limpieza de arquitectura se realizo para que la raiz quede enfocada en
`OMP/` y `ACC/`, conservando contenido historico dentro de esas carpetas para
evitar perdida de resultados, figuras o trazabilidad del trabajo.

La version `README.adoc` se mantiene como referencia historica.
