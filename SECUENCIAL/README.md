# SECUENCIAL

Baseline numérico compartido. OMP y ACC paralelizan a partir de aquí.

Cada caso (`calor1D`, `calor2D`) usa la misma plantilla:

```
calorXD/
├── *.f90, script de gráfica
├── build/              ← ejecutable (no versionar)
└── resultados/
    ├── tablas/         ← .dat
    └── figuras/        ← .png
```

## calor1D

```bash
cd SECUENCIAL/calor1D
gfortran -O3 tridiagonal.f90 calor1D.f90 -o build/calor1D
./build/calor1D
python3 graph_calor1D.py
```

Salidas: `resultados/tablas/resultado_tabla.dat`, `resultados/figuras/temperatura_1d.png`.

## calor2D

```bash
cd SECUENCIAL/calor2D
gfortran -O3 tridiagonal.f90 calor2D.f90 -o build/calor2D
./build/calor2D
python3 nano_plot.py
```

Salidas: `resultados/tablas/resultado_malla.dat`, `resultados/figuras/calor2D_plate_blue_red.png`.
