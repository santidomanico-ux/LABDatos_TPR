# LABDatos_TPR

Examen domiciliario de R — Laboratorio de Datos, Universidad Torcuato Di Tella (2026).

**Integrantes:** Kevin Burghardt, Santino Domanico, Sebastián Roballos

## Estructura

```
LABDatos_TPR/
├── README.md
├── informe.pdf
├── analisis_datos/
│   ├── script_principal.R
│   ├── funciones.R
│   └── data/
├── analisis_econometrico/
│   ├── script_principal.R
│   └── funciones.R
│   └── data/
└── simulacion/
    ├── script_principal.R
    └── funciones.R
```

Cada ejercicio tiene su propio `script_principal.R`, que carga sus funciones desde `funciones.R` con `source()`.

## Cómo correrlo

1. Clonar o descargar el repositorio.
2. Abrir R o RStudio con el directorio de trabajo en la carpeta del repositorio, por ejemplo con `setwd("ruta/a/LABDatos_TPR")`. Todas las rutas son relativas a esa carpeta.
3. Instalar `tidyverse` si no está instalado: `install.packages("tidyverse")`.
4. Correr completo el `script_principal.R` de cada ejercicio (en RStudio, botón **Source**).

## Ejercicios

### Análisis de datos: Oil & Gas no convencional en Argentina

**Datos:** "Producción de pozos de gas y petróleo no convencional" (Secretaría de Energía, datos.gob.ar), período 2014–2025. El CSV original pesa 139 MB y GitHub no acepta archivos de más de 100 MB, así que está subido completo pero comprimido (`.csv.gz`, 14 MB); `read_csv()` lo lee directamente.

**Preguntas:**
1. ¿Quiénes son los grandes players y cómo se reparten el mercado?
2. ¿Cómo creció la producción de petróleo y la de gas?
3. ¿Se extrae más de roca shale o de roca tight, y cómo evolucionó?

### Análisis econométrico: gapminder

Regresiones sobre ingreso por persona y esperanza de vida con el dataset gapminder (`data/gapminder.csv`): modelos polinómicos de ingreso para Argentina con testeo en los últimos 10 años, correlaciones entre países sudamericanos, y regresiones simples y múltiples de esperanza de vida (2010), con contrastes de hipótesis y selección de un modelo de hasta tres covariables.
Este script se corre con el directorio de trabajo en `analisis_econometrico/`, porque lee `data/gapminder.csv` y carga `funciones.R` con rutas relativas.

### Simulación: demanda con preferencias Cobb-Douglas

Simulación de 10.000 hogares con ingresos chi-cuadrado: demandas óptimas, shock de precios y heterogeneidad en preferencias.
