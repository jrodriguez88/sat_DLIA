# sat_DLIA — Sistema de Alerta Temprana de Incendios (CAR Cundinamarca)

Sistema integral para **detección, monitoreo, análisis y pronóstico** de incendios de cobertura vegetal en la **cuenca del río Bogotá** y jurisdicción de la **CAR Cundinamarca**, integrando fuentes institucionales, sensores remotos y modelación predictiva.

> **Entrada clave:** `runMain.R` (script maestro). Ejecuta el flujo completo **desde ingesta y curaduría (DB0)** hasta **modelación con Random Forest** y generación de **mapas mensuales de probabilidad**.

---

## Objetivo

1. **Curar e integrar** múltiples fuentes geoespaciales y tabulares (institucionales + satelitales).
2. Construir un **dataset de entrenamiento** a partir de eventos de ocurrencia (y pseudo-ocurrencias) con extracción de predictores.
3. Entrenar y evaluar un **modelo supervisado (Random Forest)** para estimar **probabilidad de ocurrencia** y producir salidas cartográficas (rasters y mapas).

---

## Estructura del repositorio

* `runMain.R` — **orquestador** del workflow.
* `requirements.R` — instalación/carga de dependencias.
* `src/` — scripts funcionales (ingesta, preprocesamiento, extracción, FE, modelos, mapeo).
* `aux_files/` — archivos auxiliares (selección de capas, catálogos, etc.).
* `data/` — *workspace local* de datos (no se recomienda versionar datos pesados):

  * `data/raw/` — insumos originales.
  * `data/interm/` — productos intermedios y capas preparadas.
  * `data/final/` — dataset curado, predictores finales, salidas del modelo.
* `docs/` — documentación técnica / figuras.
* `notebooks/` — bitácoras, Rmd y análisis exploratorio.


---

## Quickstart (ejecución mínima)

1. Clonar el repositorio y abrir el proyecto `sat_DLIA.Rproj`.
2. Ejecutar `runMain.R` (o por bloques) desde RStudio.

> ⚠️ Varias fuentes **no se distribuyen** en el repositorio (licenciamiento, tamaño o acceso institucional). Revisa la sección **Fuentes de información** y los flags de descarga en `runMain.R`.

---

## Flujo de datos y análisis (según `runMain.R`)

### 0) Preparación

* Carga dependencias: `source("requirements.R")`
* Crea estructura: `data/raw/`, `data/interm/`, `data/final/`
* Carga funciones internas: `source("src/funciones_sat_car.R")`

### 1) Ingesta e integración (DB0)

1. **GeoServicios CAR (DRN):** exploración + descarga de capas seleccionadas.

   * `src/visor_car.R`
   * `aux_files/geoambiental_car_select.csv` (selector de capas)
   * `src/ingest_layer_geocar_all.R`
2. **Capas base territoriales:** límites/jurisdicción/entidades administrativas.

   * `src/ingest_capas_base.R`
3. **Infraestructura:** p.ej. líneas de transmisión eléctrica (UPME).

   * `src/ingest_layer_infraestructura.R`
4. **Eventos de incendios (ocurrencia):** integración de registros DGOAT-CAR/UNGRD + insumos satelitales (MCD64A1).

   * `src/ingest_layer_eventos.R`
5. **Combustibles:** Fuelbed (Pettinari 2015) y parámetros FCCS.

   * `src/ingest_layer_combustible.R`
6. **Coberturas de la tierra:** CLC-IDEAM / Ecosistemas IDEAM (según disponibilidad).

   * `src/ingest_layer_coberturas.R`
7. **Terreno:** DEM y derivados (pendiente, orientación, etc.) a partir de NASADEM.

   * `src/ingest_layer_terreno.R`
8. **Vegetación (NDVI):** MODIS MOD13A1 (preprocesado/recortado con GEE).

   * `src/ingest_layer_ndvi.R`
9. **Meteorología:** precipitación, Tmax, Tmin, HR (insumos IDEAM; cubos/climatología según fuente).

   * `src/ingest_layer_meteo.R`
10. **Socioeconómico (municipal):** indicadores (DANE u otras fuentes oficiales).

    * `src/ingest_layer_socioeconomica.R`

### 2) Variables derivadas

* **Rasters de distancia** a infraestructura/elementos relevantes (feature engineering espacial):

  * `src/calcula_raster_distancias.R`

### 3) Extracción de predictores y construcción del dataset

1. Carga/estandariza eventos (ej. `data/final/INCENDIOS_REVISED_CAR.xlsx`).
2. **Extracción por punto** (valor del píxel o vecindad) de todos los predictores:

   * `src/funcion_extraccion_data_DB0.R`
   * `src/creacion_datos_entrenamiento_modelos.R`
3. Control de calidad: missingness (naniar) y validación rápida.
4. Exporta dataset curado:

   * `data/final/test_data.csv`

### 4) Análisis multivariado y clustering (exploratorio)

* EDA + Feature Engineering base: `src/EDA_FE_test_data.R`
* PCA + comparación de clustering (K-means / HClust / DBSCAN):

  * `src/Analisis_Multivariado_PCA_Cluster.R`
* (Opcional) Feature engineering extendido:

  * `src/Feature_Engineering_CAR.R`

### 5) Pseudo-ocurrencias (presencia/ausencia)

* Generación de pseudo-ocurrencias para balancear/entrenar modelos supervisados:

  * `src/generacion_pseudoocurrencias.R`
* Dataset final para modelación:

  * `data/final/test_data_modelos.csv`

### 6) Modelación Random Forest

1. Preparación de variables y particiones: `src/data_to_modeling_RF.R`
2. Entrenamiento + testing: `src/Prototipo_RandomForest_CAR.R`

   * métricas (ej. *F1 / f_meas*), matriz de confusión y diagnóstico
   * importancia de variables (VIP)

### 7) Mapas de probabilidad de ocurrencia

* Ensamble de predictores (estáticos + variables mensuales) y predicción espacial:

  * `src/mapas_prob_incendios_RandomForest.R`
* Salidas típicas:

  * `data/final/*_evt.tif` (capas mensuales)
  * `prob_stack` (probabilidad continua) y `prob_class` (clases)

---

## Fuentes de información

> Nota: las fuentes institucionales pueden requerir permisos internos, endpoints específicos o descargas manuales.

| Componente                | Fuente                                         |          Tipo | Uso en el modelo                                   |
| ------------------------- | ---------------------------------------------- | ------------: | -------------------------------------------------- |
| Capas base / jurisdicción | CAR (GeoServicios DRN)                         |        Vector | enmascarar, recortar, unidades administrativas     |
| Infraestructura           | UPME (Geoportal/FeatureServer)                 |        Vector | distancias a infraestructura                       |
| Ocurrencia incendios      | DGOAT-CAR / UNGRD                              |  Vector/tabla | variable respuesta / presencia                     |
| Área quemada              | MODIS MCD64A1 (V6.1)                           |        Raster | apoyo a ocurrencia y validación                    |
| Combustibles              | Pettinari (2015) Global Fuelbed Dataset (FCCS) |  Raster/tabla | potencial de combustión y variables de combustible |
| Coberturas                | CLC-IDEAM / Ecosistemas IDEAM                  | Vector/Raster | tipo de cobertura / contexto ecológico             |
| Terreno                   | NASADEM                                        |        Raster | elevación y derivados topográficos                 |
| Vegetación                | MODIS MOD13A1 (NDVI)                           |        Raster | condición de vegetación                            |
| Meteorología              | IDEAM (precip., Tmax, Tmin, HR)                |   Raster/cubo | forzantes climáticos                               |
| Socioeconómico            | DANE y/o fuentes oficiales                     |  Tabla/Vector | presión antrópica / contexto municipal             |

---

## Métodos implementados (resumen)

* **ETL geoespacial:** descarga/lectura, estandarización CRS, recortes, rasterización, resampling y enmascaramiento.
* **Variables derivadas:** distancia a elementos (feature engineering espacial).
* **Curaduría del dataset:** extracción de predictores por evento, control de NA y validación.
* **Exploratorio multivariado:** PCA + clustering para perfilar contextos/gradientes.
* **Aprendizaje supervisado:** Random Forest (clasificación presencia/ausencia con pseudo-ocurrencias).
* **Mapeo predictivo:** generación de rasters mensuales de probabilidad y clasificación por rangos.

---

## Próximos pasos 

* Validación robusta: validación espacial/temporal, calibración y análisis de incertidumbre.
* Modelos alternativos: MaxEnt, GBM/XGBoost, modelos espacio-temporales.

---

## Cómo citar

Si usas este repositorio como referencia técnica:

> Rodriguez‑Espinoza, J. (2025). *sat_DLIA: Sistema Integral para la Detección, Monitoreo, Análisis y Pronóstico de Incendios en la Cobertura Vegetal (CAR Cundinamarca)*. GitHub repository.

---

## Licencia

MIT — ver `LICENSE`.
