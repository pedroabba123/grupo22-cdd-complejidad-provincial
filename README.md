# Especialización y diversificación productiva de las provincias argentinas

**Un primer paso hacia la complejidad económica provincial**

Trabajo práctico de *Ciencia de Datos para Economía y Negocios* (FCE-UBA, 2.º cuatrimestre de 2026). Grupo 22.

**Integrantes:** Tomás Izbicki, Pedro Abbiati y Juan Martinez.

---

## Objetivo e hipótesis

Queremos ver en qué se diferencian las provincias argentinas por cómo reparten su empleo entre actividades y si eso tiene que ver con cuánto exportan.

- **Hipótesis 1.** Las provincias se diferencian por su estructura productiva y no solo por su tamaño: algunas concentran el empleo en pocas ramas y otras lo reparten en muchas.
- **Hipótesis 2.** Las provincias con el empleo más diversificado tienen más empresas exportadoras por establecimiento. La hipótesis nula es que no hay asociación entre la concentración del empleo y la proporción de exportadoras (ρ = 0).

El título dice "un primer paso" porque la base mide empleo y establecimientos, no exportaciones en dólares, así que no alcanza para calcular un índice de complejidad completo.

## Base de datos

- **Fuente:** *Distribución geográfica de los establecimientos productivos*, CEP XXI, Ministerio de Economía. <https://datos.produccion.gob.ar/dataset/distribucion-geografica-de-los-establecimientos-productivos>
- **Unidad de observación:** departamento × actividad (CLAE de 6 dígitos) × año.
- **Tamaño:** 195.370 filas y 11 variables. Cubre 24 provincias, 527 departamentos y 945 actividades.
- **Período:** 2021 y 2022. El análisis usa 2022 y deja 2021 como control.
- **Qué mide:** empleo privado registrado, establecimientos y empresas exportadoras de cada celda. No incluye empleo público ni informal.
- **Base auxiliar:** `auxiliar/provincias_aux.csv`, con la región de cada provincia (INDEC) y el índice de complejidad económica por empleo de Fundar (Luvini, 2024, Tabla 1 del anexo), que se usa solo como contraste.

## Qué hicimos

1. **Descriptivas y calidad de datos.** Variables, tipo y faltantes. Distribución de las variables principales. Diagnóstico de faltantes y outliers, decisión de mantener los outliers (son grandes empleadores reales) y comparación de las estadísticas antes y después de limpiar.
2. **Índices de especialización.** Cocientes de localización por provincia y sector, índices de concentración (HHI, Theil, ramas equivalentes) a tres niveles de detalle, y diversidad y ubicuidad contrastadas con el ECI de Fundar.
3. **Cluster de provincias.** HCPC (PCA, Ward y k-means) sobre la estructura sectorial del empleo, con elección de k, sensibilidad, test contra datos sin grupos y validación externa.
4. **Inferencia.** Correlación de Spearman con prueba de permutación entre concentración del empleo y exportadoras, y regresión con errores robustos. Revisión de supuestos, sensibilidad y corrección de Holm.
5. **Chequeo a nivel departamental.** El mismo test con departamentos de al menos 500 empleos, con errores agrupados por provincia y con efectos fijos de provincia.
6. **Visualizaciones.** Dos gráficos finales (uno exploratorio y otro comunicativo) y los gráficos de cada etapa.

## Estructura del repositorio

```
grupo22_cdd_tp/
├── raw/        Base original, tal como se descargó (no se modifica)
├── auxiliar/   Región de cada provincia y ECI de Fundar
├── input/      Base procesada, lista para el análisis
├── output/
│   ├── tablas/     Tablas de resultados
│   └── graficos/   Gráficos (PNG y los dos JPG finales)
├── script/     Scripts de R, numerados en el orden de ejecución
├── utils/      Funciones propias, una por archivo
├── grupo22_cdd_tp.Rproj
└── README.md
```

Los nombres de las tablas y gráficos de `output/` empiezan con un prefijo: `i2_` para el análisis principal (índices, cluster e inferencia), `desc_` para las descriptivas, `post_` para las descriptivas después de limpiar y `dep_` para el chequeo departamental. Los dos gráficos finales son `grupo_22_exploratorio.jpg` y `grupo_22_comunicativo.jpg`.

## Cómo reproducir el análisis

**1. Paquetes.** Usamos R 4.6.1. Hay que tener instalados estos paquetes:

```r
install.packages(c("tidyverse", "skimr", "janitor", "scales", "ggrepel", "ggtext", "patchwork",
                   "FactoMineR", "factoextra", "cluster", "mclust", "MASS",
                   "sandwich", "lmtest", "car"))
```

**2. Carpeta de trabajo.** Abrir `grupo22_cdd_tp.Rproj` en RStudio. Así todas las rutas son relativas a la raíz del proyecto.

**3. Correr los scripts en orden.** Se pueden correr todos juntos con `script/00_correr_todo.R` (desde una terminal: `Rscript script/00_correr_todo.R`) o de a uno. Cada script lee los archivos que dejó el anterior, no depende de objetos en memoria. En total tardan alrededor de un minuto.

| Script | Qué hace | Lee | Guarda |
| --- | --- | --- | --- |
| `01_carga_y_exploracion_inicial.R` | Carga la base, la limpia de nombres y hace la exploración inicial | `raw/` | `input/establecimientos.csv`, tablas y gráficos de la exploración inicial |
| `02_descriptivas_variables_principales.R` | Variables usadas, tipo, faltantes y estadísticas descriptivas a nivel celda y provincia | `input/`, `auxiliar/` | `desc_*.csv`, `desc_fig_*.png` |
| `03_faltantes_y_outliers.R` | Diagnóstico de faltantes y outliers, decisión y descriptivas después de limpiar | `input/`, `auxiliar/` | `i2_*` de calidad de datos, `post_*.csv`, `post_fig_*.png` |
| `04_indices_lq_hhi_theil.R` | Cocientes de localización, HHI, Theil, diversidad y ubicuidad, y tabla maestra por provincia | `input/`, `auxiliar/` | `i2_tabla_maestra_provincias.csv` y tablas y gráficos de los índices |
| `05_cluster_provincias_hcpc.R` | Cluster de provincias, sensibilidad, test contra datos sin grupos y validación externa | `input/`, `auxiliar/`, tabla maestra | `i2_cluster_*.csv` y gráficos del cluster |
| `06_inferencia_spearman_regresion.R` | Spearman con permutación, regresión robusta, supuestos y sensibilidad con Holm | tabla maestra, asignación de clusters | `i2_inferencia_*.csv` y gráficos de inferencia |
| `07_robustez_nivel_departamental.R` | El mismo test entre departamentos | `input/`, `auxiliar/` | `dep_*.csv`, `dep_fig_dispersion.png` |
| `08_graficos_instancia3.R` | Los dos gráficos finales, exploratorio y comunicativo | tabla maestra, asignación de clusters | `grupo_22_exploratorio.jpg`, `grupo_22_comunicativo.jpg` |

Las funciones propias están en `utils/`: `calcular_hhi.R`, `calcular_theil_entropia.R`, `calcular_lq.R`, `winsorizar.R`, `p_permutacion_spearman.R` y `tema_tp.R` (tema y paletas de los gráficos).

## Conclusiones principales

- **Hipótesis 1, se cumple.** Las provincias se diferencian por su estructura y no solo por su tamaño. Santa Cruz, Neuquén y Chubut son las más especializadas (en minería), y el cluster ordena a las provincias en cuatro familias. La estructura de grupos es débil: la silueta promedio es 0,19 y no se distingue de la de datos simulados sin grupos.
- **Hipótesis 2, se cumple en parte.** Entre las 24 provincias y con el empleo medido en 87 ramas, más diversificación se asocia con más empresas exportadoras por establecimiento (ρ de Spearman = −0,62 con el HHI, p de permutación = 0,0015). Esa asociación resiste a los outliers, pero no aparece con 21 sectores ni con 945 actividades, y tampoco se repite entre departamentos.
- **Qué podemos afirmar.** Que hay una asociación entre las provincias argentinas que depende de cómo se mide la diversificación. No podemos afirmar que sea general ni causal.
- **Próximos pasos.** Sumar exportaciones provinciales en dólares para construir un ECI exportador, usar más años y explicar por qué el signo cambia entre provincias y departamentos.
