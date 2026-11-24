# =============================================================================
# TP Ciencia de Datos - FCE UBA - Grupo 22
# Tema: Complejidad Economica Provincial
#
# ENTREGA FINAL - Script 01: carga de la base y exploracion inicial (EDA de la primera entrega)
#
# Base: Datos_por_departamento_y_actividad.csv
# Fuente: datos.produccion.gob.ar - "Distribucion geografica de
# establecimientos productivos" (Ministerio de Economia / ex Produccion).
# Es la misma base usada en las clases 4 y 6 del curso.
#
# Idea del trabajo: en vez de mirar la diferencia entre provincias solo por
# PBI, miramos COMO esta compuesta la estructura productiva de cada provincia
# (en que sectores tiene empleo y establecimientos) para despues, en la
# Instancia 2, poder calcular indicadores de especializacion/complejidad
# (ventaja comparativa revelada, indices de concentracion, etc.). En esta
# primera entrega el objetivo es solo conocer y describir la base.
# =============================================================================


# =============================================================================
# 0. PAQUETES Y CONFIGURACION
# =============================================================================

library(tidyverse)
library(skimr)
library(janitor)
library(scales)

options(scipen = 999)  # evitar notacion cientifica en numeros grandes


# =============================================================================
# 1. RUTAS DE TRABAJO
# =============================================================================

instub_raw    <- "raw"
instub_input  <- "input"
outstub_tab   <- "output/tablas"
outstub_graf  <- "output/graficos"

ruta_csv <- file.path(instub_raw, "Datos_por_departamento_y_actividad.csv")

# Carpetas de salida (git no guarda carpetas vacias, asi que se crean si no existen)
for (carpeta in c(instub_input, outstub_tab, outstub_graf)) dir.create(carpeta, showWarnings = FALSE, recursive = TRUE)


# =============================================================================
# 2. CARGA DE DATOS
# =============================================================================

establecimientos <- read_csv(ruta_csv, show_col_types = FALSE)
establecimientos <- clean_names(establecimientos)

# Guardamos una copia "de trabajo" en input/, tal como pide la estructura del TP:
# raw/ nunca se toca, input/ es la version lista para analizar.
write_csv(establecimientos, file.path(instub_input, "establecimientos.csv"))


# =============================================================================
# 3. DICCIONARIO DE VARIABLES
# =============================================================================
# La consigna de la entrega 1 pide describir que representa cada variable, de
# que tipo es y si tiene nulos. Lo armamos como objeto de R para poder
# exportarlo directo a la tabla de la PPT.

diccionario <- tribble(
  ~variable,             ~descripcion,                                                          ~tipo,
  "anio",                "Año de la observación (2021 o 2022)",                                 "Numérica (temporal)",
  "in_departamentos",    "Código INDEC del departamento/partido",                                "Categórica (id)",
  "departamento",        "Nombre del departamento o partido",                                    "Categórica",
  "provincia_id",        "Código INDEC de la provincia",                                          "Categórica (id)",
  "provincia",           "Nombre de la provincia (24: 23 provincias + CABA)",                     "Categórica",
  "clae6",               "Código de actividad económica a 6 dígitos (CLAE, clasificador AFIP)",   "Categórica",
  "clae2",               "Código de actividad económica a 2 dígitos (agregación de clae6)",       "Categórica",
  "letra",               "Letra de sección económica (A a U, agregación máxima del CLAE)",        "Categórica",
  "empleo",               "Cantidad de puestos de trabajo formales declarados",                   "Numérica",
  "establecimientos",     "Cantidad de establecimientos productivos",                             "Numérica",
  "empresas_exportadoras","Cantidad de empresas exportadoras dentro de esa celda",                "Numérica"
)

print(diccionario, n = Inf)
write_csv(diccionario, file.path(outstub_tab, "diccionario_variables.csv"))

# Diccionario de letras (seccion economica). No viene en la base: es la
# clasificacion CLAE/CLANAE, alineada a CIIU Rev.4, de conocimiento publico.
# Se usa solo para leer mejor los graficos y tablas, no modifica los datos.
dic_letra <- tribble(
  ~letra, ~sector,
  "A", "Agricultura, ganadería, caza y silvicultura",
  "B", "Explotación de minas y canteras",
  "C", "Industria manufacturera",
  "D", "Electricidad, gas, vapor y aire acondicionado",
  "E", "Agua, saneamiento y gestión de residuos",
  "F", "Construcción",
  "G", "Comercio mayorista y minorista",
  "H", "Transporte y almacenamiento",
  "I", "Alojamiento y servicios de comida",
  "J", "Información y comunicaciones",
  "K", "Servicios financieros y de seguros",
  "L", "Actividades inmobiliarias",
  "M", "Actividades profesionales, científicas y técnicas",
  "N", "Actividades administrativas y de apoyo",
  "O", "Administración pública y defensa",
  "P", "Enseñanza",
  "Q", "Salud humana y servicios sociales",
  "R", "Artes, entretenimiento y recreación",
  "S", "Otras actividades de servicios",
  "T", "Hogares como empleadores",
  "U", "Organismos extraterritoriales"
)
write_csv(dic_letra, file.path(outstub_tab, "diccionario_letras.csv"))


# =============================================================================
# 4. PRIMER VISTAZO: DIMENSIONES, UNIDAD DE OBSERVACION, TIPOS
# =============================================================================

dim(establecimientos)
glimpse(establecimientos)

# La unidad de observacion declarada es: departamento x actividad (clae6) x anio.
# Chequeamos que esa combinacion sea unica (si hay repetidos, algo esta mal).
establecimientos |>
  count(anio, in_departamentos, clae6) |>
  filter(n > 1) |>
  nrow()
# Si da 0, la clave anio-departamento-clae6 es unica: confirma la unidad de
# observacion.

n_distinct(establecimientos$provincia)     # 24 (23 provincias + CABA)
n_distinct(establecimientos$in_departamentos) # departamentos/partidos
n_distinct(establecimientos$clae6)         # actividades a 6 digitos
table(establecimientos$anio)


# =============================================================================
# 5. VALORES FALTANTES
# =============================================================================

colSums(is.na(establecimientos))

faltantes <- establecimientos |>
  summarise(across(everything(), ~ mean(is.na(.x)) * 100)) |>
  pivot_longer(everything(), names_to = "variable", values_to = "pct_na") |>
  arrange(desc(pct_na))

faltantes
write_csv(faltantes, file.path(outstub_tab, "valores_faltantes.csv"))


# =============================================================================
# 6. DESCRIPTIVAS GENERALES (NIVEL NACIONAL)
# =============================================================================

establecimientos |>
  group_by(anio) |>
  summarise(
    empleo_total = sum(empleo, na.rm = TRUE),
    establecimientos_total = sum(establecimientos, na.rm = TRUE),
    empresas_exp_total = sum(empresas_exportadoras, na.rm = TRUE),
    .groups = "drop"
  )

skim(establecimientos |> select(empleo, establecimientos, empresas_exportadoras))

# Tamaño medio de establecimiento (empleo por establecimiento), a nivel de
# celda departamento x actividad. Es una variable derivada muy usada en el
# curso (clase04) para comparar estructura productiva.
establecimientos <- establecimientos |>
  mutate(emp_por_estab = if_else(establecimientos > 0, empleo / establecimientos, NA_real_))

summary(establecimientos$emp_por_estab)


# =============================================================================
# 7. AGREGADO A NIVEL PROVINCIA (para el tema del TP)
# =============================================================================
# El tema del grupo es "Complejidad Economica Provincial": nos interesa la
# ESTRUCTURA productiva de cada provincia, asi que agregamos por provincia.

prov_2022 <- establecimientos |>
  filter(anio == 2022) |>
  group_by(provincia) |>
  summarise(
    empleo_total = sum(empleo, na.rm = TRUE),
    establecimientos_total = sum(establecimientos, na.rm = TRUE),
    empresas_exp_total = sum(empresas_exportadoras, na.rm = TRUE),
    n_departamentos = n_distinct(in_departamentos),
    n_actividades = n_distinct(clae6),
    .groups = "drop"
  ) |>
  mutate(
    emp_por_estab = empleo_total / establecimientos_total,
    share_empleo_nacional = round(100 * empleo_total / sum(empleo_total), 2)
  ) |>
  arrange(desc(empleo_total))

prov_2022
write_csv(prov_2022, file.path(outstub_tab, "resumen_provincias_2022.csv"))

# Concentracion geografica: cuanto empleo concentran las provincias mas
# grandes (dato inicial, sin calcular todavia un indice formal de
# concentracion, que se vera en la Unidad 3 / Instancia 2).
prov_2022 |>
  mutate(share_acum = cumsum(share_empleo_nacional)) |>
  select(provincia, share_empleo_nacional, share_acum) |>
  print(n = Inf)


# =============================================================================
# 8. COMPOSICION SECTORIAL (NACIONAL Y POR PROVINCIA)
# =============================================================================

# --- 8.1 Composicion sectorial nacional (2022) --------------------------------

sector_nacional <- establecimientos |>
  filter(anio == 2022) |>
  group_by(letra) |>
  summarise(empleo = sum(empleo, na.rm = TRUE), .groups = "drop") |>
  left_join(dic_letra, by = "letra") |>
  mutate(share = round(100 * empleo / sum(empleo), 2)) |>
  arrange(desc(share))

sector_nacional
write_csv(sector_nacional, file.path(outstub_tab, "composicion_sectorial_nacional.csv"))

# --- 8.2 Comparacion de estructura productiva entre 3 provincias -------------
# Elegimos tres provincias con perfiles productivos bien distintos para
# ilustrar el "por que" de las diferencias entre sistemas productivos que
# el grupo queria explorar: Buenos Aires (industria/servicios, la mas
# grande), Neuquen (mineria/hidrocarburos, Vaca Muerta) y Santiago del
# Estero (perfil mas agropecuario).

provincias_foco <- c("Buenos Aires", "Neuquen", "Santiago Del Estero")

sector_provincias <- establecimientos |>
  filter(anio == 2022, provincia %in% provincias_foco) |>
  group_by(provincia, letra) |>
  summarise(empleo = sum(empleo, na.rm = TRUE), .groups = "drop") |>
  group_by(provincia) |>
  mutate(share = round(100 * empleo / sum(empleo), 2)) |>
  ungroup() |>
  left_join(dic_letra, by = "letra") |>
  arrange(provincia, desc(share))

sector_provincias
write_csv(sector_provincias, file.path(outstub_tab, "composicion_sectorial_3provincias.csv"))


# --- 8.3 Perfil productivo dominante de las 24 provincias ---------------------
# En vez de elegir 3 provincias a mano, miramos las 24: para cada una, cual es
# el sector (letra) que mas empleo concentra, y como se compara ese mismo
# porcentaje con el peso que tiene ese sector a nivel nacional. Esto deja ver
# los casos "raros" sin que los hayamos elegido nosotros de antemano.
# Ojo: mostramos los dos porcentajes juntos (provincial y nacional), NO
# calculamos un cociente/indice formal entre ambos -> eso es contenido de la
# Unidad 3 (ventaja comparativa revelada / HHI), que se aborda en la
# Instancia 2. Aca nos quedamos en lo puramente descriptivo.

sector_prov_total <- establecimientos |>
  filter(anio == 2022) |>
  group_by(provincia, letra) |>
  summarise(empleo = sum(empleo, na.rm = TRUE), .groups = "drop") |>
  group_by(provincia) |>
  mutate(share_provincial = round(100 * empleo / sum(empleo), 1)) |>
  ungroup() |>
  left_join(sector_nacional |> select(letra, share_nacional = share), by = "letra") |>
  left_join(dic_letra, by = "letra")

perfil_dominante <- sector_prov_total |>
  group_by(provincia) |>
  slice_max(share_provincial, n = 1, with_ties = FALSE) |>
  ungroup() |>
  transmute(
    provincia, sector, share_provincial, share_nacional,
    brecha_pp = round(share_provincial - share_nacional, 1)
  ) |>
  arrange(desc(brecha_pp))

perfil_dominante
write_csv(perfil_dominante, file.path(outstub_tab, "perfil_dominante_provincias.csv"))

cat("\nProvincias donde el sector top se aleja mas del promedio nacional:\n")
print(perfil_dominante |> slice_max(brecha_pp, n = 5), n = Inf)
cat("\nProvincias cuyo sector top esta mas cerca del promedio nacional:\n")
print(perfil_dominante |> slice_min(brecha_pp, n = 5), n = Inf)


# --- 8.4 Orientacion exportadora -----------------------------------------------
# Variable que no habiamos explorado todavia: empresas_exportadoras. La
# miramos como intensidad exportadora = empresas exportadoras / establecimientos,
# por provincia y por sector. Es una razon simple entre dos variables de la
# base, no un metodo de la Unidad 2/3.

exportadoras_provincia <- prov_2022 |>
  transmute(
    provincia,
    establecimientos_total, empresas_exp_total,
    intensidad_exportadora = round(100 * empresas_exp_total / establecimientos_total, 2)
  ) |>
  arrange(desc(intensidad_exportadora))

exportadoras_provincia
write_csv(exportadoras_provincia, file.path(outstub_tab, "orientacion_exportadora_provincia.csv"))

exportadoras_sector <- establecimientos |>
  filter(anio == 2022) |>
  group_by(letra) |>
  summarise(
    establecimientos = sum(establecimientos, na.rm = TRUE),
    empresas_exp = sum(empresas_exportadoras, na.rm = TRUE),
    .groups = "drop"
  ) |>
  mutate(intensidad_exportadora = round(100 * empresas_exp / establecimientos, 2)) |>
  left_join(dic_letra, by = "letra") |>
  arrange(desc(intensidad_exportadora))

exportadoras_sector
write_csv(exportadoras_sector, file.path(outstub_tab, "orientacion_exportadora_sector.csv"))


# =============================================================================
# 9. GRAFICOS
# =============================================================================
# En esta primera entrega no se espera diseño elaborado: se prioriza que los
# graficos sean correctos, legibles y adecuados al tipo de variable.

tema_simple <- theme_minimal(base_size = 12)

# --- 9.1 Empleo total por provincia (barras) ----------------------------------

g_empleo_provincia <- prov_2022 |>
  mutate(provincia = fct_reorder(provincia, empleo_total)) |>
  ggplot(aes(x = empleo_total, y = provincia)) +
  geom_col(fill = "steelblue") +
  scale_x_continuous(labels = label_comma(big.mark = ".", decimal.mark = ",")) +
  labs(
    title = "Empleo formal total por provincia (2022)",
    x = "Puestos de trabajo", y = NULL
  ) +
  tema_simple

ggsave(file.path(outstub_graf, "01_empleo_por_provincia.png"),
       g_empleo_provincia, width = 8, height = 6, dpi = 150)

# --- 9.2 Composicion sectorial nacional (barras) ------------------------------

g_sector_nacional <- sector_nacional |>
  mutate(sector = fct_reorder(sector, share)) |>
  ggplot(aes(x = share, y = sector)) +
  geom_col(fill = "darkorange") +
  labs(
    title = "Composición del empleo por sector (2022)",
    x = "% del empleo total", y = NULL
  ) +
  tema_simple

ggsave(file.path(outstub_graf, "02_composicion_sectorial_nacional.png"),
       g_sector_nacional, width = 8, height = 7, dpi = 150)

# --- 9.3 Composicion sectorial: 3 provincias con perfiles distintos ----------

g_sector_provincias <- sector_provincias |>
  filter(share >= 3) |>  # nos quedamos con los sectores mas relevantes para que se lea
  mutate(sector = fct_reorder(sector, share, .fun = sum)) |>
  ggplot(aes(x = share, y = sector, fill = provincia)) +
  geom_col(position = "dodge") +
  labs(
    title = "Estructura productiva por provincia (2022)",
    subtitle = "Sectores con al menos 3% del empleo provincial",
    x = "% del empleo de la provincia", y = NULL, fill = "Provincia"
  ) +
  tema_simple

ggsave(file.path(outstub_graf, "03_estructura_3provincias.png"),
       g_sector_provincias, width = 10, height = 7, dpi = 150)

# --- 9.4 Histograma: tamaño medio de establecimiento --------------------------
# Variable numerica continua y con fuerte asimetria (muchas celdas chicas,
# pocas con establecimientos enormes): se muestra en escala log10.

g_hist_tamano <- establecimientos |>
  filter(anio == 2022, emp_por_estab > 0) |>
  ggplot(aes(x = emp_por_estab)) +
  geom_histogram(bins = 40, fill = "seagreen", color = "white") +
  scale_x_log10(labels = label_number()) +
  labs(
    title = "Distribución del tamaño medio de establecimiento (2022)",
    subtitle = "Empleo / establecimientos, por celda departamento-actividad. Escala logarítmica.",
    x = "Empleados por establecimiento (log10)", y = "Cantidad de celdas"
  ) +
  tema_simple

ggsave(file.path(outstub_graf, "04_histograma_tamano_establecimiento.png"),
       g_hist_tamano, width = 8, height = 6, dpi = 150)

# --- 9.5 Boxplot: empleo por departamento, comparando provincias foco --------

g_box_empleo <- establecimientos |>
  filter(anio == 2022, provincia %in% provincias_foco, empleo > 0) |>
  group_by(provincia, in_departamentos) |>
  summarise(empleo_depto = sum(empleo), .groups = "drop") |>
  ggplot(aes(x = provincia, y = empleo_depto)) +
  geom_boxplot(fill = "lightblue") +
  scale_y_log10(labels = label_comma(big.mark = ".", decimal.mark = ",")) +
  labs(
    title = "Empleo total por departamento, según provincia (2022)",
    subtitle = "Escala logarítmica por la fuerte dispersión entre departamentos",
    x = NULL, y = "Empleo por departamento (log10)"
  ) +
  tema_simple

ggsave(file.path(outstub_graf, "05_boxplot_empleo_departamento.png"),
       g_box_empleo, width = 7, height = 6, dpi = 150)

# --- 9.6 Dispersion: empleo vs establecimientos (relacion entre variables) ---

g_scatter <- establecimientos |>
  filter(anio == 2022, empleo > 0, establecimientos > 0) |>
  group_by(provincia, in_departamentos) |>
  summarise(empleo_depto = sum(empleo), estab_depto = sum(establecimientos),
            .groups = "drop") |>
  ggplot(aes(x = estab_depto, y = empleo_depto)) +
  geom_point(alpha = 0.5, color = "steelblue") +
  scale_x_log10(labels = label_comma(big.mark = ".", decimal.mark = ",")) +
  scale_y_log10(labels = label_comma(big.mark = ".", decimal.mark = ",")) +
  labs(
    title = "Relación entre establecimientos y empleo por departamento (2022)",
    subtitle = "Cada punto es un departamento. Escala log-log.",
    x = "Establecimientos (log10)", y = "Empleo (log10)"
  ) +
  tema_simple

ggsave(file.path(outstub_graf, "06_scatter_empleo_establecimientos.png"),
       g_scatter, width = 7, height = 6, dpi = 150)

# --- 9.7 Perfil dominante: share provincial vs nacional, las 24 provincias ----
# Grafico de puntos (tipo "dumbbell"): para cada provincia, el % de su sector
# top y el % que ese mismo sector tiene a nivel pais. Cuanto mas lejos estan
# los dos puntos, mas se aleja esa provincia del perfil productivo promedio.

perfil_plot <- perfil_dominante |>
  mutate(
    etiqueta = paste0(provincia, " — ", sector),
    etiqueta = fct_reorder(etiqueta, brecha_pp)
  )

g_perfil <- ggplot(perfil_plot) +
  geom_segment(aes(x = share_nacional, xend = share_provincial,
                    y = etiqueta, yend = etiqueta), color = "grey70", linewidth = 1) +
  geom_point(aes(x = share_nacional, y = etiqueta, color = "Peso nacional del sector"), size = 2.5) +
  geom_point(aes(x = share_provincial, y = etiqueta, color = "Peso en la provincia"), size = 2.5) +
  scale_color_manual(values = c("Peso nacional del sector" = "grey40",
                                 "Peso en la provincia" = "darkorange")) +
  labs(
    title = "Sector con más peso en cada provincia, vs. su peso a nivel país (2022)",
    subtitle = "Ordenado por qué tan lejos está cada provincia del promedio nacional",
    x = "% del empleo", y = NULL, color = NULL
  ) +
  theme_minimal(base_size = 10) +
  theme(legend.position = "top")

ggsave(file.path(outstub_graf, "07_perfil_dominante_provincias.png"),
       g_perfil, width = 9, height = 9, dpi = 150)

# --- 9.8 Orientacion exportadora por provincia --------------------------------

g_export_prov <- exportadoras_provincia |>
  mutate(provincia = fct_reorder(provincia, intensidad_exportadora)) |>
  ggplot(aes(x = intensidad_exportadora, y = provincia)) +
  geom_col(fill = "seagreen") +
  labs(
    title = "Intensidad exportadora por provincia (2022)",
    subtitle = "Empresas exportadoras cada 100 establecimientos",
    x = "%", y = NULL
  ) +
  tema_simple

ggsave(file.path(outstub_graf, "08_exportadoras_provincia.png"),
       g_export_prov, width = 8, height = 6, dpi = 150)


# =============================================================================
# 10. PRIMEROS HALLAZGOS (para completar a mano en la PPT con estos numeros)
# =============================================================================

# a. Concentracion geografica del empleo
top3_share <- prov_2022 |> slice_max(empleo_total, n = 3) |> pull(share_empleo_nacional) |> sum()
cat("Las 3 provincias con mas empleo concentran el", round(top3_share, 1), "% del empleo formal nacional (2022).\n")

# b. Peso del sector top a nivel nacional
cat("El sector con mayor participacion nacional es",
    sector_nacional$sector[1], "con el", sector_nacional$share[1], "% del empleo.\n")

# c. Correlacion empleo-establecimientos (a nivel departamento)
depto_2022 <- establecimientos |>
  filter(anio == 2022) |>
  group_by(in_departamentos) |>
  summarise(empleo_depto = sum(empleo), estab_depto = sum(establecimientos), .groups = "drop")
cor_emp_estab <- cor(depto_2022$empleo_depto, depto_2022$estab_depto, use = "complete.obs")
cat("Correlacion (Pearson) entre empleo y establecimientos por departamento:", round(cor_emp_estab, 2), "\n")

# d. Crecimiento del empleo 2021 -> 2022
emp_anual <- establecimientos |> group_by(anio) |> summarise(empleo = sum(empleo, na.rm = TRUE))
var_pct <- round(100 * (emp_anual$empleo[emp_anual$anio == 2022] /
                          emp_anual$empleo[emp_anual$anio == 2021] - 1), 2)
cat("Variacion del empleo formal total 2021->2022:", var_pct, "%\n")

# e. Departamento con mayor tamaño medio de establecimiento (posible outlier)
outlier_tamano <- establecimientos |>
  filter(anio == 2022, establecimientos >= 5) |>  # sacamos celdas con muy pocos establecimientos, poco robustas
  slice_max(emp_por_estab, n = 5) |>
  select(provincia, departamento, letra, clae6, empleo, establecimientos, emp_por_estab)
outlier_tamano

cat("\nScript 01 terminado. Tablas en output/tablas/, graficos en output/graficos/.\n")
