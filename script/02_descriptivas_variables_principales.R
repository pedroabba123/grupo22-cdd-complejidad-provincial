# =============================================================================
# TP Ciencia de Datos - FCE UBA - Grupo 22
# ENTREGA FINAL - Script 02: estadisticas descriptivas de las variables principales
#
# Que hace: describe las variables que efectivamente se usan en el trabajo (tipo, faltantes, tendencia
# central, dispersion y forma de la distribucion), a nivel de celda (departamento x actividad) y a nivel de
# provincia, ANTES de cualquier tratamiento de outliers. El script 03 muestra las mismas medidas despues.
#
# Lee    : input/establecimientos.csv, auxiliar/provincias_aux.csv
# Guarda : output/tablas/desc_*.csv y output/graficos/desc_fig_*.png
# =============================================================================

suppressMessages({
  library(tidyverse)
  library(scales)
})
source("utils/calcular_hhi.R")
source("utils/tema_tp.R")

d   <- read_csv("input/establecimientos.csv", show_col_types = FALSE)
aux <- read_csv("auxiliar/provincias_aux.csv", show_col_types = FALSE)

dir.create("output/tablas", showWarnings = FALSE, recursive = TRUE)
dir.create("output/graficos", showWarnings = FALSE, recursive = TRUE)

# ---- 1. Tabla de variables utilizadas -----------------------------------------------------------------
# Los faltantes se calculan sobre la base completa (2021 y 2022).
vars <- tribble(
  ~variable,              ~descripcion,                                                        ~tipo,
  "anio",                 "Año de referencia (2021 o 2022)",                                   "Temporal (discreta)",
  "provincia",            "Provincia (24 jurisdicciones, CABA incluida)",                      "Categórica nominal",
  "departamento",         "Departamento o partido, o comuna en CABA (527)",                    "Categórica nominal",
  "clae6",                "Actividad económica, CLAE a 6 dígitos (945 actividades)",           "Categórica nominal",
  "clae2",                "Rama de actividad, CLAE a 2 dígitos (87 ramas)",                    "Categórica nominal",
  "letra",                "Sector de actividad, letra CLAE (21 sectores)",                     "Categórica nominal",
  "empleo",               "Puestos de trabajo privados registrados, en personas",              "Numérica discreta",
  "establecimientos",     "Establecimientos productivos, en unidades",                         "Numérica discreta",
  "empresas_exportadoras", "Empresas que exportaron en el año, en unidades",                   "Numérica discreta"
) |>
  mutate(faltantes = map_int(variable, ~ sum(is.na(d[[.x]]))))
write_csv(vars, "output/tablas/desc_variables_usadas.csv")

# ---- 2. Estadisticas descriptivas a nivel celda (departamento x actividad), 2022 ------------------------
asimetria <- function(x) mean((x - mean(x))^3) / sd(x)^3   # coeficiente de asimetria de Fisher (momentos)

resumir <- function(x, nombre) tibble(
  variable = nombre, n = length(x), media = mean(x), mediana = median(x), desvio = sd(x),
  p25 = quantile(x, .25), p75 = quantile(x, .75), minimo = min(x), maximo = max(x), asimetria = asimetria(x)
)

d22 <- d |> filter(anio == 2022) |> mutate(empleo_por_estab = empleo / establecimientos)

celdas <- bind_rows(
  resumir(d22$empleo, "Empleo por celda"),
  resumir(d22$establecimientos, "Establecimientos por celda"),
  resumir(d22$empleo_por_estab, "Empleo por establecimiento")
) |> mutate(across(where(is.numeric), ~ round(.x, 2)))
write_csv(celdas, "output/tablas/desc_celdas_2022.csv")
print(celdas)

# ---- 3. Estadisticas descriptivas a nivel provincia, 2022 ------------------------------------------------
prov <- d22 |>
  group_by(provincia) |>
  summarise(empleo = sum(empleo), establecimientos = sum(establecimientos),
            exportadoras = sum(empresas_exportadoras), .groups = "drop") |>
  mutate(exportadoras_c100 = 100 * exportadoras / establecimientos,
         empleo_por_estab = empleo / establecimientos)

hhi_prov <- d22 |>
  group_by(provincia, clae2) |> summarise(e = sum(empleo), .groups = "drop") |>
  group_by(provincia) |> summarise(hhi_clae2 = calcular_hhi(e), .groups = "drop")
prov <- left_join(prov, hhi_prov, by = "provincia") |>
  mutate(ramas_equivalentes = 1 / hhi_clae2) |>
  left_join(aux |> select(provincia, provincia_label, region), by = "provincia")

provincias <- bind_rows(
  resumir(prov$empleo, "Empleo privado registrado (personas)"),
  resumir(prov$establecimientos, "Establecimientos"),
  resumir(prov$exportadoras_c100, "Exportadoras cada 100 establecimientos"),
  resumir(prov$hhi_clae2, "HHI del empleo por rama (clae2)"),
  resumir(prov$ramas_equivalentes, "Ramas equivalentes (1 / HHI)")
) |> mutate(across(where(is.numeric), ~ signif(.x, 4)))
write_csv(provincias, "output/tablas/desc_provincias_2022.csv")
print(provincias)

# ---- 4. Graficos descriptivos -------------------------------------------------------------------------
fmt_miles <- label_number(big.mark = ".", decimal.mark = ",")

# 4.1 Distribucion del empleo por celda y por establecimiento: asimetria fuerte, se justifica la escala log
largo <- bind_rows(
  tibble(panel = "Empleo por celda (personas)", valor = d22$empleo),
  tibble(panel = "Empleo por establecimiento (personas)", valor = d22$empleo_por_estab)
) |> mutate(panel = factor(panel, levels = unique(panel)))
med <- largo |> group_by(panel) |> summarise(mediana = median(valor), .groups = "drop")

g_dist <- ggplot(largo, aes(valor)) +
  geom_histogram(bins = 45, fill = pal_azul, colour = "white", linewidth = 0.2) +
  geom_vline(data = med, aes(xintercept = mediana), colour = pal_naranja, linewidth = 0.9) +
  geom_text(data = med, aes(x = mediana, y = Inf, label = paste0("Mediana: ", number(mediana, accuracy = 0.1, decimal.mark = ","))),
            colour = pal_naranja, hjust = -0.08, vjust = 1.6, size = 3.6, fontface = "bold") +
  facet_wrap(~ panel, scales = "free") +
  scale_x_log10(labels = fmt_miles) +
  scale_y_continuous(labels = fmt_miles) +
  labs(title = "La mayoría de las celdas son chicas y unas pocas son enormes",
       subtitle = "Distribución de las celdas departamento × actividad, 2022 (98.924 celdas). Eje horizontal en escala logarítmica.",
       x = NULL, y = "Cantidad de celdas", caption = fuente_cep) +
  tema_tp(base_size = 11) + theme(strip.text = element_text(face = "bold"))
ggsave("output/graficos/desc_fig_distribucion_celdas.png", g_dist, width = 9, height = 4.3, dpi = 200, bg = "white")

# 4.2 Exportadoras cada 100 establecimientos por provincia (barras ordenadas, desde cero, color = region)
g_exp <- prov |>
  mutate(provincia_label = fct_reorder(provincia_label, exportadoras_c100)) |>
  ggplot(aes(exportadoras_c100, provincia_label, fill = region)) +
  geom_col(width = 0.75) +
  geom_text(aes(label = number(exportadoras_c100, accuracy = 0.1, decimal.mark = ",")), hjust = -0.15, size = 3.2, colour = "grey25") +
  scale_fill_manual(values = pal_regiones, name = "Región") +
  scale_x_continuous(expand = expansion(mult = c(0, 0.12)), labels = label_number(decimal.mark = ",")) +
  labs(title = "Mendoza tiene la mayor proporción de empresas exportadoras",
       subtitle = "Empresas exportadoras cada 100 establecimientos, por provincia, 2022",
       x = "Empresas exportadoras cada 100 establecimientos", y = NULL, caption = fuente_cep) +
  tema_tp(base_size = 11) + theme(panel.grid.major.y = element_blank())
ggsave("output/graficos/desc_fig_exportadoras_provincias.png", g_exp, width = 7.5, height = 6.2, dpi = 200, bg = "white")

# 4.3 Empleo total por provincia (barras desde cero, en miles de personas)
g_emp <- prov |>
  mutate(provincia_label = fct_reorder(provincia_label, empleo), miles = empleo / 1000) |>
  ggplot(aes(miles, provincia_label, fill = region)) +
  geom_col(width = 0.75) +
  geom_text(aes(label = number(miles, accuracy = 1, big.mark = ".", decimal.mark = ",")), hjust = -0.15, size = 3.2, colour = "grey25") +
  scale_fill_manual(values = pal_regiones, name = "Región") +
  scale_x_continuous(expand = expansion(mult = c(0, 0.1)), labels = fmt_miles) +
  labs(title = "Buenos Aires y CABA reúnen más de la mitad del empleo privado",
       subtitle = "Empleo privado registrado por provincia, 2022",
       x = "Empleo privado registrado (miles de personas)", y = NULL, caption = fuente_cep) +
  tema_tp(base_size = 11) + theme(panel.grid.major.y = element_blank())
ggsave("output/graficos/desc_fig_empleo_provincias.png", g_emp, width = 8.5, height = 7, dpi = 200, bg = "white")

cat("\nScript 02 terminado.\n")
