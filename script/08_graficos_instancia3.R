# =============================================================================
# TP Ciencia de Datos - FCE UBA - Grupo 22
# ENTREGA FINAL - Script 08: las dos visualizaciones de la Instancia 3 (version final)
#
# Hallazgo que cuentan: las provincias con el empleo repartido en mas ramas de actividad tienen mas empresas
# exportadoras cada 100 establecimientos. La relacion aparece al nivel de ramas (clae2) y no al de sectores
# (letra) ni al de actividades (clae6). El script 07 muestra que tampoco se repite entre departamentos.
#
# Cambios respecto de la Instancia 3 (devolucion del docente):
#   - comunicativo: sin lenguaje tecnico (Spearman, p-valor), sin leyenda de tamano (se explica en el subtitulo),
#     subtitulo con el resultado, titulo mas enfatico con los colores de los dos grupos, sin las aclaraciones
#     sobre causalidad y nivel de desagregacion.
#   - exploratorio: se agrega el significado de cada cuadrante.
#
# Lee    : output/tablas/i2_tabla_maestra_provincias.csv, i2_cluster_asignacion.csv (scripts 04 y 05)
# Guarda : output/graficos/grupo_22_exploratorio.jpg y grupo_22_comunicativo.jpg (300 dpi)
# =============================================================================

suppressMessages({
  library(tidyverse)
  library(ggrepel)
  library(ggtext)
  library(scales)
})
source("utils/tema_tp.R", encoding = "UTF-8")
source("utils/p_permutacion_spearman.R", encoding = "UTF-8")

dir.create("output/graficos", showWarnings = FALSE, recursive = TRUE)

# ---- datos -------------------------------------------------------------------
maestra <- read_csv("output/tablas/i2_tabla_maestra_provincias.csv", show_col_types = FALSE)
clus    <- read_csv("output/tablas/i2_cluster_asignacion.csv", show_col_types = FALSE) |>
  select(provincia, grupo_nombre)

d <- maestra |>
  left_join(clus, by = "provincia") |>
  transmute(provincia = provincia_label, empleo, int_exp, grupo = grupo_nombre,
            neq_letra, neq_c2, neq_c6)

stopifnot(nrow(d) == 24, !anyNA(d$grupo), !anyNA(d$int_exp))

# Spearman entre ramas equivalentes (N = 1/HHI) y exportadoras cada 100 establecimientos, por nivel.
# Es la misma correlacion del cuerpo del trabajo con el signo invertido (N = 1/HHI es decreciente en el HHI).
niveles <- tribble(
  ~var,         ~nivel,                          ~n_cat,
  "neq_letra",  "Sectores (letra)",              "21 sectores",
  "neq_c2",     "Ramas (clae2)",                 "87 ramas",
  "neq_c6",     "Actividades (clae6)",           "945 actividades"
)

# p de permutacion (10.000 permutaciones), el mismo metodo del cuerpo del trabajo
rho_p <- map_dfr(niveles$var, function(v) {
  tibble(var = v, rho = cor(d[[v]], d$int_exp, method = "spearman"),
         p = p_permutacion_spearman(d[[v]], d$int_exp))
}) |> left_join(niveles, by = "var")
print(rho_p)

fmt_p <- function(p) {
  map_chr(p, function(x) paste0("p = ", formatC(x, format = "f", digits = if (x < 0.01) 4 else 2, decimal.mark = ",")))
}
rotulo <- function(r) paste0(r$nivel, " · ", r$n_cat, "\nρ de Spearman = ",
                             formatC(r$rho, format = "f", digits = 2, decimal.mark = ","), " (", fmt_p(r$p), ")")

# ---- 1. EXPLORATORIO: tres niveles de desagregacion, cuadrantes en las medianas ---------------------
largo <- d |>
  pivot_longer(c(neq_letra, neq_c2, neq_c6), names_to = "var", values_to = "neq") |>
  left_join(rho_p |> select(var, nivel, n_cat, rho, p), by = "var") |>
  mutate(panel = factor(paste0(nivel, " · ", n_cat, "\nρ de Spearman = ",
                               formatC(rho, format = "f", digits = 2, decimal.mark = ","), " (", fmt_p(p), ")"),
                        levels = map_chr(seq_len(nrow(rho_p)), ~ rotulo(rho_p[.x, ]))),
         # ajuste fino de algunas etiquetas para que no pisen puntos (unidades del eje y)
         ajuste_y = case_when(provincia == "Buenos Aires" ~ 0.9,
                              provincia == "Tierra del Fuego" & var == "neq_c2" ~ -0.8,
                              provincia == "Tierra del Fuego" & var == "neq_c6" ~ 0.7,
                              TRUE ~ 0))

medianas <- largo |> group_by(panel) |> summarise(mx = median(neq), my = median(int_exp), .groups = "drop")

# Significado de cada cuadrante, escrito en las esquinas de cada panel
esquinas <- expand_grid(panel = levels(largo$panel), esq = c("sup_der", "sup_izq", "inf_der", "inf_izq")) |>
  mutate(panel = factor(panel, levels = levels(largo$panel)),
         x = if_else(str_detect(esq, "der"), Inf, -Inf),
         y = if_else(str_detect(esq, "sup"), 8, 1.2),
         hj = if_else(str_detect(esq, "der"), 1.04, -0.04),
         vj = if_else(str_detect(esq, "sup"), 1.15, -0.15),
         texto = unname(c(sup_der = "Diversificadas y
más exportadoras",
                          sup_izq = "Concentradas pero
más exportadoras",
                          inf_der = "Diversificadas pero
menos exportadoras",
                          inf_izq = "Concentradas y
menos exportadoras")[esq]))

etiquetadas <- c("CABA", "Mendoza", "Formosa", "Tierra del Fuego", "Santa Cruz", "Buenos Aires")

abreviar <- c("Intermedio: agro, industria y servicios" = "Intermedio (agro, industria y servicios)",
              "Patagonia minera y petrolera"            = "Patagonia minera y petrolera",
              "CABA: servicios modernos"                = "CABA (servicios modernos)",
              "Comercio y construcción"                 = "Comercio y construcción")

g_expl <- ggplot(largo, aes(neq, int_exp)) +
  geom_vline(data = medianas, aes(xintercept = mx), colour = "grey70", linetype = "22", linewidth = 0.4) +
  geom_hline(data = medianas, aes(yintercept = my), colour = "grey70", linetype = "22", linewidth = 0.4) +
  geom_text(data = esquinas, aes(x = x, y = y, label = texto, hjust = hj, vjust = vj),
            inherit.aes = FALSE, colour = "grey45", size = 2.6, fontface = "italic", lineheight = 0.95) +
  geom_point(aes(size = empleo, colour = grupo, shape = grupo), alpha = 0.85) +
  # se pasan todas las provincias (con rotulo vacio salvo las nombradas) para que las etiquetas esquiven todos los puntos
  geom_text_repel(aes(label = ifelse(provincia %in% etiquetadas, ifelse(provincia == "Tierra del Fuego", "T. del Fuego", provincia), "")),
                  size = 3.2, colour = "grey15", min.segment.length = 0, segment.colour = "grey50",
                  nudge_y = largo$ajuste_y,
                  box.padding = 0.6, point.padding = 0.5, force = 6, max.iter = 20000, max.overlaps = Inf, seed = 5) +
  facet_wrap(~ panel, scales = "free_x") +
  scale_colour_manual(values = pal_grupos, labels = abreviar, name = "Grupo (cluster HCPC, k = 4)") +
  scale_shape_manual(values = forma_grupos, labels = abreviar, name = "Grupo (cluster HCPC, k = 4)") +
  scale_size_area(max_size = 7, breaks = c(50e3, 500e3, 2e6), labels = label_number(big.mark = ".", decimal.mark = ","),
                  name = "Empleo privado registrado") +
  scale_x_continuous(labels = label_number(big.mark = ".", decimal.mark = ","), expand = expansion(mult = 0.1)) +
  scale_y_continuous(labels = label_number(decimal.mark = ","), limits = c(1.2, 8), breaks = 2:8) +
  guides(colour = guide_legend(order = 1, nrow = 2, override.aes = list(size = 3.5)),
         shape  = guide_legend(order = 1, nrow = 2),
         size   = guide_legend(order = 2, nrow = 1)) +
  labs(
    title    = "Concentración del empleo y orientación exportadora de las 24 provincias, según el nivel de desagregación",
    subtitle = "Cada punto es una provincia. Las líneas punteadas son la mediana de cada eje y dividen cada panel en cuatro cuadrantes;
\"más\" y \"menos\" se miden contra esa mediana.",
    x        = "Ramas equivalentes del empleo (N = 1 / HHI; más a la derecha = empleo más diversificado)",
    y        = "Empresas exportadoras cada 100 establecimientos",
    caption  = paste0(fuente_cep, "\nEl eje vertical empieza en 1,2. HHI calculado sobre la distribución del empleo entre sectores, ramas o actividades. ",
                      "Los p-valores son de permutación (10.000) y sin corrección\npor comparaciones múltiples; con Holm solo se sostiene el panel de ramas.")
  ) +
  tema_tp(base_size = 10.5) +
  theme(strip.text = element_text(face = "bold", hjust = 0, size = 9.5),
        legend.box = "vertical", legend.spacing.y = unit(2, "pt"),
        legend.text = element_text(size = 8.5), legend.title = element_text(size = 9, face = "bold"),
        panel.grid.major = element_line(colour = "grey92"),
        plot.title = element_text(size = 12.5, face = "bold"),
        plot.caption = element_text(size = 7.5, lineheight = 1.15))

# ---- 2. COMUNICATIVO: un mensaje, el resto en gris --------------------------------------------------
col_div <- pal_azul; col_con <- pal_naranja

dc <- d |>
  mutate(rol = case_when(
    provincia %in% c("CABA", "Buenos Aires", "Santa Fe")         ~ "diversificadas",
    provincia %in% c("Formosa", "Tucumán", "Tierra del Fuego")   ~ "concentradas",
    provincia == "Mendoza"                                        ~ "excepcion",
    TRUE                                                          ~ "resto"))
col_rol <- c(diversificadas = col_div, concentradas = col_con, excepcion = "grey25", resto = "grey72")
# las burbujas grandes tapan sus propias etiquetas: se suben (unidades del eje y)
dc <- dc |> mutate(ajuste_y = case_when(provincia == "Buenos Aires" ~ 1.0, provincia == "CABA" ~ 0.9, provincia == "Santa Fe" ~ -1.1, TRUE ~ 0))

# Rangos de los dos grupos, para contarlos en el subtitulo (los numeros salen de los datos, no se escriben a mano)
rango <- function(r) {
  v <- dc$int_exp[dc$rol == r]
  paste0(formatC(min(v), format = "f", digits = 1, decimal.mark = ","), " y ", formatC(max(v), format = "f", digits = 1, decimal.mark = ","))
}
mza <- formatC(dc$int_exp[dc$provincia == "Mendoza"], format = "f", digits = 1, decimal.mark = ",")

titulo_com <- paste0("Las provincias con el empleo <b style='color:", col_div, "'>diversificado</b> tienen más empresas<br>",
                     "exportadoras que las de empleo <b style='color:", col_con, "'>concentrado</b>")
subtitulo_com <- paste0(
  "Empresas exportadoras cada 100 establecimientos, 2022. Formosa, Tucumán y Tierra del Fuego tienen entre ", rango("concentradas"), ";<br>",
  "CABA, Buenos Aires y Santa Fe, entre ", rango("diversificadas"), ". Mendoza, con ", mza, ", es la excepción. ",
  "El tamaño de cada burbuja es el empleo de la provincia.")

g_com <- ggplot(dc, aes(neq_c2, int_exp)) +
  geom_smooth(method = "lm", formula = y ~ x, se = FALSE, colour = "grey45", linewidth = 0.7, linetype = "22") +
  geom_point(data = filter(dc, rol == "resto"), aes(size = empleo), colour = col_rol["resto"], alpha = 0.9) +
  geom_point(data = filter(dc, rol != "resto"), aes(size = empleo, colour = rol), alpha = 0.95) +
  geom_text_repel(aes(label = ifelse(rol == "resto", "", provincia), colour = rol), size = 3.9, fontface = "bold",
                  min.segment.length = 0, segment.colour = "grey60", box.padding = 0.9, point.padding = 0.6,
                  nudge_y = dc$ajuste_y, force = 6, max.iter = 20000, max.overlaps = Inf, seed = 11, show.legend = FALSE) +
  scale_colour_manual(values = col_rol, guide = "none") +
  scale_size(range = c(2.5, 17), guide = "none") +
  scale_x_continuous(breaks = seq(10, 30, 5), expand = expansion(mult = c(0.03, 0.08))) +
  scale_y_continuous(limits = c(0, NA), breaks = 0:8, expand = expansion(mult = c(0, 0.08))) +
  annotate("text", x = 29.6, y = 0.55, hjust = 1, vjust = 0, colour = col_div, fontface = "bold", size = 3.6,
           label = "Empleo repartido en muchas ramas →") +
  annotate("text", x = 11.2, y = 0.55, hjust = 0, vjust = 0, colour = col_con, fontface = "bold", size = 3.6,
           label = "← Empleo concentrado en pocas ramas") +
  labs(
    title    = titulo_com,
    subtitle = subtitulo_com,
    x        = "Diversificación del empleo (cantidad de ramas de actividad equivalentes)",
    y        = "Empresas exportadoras cada 100 establecimientos",
    caption  = paste0(fuente_cep, "\nLa diversificación es la cantidad de ramas de igual tamaño que darían el mismo reparto del empleo que el observado (87 ramas CLAE).")
  ) +
  tema_tp(base_size = 11.5) +
  theme(plot.title = element_markdown(size = 16, face = "bold", colour = "grey10", lineheight = 1.15),
        plot.subtitle = element_markdown(size = 9.6, colour = "grey25", lineheight = 1.3),
        panel.grid.major.x = element_blank(), panel.grid.major.y = element_line(colour = "grey90"),
        plot.caption = element_text(size = 7.8, lineheight = 1.15))

# ---- exportar (jpg, 300 dpi) ------------------------------------------------------------------------
ggsave("output/graficos/grupo_22_exploratorio.jpg", g_expl, width = 25, height = 15, units = "cm", dpi = 300, bg = "white")
ggsave("output/graficos/grupo_22_comunicativo.jpg", g_com,  width = 22, height = 15, units = "cm", dpi = 300, bg = "white")
cat("Listo: output/graficos/grupo_22_exploratorio.jpg y grupo_22_comunicativo.jpg\n")
