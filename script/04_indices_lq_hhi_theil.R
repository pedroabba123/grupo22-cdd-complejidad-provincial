# =============================================================================
# TP Ciencia de Datos - FCE UBA - Grupo 22
# ENTREGA FINAL - Script 04: metodo cuantitativo 1, indices de especializacion
#
# Objetivo: (a) cocientes de localizacion (LQ) por provincia y sector, (b) indices de
# concentracion (HHI, N equivalente, Theil, CR4) a tres niveles de desagregacion,
# (c) diversidad y ubicuidad (bloques de la complejidad de Hidalgo-Hausmann, clase 11)
# y comparacion con el ECI de Fundar. Año de referencia: 2022 (2021 solo como control).
# Correr desde la carpeta del proyecto.
# =============================================================================

suppressMessages({
  library(tidyverse)
  library(scales)
  library(ggrepel)
  library(patchwork)
})
options(scipen = 999)
set.seed(2026)

source("utils/calcular_hhi.R")
source("utils/calcular_theil_entropia.R")
source("utils/calcular_lq.R")
source("utils/tema_tp.R")

d   <- read_csv("input/establecimientos.csv", show_col_types = FALSE)
aux <- read_csv("auxiliar/provincias_aux.csv", show_col_types = FALSE)
d22 <- d |> filter(anio == 2022)

dir.create("output/tablas", showWarnings = FALSE, recursive = TRUE)
dir.create("output/graficos", showWarnings = FALSE, recursive = TRUE)

# Nombres cortos de los sectores (letra CLAE) para graficos
sectores <- tribble(
  ~letra, ~sector_corto,
  "A", "Agro", "B", "Minería", "C", "Industria", "D", "Electricidad y gas", "E", "Agua y residuos",
  "F", "Construcción", "G", "Comercio", "H", "Transporte", "I", "Alojamiento y gastro.",
  "J", "Información y com.", "K", "Finanzas y seguros", "L", "Inmobiliarias", "M", "Prof. y técnicas",
  "N", "Apoyo administrativo", "O", "Administración pública", "P", "Enseñanza", "Q", "Salud",
  "R", "Arte y recreación", "S", "Otros servicios", "T", "Hogares", "U", "Extraterritoriales"
)

# ---- matrices provincia x categoria (empleo) ----
matriz <- function(datos, var) {
  w <- datos |> group_by(provincia, cat = .data[[var]]) |> summarise(e = sum(empleo), .groups = "drop") |>
    pivot_wider(names_from = cat, values_from = e, values_fill = 0)
  m <- as.matrix(w[, -1]); rownames(m) <- w$provincia
  m
}
Ml <- matriz(d22, "letra"); M2 <- matriz(d22, "clae2"); M6 <- matriz(d22, "clae6")
Ml21 <- matriz(filter(d, anio == 2021), "letra"); M221 <- matriz(filter(d, anio == 2021), "clae2")
cat("Dimensiones 2022 -> letra:", dim(Ml), " clae2:", dim(M2), " clae6:", dim(M6), "\n")

# =============================================================================
# 1. COCIENTES DE LOCALIZACION (nivel letra)
# =============================================================================
LQ <- calcular_lq(Ml)
sh <- Ml / rowSums(Ml)

lq_largo <- as_tibble(LQ, rownames = "provincia") |>
  pivot_longer(-provincia, names_to = "letra", values_to = "lq") |>
  left_join(as_tibble(sh, rownames = "provincia") |> pivot_longer(-provincia, names_to = "letra", values_to = "share"),
            by = c("provincia", "letra")) |>
  left_join(sectores, by = "letra")
write_csv(lq_largo, "output/tablas/i2_lq_provincia_letra.csv")

# Especializacion principal: mayor LQ entre sectores que pesan al menos 1% del empleo provincial.
# El piso evita que un sector minusculo (ej. 0,3% del empleo) gane por azar.
principal <- function(piso) {
  lq_largo |> filter(share >= piso) |> group_by(provincia) |> slice_max(lq, n = 1, with_ties = FALSE) |> ungroup() |>
    transmute(provincia, letra, sector_corto, share = round(100 * share, 1), lq = round(lq, 3))
}
esp_principal <- principal(0.01)
write_csv(esp_principal |> arrange(desc(lq)), "output/tablas/i2_lq_especializacion_principal.csv")
esp_principal_2 <- principal(0.02)
cat("Coincide la especializacion principal con piso 1% y 2%:", sum(esp_principal$letra == esp_principal_2$letra), "de 24\n")
print(esp_principal |> arrange(desc(lq)), n = 24)

# Cantidad de sectores con LQ > 1 y con LQ > 2 (piso 1% de empleo)
n_lq <- lq_largo |> filter(share >= 0.01) |> group_by(provincia) |>
  summarise(n_sect_lq1 = sum(lq > 1), n_sect_lq2 = sum(lq > 2), .groups = "drop")

# Heatmap de LQ (letras A a S; T y U pesan 0,0% del empleo)
orden_prov <- hclust(dist(scale(as.matrix(sh[, setdiff(colnames(sh), c("T", "U"))]))), "ward.D2")$order
orden_prov <- rownames(sh)[orden_prov]
orden_col <- sectores |> filter(!letra %in% c("T", "U")) |>
  left_join(tibble(letra = colnames(Ml), nac = colSums(Ml) / sum(Ml)), by = "letra") |> arrange(desc(nac)) |> pull(sector_corto)

hm <- lq_largo |> filter(!letra %in% c("T", "U")) |>
  left_join(aux |> select(provincia, provincia_label), by = "provincia") |>
  mutate(provincia_label = factor(provincia_label, levels = rev(aux$provincia_label[match(orden_prov, aux$provincia)])),
         sector_corto = factor(sector_corto, levels = orden_col),
         lq_plot = pmax(pmin(log2(lq), 3), -3),
         etiqueta = ifelse(lq >= 2, formatC(lq, format = "f", digits = 1, decimal.mark = ","), ""))
g_hm <- ggplot(hm, aes(sector_corto, provincia_label, fill = lq_plot)) +
  geom_tile(colour = "white", linewidth = 0.4) +
  geom_text(aes(label = etiqueta), size = 2.6) +
  scale_fill_gradient2(low = "#3B6FB6", mid = "#F5F5F5", high = "#D9822B", midpoint = 0, limits = c(-3, 3),
                       breaks = c(-3, -1, 0, 1, 3), labels = c("1/8", "1/2", "1", "2", "8 o más"),
                       name = "LQ (escala log)") +
  labs(title = "La minería patagónica es la especialización más intensa del país",
       subtitle = "Cociente de localización por provincia y sector. Naranja: el sector pesa más en la provincia que en el país. Se muestra el valor cuando LQ ≥ 2.",
       caption = fuente_cep,
       x = NULL, y = NULL) +
  tema_tp(11) +
  theme(axis.text.x = element_text(angle = 40, hjust = 1, vjust = 1), legend.position = "bottom", panel.grid = element_blank())
ggsave("output/graficos/i2_fig_lq_heatmap.png", g_hm, width = 11, height = 7.5, dpi = 150)

# =============================================================================
# 2. CONCENTRACION: HHI, N equivalente, Theil, CR4 y D (UNCTAD) a tres niveles
# =============================================================================
indices_nivel <- function(M, nivel, N_cat = ncol(M)) {
  nac <- colSums(M) / sum(M)
  tibble(
    provincia = rownames(M),
    !!paste0("hhi_", nivel)   := apply(M, 1, calcular_hhi),
    !!paste0("neq_", nivel)   := 1 / apply(M, 1, calcular_hhi),
    !!paste0("theil_", nivel) := apply(M, 1, calcular_theil_entropia, n_categorias = N_cat),
    !!paste0("cr4_", nivel)   := apply(M, 1, function(r) sum(sort(r / sum(r), decreasing = TRUE)[1:4])),
    !!paste0("D_", nivel)     := apply(M, 1, function(r) sum(abs(r / sum(r) - nac)) / 2)
  )
}
I <- indices_nivel(Ml, "letra") |> left_join(indices_nivel(M2, "c2"), by = "provincia") |>
  left_join(indices_nivel(M6, "c6"), by = "provincia")

# Control con 2021
I21 <- indices_nivel(Ml21, "letra") |> select(provincia, hhi_letra_21 = hhi_letra) |>
  left_join(indices_nivel(M221, "c2") |> select(provincia, hhi_c2_21 = hhi_c2), by = "provincia")
estab <- tibble(
  par = c("HHI letra 2021 vs 2022", "HHI clae2 2021 vs 2022"),
  spearman = c(cor(I$hhi_letra, I21$hhi_letra_21, method = "spearman"), cor(I$hhi_c2, I21$hhi_c2_21, method = "spearman"))
)
print(estab)

# Estabilidad del ranking entre niveles de desagregacion (Spearman)
cor_niveles <- cor(I |> select(hhi_letra, hhi_c2, hhi_c6), method = "spearman")
cor_niveles_df <- as_tibble(round(cor_niveles, 2), rownames = "indice")
write_csv(cor_niveles_df, "output/tablas/i2_cor_rankings_niveles.csv")
print(cor_niveles_df)
cor_theil_hhi <- tibble(nivel = c("letra", "clae2", "clae6"),
                        spearman_hhi_theil = c(cor(I$hhi_letra, I$theil_letra, method = "spearman"),
                                               cor(I$hhi_c2, I$theil_c2, method = "spearman"),
                                               cor(I$hhi_c6, I$theil_c6, method = "spearman")))
write_csv(cor_theil_hhi, "output/tablas/i2_cor_hhi_theil.csv"); print(cor_theil_hhi)

# Relacion con el tamaño (empleo): los indices de clae6 reflejan mucho el tamaño de la provincia
tam <- d22 |> group_by(provincia) |> summarise(empleo = sum(empleo), establecimientos = sum(establecimientos),
                                              exportadoras = sum(empresas_exportadoras), .groups = "drop") |>
  mutate(int_exp = 100 * exportadoras / establecimientos)
tam_trad <- d22 |> filter(letra %in% c("A", "B", "C")) |> group_by(provincia) |>
  summarise(int_exp_trad = 100 * sum(empresas_exportadoras) / sum(establecimientos), .groups = "drop")
I <- I |> left_join(tam, by = "provincia") |> left_join(tam_trad, by = "provincia")
cor_tam <- tibble(indice = c("hhi_letra", "hhi_c2", "hhi_c6"),
                  spearman_con_empleo = c(cor(I$hhi_letra, I$empleo, method = "spearman"),
                                          cor(I$hhi_c2, I$empleo, method = "spearman"),
                                          cor(I$hhi_c6, I$empleo, method = "spearman")))
write_csv(cor_tam, "output/tablas/i2_cor_hhi_empleo.csv"); print(cor_tam)

# Bump chart: ranking de concentracion (1 = mas concentrada) segun el nivel
rk <- I |> transmute(provincia, Letra = rank(-hhi_letra), Clae2 = rank(-hhi_c2), Clae6 = rank(-hhi_c6)) |>
  pivot_longer(-provincia, names_to = "nivel", values_to = "ranking") |>
  mutate(nivel = factor(nivel, levels = c("Letra", "Clae2", "Clae6"),
                        labels = c(paste0("Letra\n(", ncol(Ml), " sectores)"), paste0("Clae2\n(", ncol(M2), " ramas)"),
                                   paste0("Clae6\n(", ncol(M6), " actividades)")))) |>
  left_join(aux |> select(provincia, provincia_label), by = "provincia")
destacar <- c("Tierra del Fuego", "CABA", "Buenos Aires", "Formosa", "Tucumán", "Salta", "San Luis")
g_bump <- ggplot(rk, aes(nivel, ranking, group = provincia_label)) +
  geom_line(data = ~ filter(.x, !provincia_label %in% destacar), colour = "grey80", linewidth = 0.7) +
  geom_line(data = ~ filter(.x, provincia_label %in% destacar), aes(colour = provincia_label), linewidth = 1.3) +
  geom_point(data = ~ filter(.x, provincia_label %in% destacar), aes(colour = provincia_label), size = 2.6) +
  geom_text_repel(data = ~ filter(.x, nivel == levels(nivel)[1], provincia_label %in% destacar), aes(label = provincia_label, colour = provincia_label),
                  nudge_x = -0.15, direction = "y", hjust = 1, size = 3.4, segment.color = NA) +
  geom_text_repel(data = ~ filter(.x, nivel == levels(nivel)[3], provincia_label %in% destacar), aes(label = provincia_label, colour = provincia_label),
                  nudge_x = 0.15, direction = "y", hjust = 0, size = 3.4, segment.color = NA) +
  scale_y_reverse(breaks = c(1, 6, 12, 18, 24)) +
  scale_x_discrete(expand = expansion(add = c(0.9, 0.9))) +
  scale_colour_manual(values = c("#1F4E79", "#D9822B", "#4E8F5A", "#9B3D3D", "#6B5B95", "#C49C00", "#2A9D8F")) +
  labs(title = "El ranking de concentración cambia con el nivel de desagregación",
       subtitle = "Posición de cada provincia según HHI del empleo (1 = más concentrada)",
       caption = fuente_cep,
       x = NULL, y = "Posición") +
  tema_tp() + theme(legend.position = "none", panel.grid.major.x = element_blank())
ggsave("output/graficos/i2_fig_bump_ranking.png", g_bump, width = 9, height = 6.5, dpi = 150)

# Numero equivalente a clae2 (grafico principal de concentracion)
g_neq <- I |> left_join(aux, by = "provincia") |>
  mutate(provincia_label = fct_reorder(provincia_label, neq_c2)) |>
  ggplot(aes(neq_c2, provincia_label, colour = region)) +
  geom_segment(aes(x = 0, xend = neq_c2, yend = provincia_label), linewidth = 1) +
  geom_point(size = 3) +
  scale_colour_manual(values = pal_regiones, name = NULL) +
  labs(title = paste0("CABA reparte su empleo en ", round(max(I$neq_c2)), " ramas equivalentes; Formosa, en ", round(min(I$neq_c2))),
       subtitle = "Número equivalente de ramas (1/HHI, clae2, 2022):
cuántas ramas de igual tamaño darían la misma concentración",
       caption = paste0(fuente_cep, " Color: región."),
       x = paste0("N equivalente (de ", ncol(M2), " ramas posibles)"), y = NULL) +
  tema_tp()
ggsave("output/graficos/i2_fig_neq_clae2.png", g_neq, width = 8, height = 6.5, dpi = 150)

# =============================================================================
# 3. DIVERSIDAD Y UBICUIDAD (Hidalgo-Hausmann, version empleo) a clae6
# =============================================================================
LQ6 <- calcular_lq(M6)
div_ubi <- function(umbral) {
  Mcp <- (LQ6 > 1) & (M6 >= umbral)                    # provincia p "tiene" la actividad a
  div <- rowSums(Mcp); ubi <- colSums(Mcp)
  k1  <- sapply(seq_len(nrow(Mcp)), function(i) mean(ubi[Mcp[i, ]]))
  tibble(provincia = rownames(Mcp), diversidad = div, ubicuidad_media = k1, umbral = umbral)
}
du <- map_dfr(c(10, 50, 100), div_ubi)
write_csv(du, "output/tablas/i2_diversidad_ubicuidad_todos_umbrales.csv")
du50 <- du |> filter(umbral == 50) |> select(-umbral)

comp <- I |> select(provincia, empleo, neq_c6) |> left_join(du50, by = "provincia") |> left_join(aux, by = "provincia")
sens <- du |> left_join(aux |> select(provincia, fundar_eci), by = "provincia") |> group_by(umbral) |>
  summarise(rho_div_vs_fundar = cor(diversidad, fundar_eci, method = "spearman"),
            rho_ubic_vs_fundar = cor(ubicuidad_media, fundar_eci, method = "spearman"),
            rho_div_ubic = cor(diversidad, ubicuidad_media, method = "spearman"), .groups = "drop")
write_csv(sens, "output/tablas/i2_complejidad_vs_fundar.csv"); print(sens)
rho_f <- cor.test(comp$diversidad, comp$fundar_eci, method = "spearman", exact = FALSE)
rho_f_k <- cor.test(comp$ubicuidad_media, comp$fundar_eci, method = "spearman", exact = FALSE)
rho_f_n <- cor.test(comp$neq_c6, comp$fundar_eci, method = "spearman", exact = FALSE)
cat("Spearman diversidad vs ECI Fundar:", round(rho_f$estimate, 2), " p=", signif(rho_f$p.value, 3), "\n")
cat("Spearman ubicuidad media vs ECI Fundar:", round(rho_f_k$estimate, 2), " p=", signif(rho_f_k$p.value, 3), "\n")
cat("Spearman N equivalente clae6 vs ECI Fundar:", round(rho_f_n$estimate, 2), " p=", signif(rho_f_n$p.value, 3), "\n")
cat("Spearman diversidad vs empleo provincial:", round(cor(comp$diversidad, comp$empleo, method = "spearman"), 2), "\n")

g_du <- ggplot(comp, aes(diversidad, ubicuidad_media)) +
  geom_point(aes(colour = region), size = 3) +
  geom_text_repel(aes(label = provincia_label), size = 3.2, max.overlaps = 30) +
  scale_colour_manual(values = pal_regiones, name = NULL) +
  labs(title = "Las provincias más diversas tienen actividades menos ubicuas",
       subtitle = "Clae6, 2022. Actividades con LQ > 1 y al menos 50 empleos en la provincia",
       x = "Diversidad: cantidad de actividades con LQ > 1", y = "Ubicuidad media de esas actividades\n(en cuántas provincias tienen LQ > 1)") +
  tema_tp()
g_f <- ggplot(comp, aes(ubicuidad_media, fundar_eci)) +
  geom_point(aes(colour = region), size = 3) +
  geom_text_repel(aes(label = provincia_label), size = 3.2, max.overlaps = 30) +
  scale_colour_manual(values = pal_regiones, name = NULL) +
  labs(title = "Menor ubicuidad media, mayor ECI de Fundar",
       subtitle = paste0("Spearman = ", format(round(rho_f_k$estimate, 2), decimal.mark = ",", nsmall = 2), " (n = 24). Con la diversidad: ", format(round(rho_f$estimate, 2), decimal.mark = ",", nsmall = 2)),
       x = "Ubicuidad media (nuestra, con empleo)", y = "ECI por empleo (Fundar, 2024)") +
  tema_tp()
ggsave("output/graficos/i2_fig_diversidad_ubicuidad.png",
       (g_du + g_f + plot_layout(guides = "collect") +
          plot_annotation(caption = fuente_cep_f, theme = theme(plot.caption = element_text(colour = "grey40", size = 8, hjust = 0)))) &
         theme(legend.position = "bottom"),
       width = 13, height = 6.3, dpi = 150)

# =============================================================================
# 4. TABLA MAESTRA POR PROVINCIA (insumo de los scripts 05 y 06)
# =============================================================================
maestra <- I |> left_join(du50, by = "provincia") |> left_join(aux, by = "provincia") |>
  left_join(esp_principal |> select(provincia, esp_letra = letra, esp_sector = sector_corto, esp_share = share, esp_lq = lq), by = "provincia") |>
  left_join(n_lq, by = "provincia")
write_csv(maestra, "output/tablas/i2_tabla_maestra_provincias.csv")
cat("\nHHI (clae2) por provincia, orden descendente:\n")
print(maestra |> arrange(desc(hhi_c2)) |> select(provincia, hhi_c2, neq_c2, hhi_letra, neq_letra, hhi_c6, neq_c6, int_exp, diversidad, ubicuidad_media, fundar_eci), n = 24)
cat("\nScript 04 terminado.\n")
