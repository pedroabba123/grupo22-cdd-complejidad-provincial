# =============================================================================
# TP Ciencia de Datos - FCE UBA - Grupo 22
# ENTREGA FINAL - Script 07: robustez de la asociacion a nivel departamental
#
# Pregunta: la asociacion entre diversificacion del empleo y orientacion exportadora que vimos entre las 24
# provincias (script 06), ¿se repite entre departamentos? El docente sugirio probarlo en la devolucion de la
# Instancia 2: con 24 provincias el test tiene poca potencia; con cientos de departamentos hay mas datos,
# pero los departamentos de una misma provincia no son independientes entre si.
#
# Decisiones:
#   - Se usan los departamentos con al menos 500 empleos en 2022 (los mas chicos tienen indices muy ruidosos:
#     un departamento con 30 empleos no puede estar "diversificado" en 87 ramas).
#   - Mismas definiciones que a nivel provincia: HHI del empleo por rama (clae2) y empresas exportadoras cada
#     100 establecimientos.
#   - Como los departamentos de una provincia se parecen entre si, los errores estandar se agrupan por
#     provincia (cluster-robust). Ademas se estima un modelo con efectos fijos de provincia, que compara
#     departamentos dentro de la misma provincia.
#
# Lee    : input/establecimientos.csv, auxiliar/provincias_aux.csv
# Guarda : output/tablas/dep_*.csv y output/graficos/dep_fig_*.png
# =============================================================================

suppressMessages({
  library(tidyverse)
  library(sandwich)
  library(lmtest)
  library(scales)
})
source("utils/calcular_hhi.R")
source("utils/tema_tp.R")

d   <- read_csv("input/establecimientos.csv", show_col_types = FALSE)
aux <- read_csv("auxiliar/provincias_aux.csv", show_col_types = FALSE)

dir.create("output/tablas", showWarnings = FALSE, recursive = TRUE)
dir.create("output/graficos", showWarnings = FALSE, recursive = TRUE)
set.seed(2026)

# ---- 1. Indicadores por departamento (2022) -----------------------------------------------------------
d22 <- d |> filter(anio == 2022)

dep <- d22 |>
  group_by(provincia, in_departamentos, departamento) |>
  summarise(empleo = sum(empleo), establecimientos = sum(establecimientos),
            exportadoras = sum(empresas_exportadoras), .groups = "drop") |>
  mutate(exportadoras_c100 = 100 * exportadoras / establecimientos)

hhi_dep <- d22 |>
  group_by(provincia, in_departamentos, clae2) |> summarise(e = sum(empleo), .groups = "drop") |>
  group_by(provincia, in_departamentos) |> summarise(hhi_clae2 = calcular_hhi(e), .groups = "drop")

dep <- dep |>
  left_join(hhi_dep, by = c("provincia", "in_departamentos")) |>
  left_join(aux |> select(provincia, provincia_label, region), by = "provincia") |>
  mutate(ramas_equivalentes = 1 / hhi_clae2)

dep_ok <- dep |> filter(empleo >= 500)
cat("Departamentos:", nrow(dep), "| con al menos 500 empleos:", nrow(dep_ok),
    "| provincias representadas:", n_distinct(dep_ok$provincia), "\n")

# ---- 2. Correlacion de Spearman y modelos ---------------------------------------------------------------
rho_d <- cor.test(dep_ok$hhi_clae2, dep_ok$exportadoras_c100, method = "spearman", exact = FALSE)

m_pool <- lm(exportadoras_c100 ~ log(hhi_clae2) + log(empleo), data = dep_ok)
m_fe   <- lm(exportadoras_c100 ~ log(hhi_clae2) + log(empleo) + factor(provincia), data = dep_ok)

coef_cl <- function(m, nombre) {
  ct <- coeftest(m, vcov = vcovCL(m, cluster = dep_ok$provincia, type = "HC1"))
  tibble(modelo = nombre, termino = rownames(ct), coef = ct[, 1], ee_cluster = ct[, 2], t = ct[, 3], p = ct[, 4]) |>
    filter(termino %in% c("log(hhi_clae2)", "log(empleo)")) |>
    mutate(n = nobs(m), r2 = summary(m)$r.squared, provincias = n_distinct(dep_ok$provincia))
}
tabla_modelos <- bind_rows(coef_cl(m_pool, "Todos los departamentos juntos"),
                           coef_cl(m_fe, "Con efectos fijos de provincia")) |>
  mutate(across(where(is.numeric), ~ signif(.x, 4)))
write_csv(tabla_modelos, "output/tablas/dep_regresion_coef.csv")
print(tabla_modelos)

resumen <- tibble(n_departamentos = nrow(dep_ok), rho_spearman = unname(rho_d$estimate), p_asintotico = rho_d$p.value)
write_csv(resumen |> mutate(across(where(is.numeric), ~ signif(.x, 4))), "output/tablas/dep_spearman.csv")
print(resumen)

# Descriptivas de las variables a nivel departamento
desc_dep <- dep_ok |>
  summarise(across(c(empleo, hhi_clae2, exportadoras_c100),
                   list(media = mean, mediana = median, desvio = sd, p25 = ~ quantile(.x, .25), p75 = ~ quantile(.x, .75)))) |>
  pivot_longer(everything(), names_to = c("variable", "medida"), names_pattern = "(.*)_(media|mediana|desvio|p25|p75)$") |>
  pivot_wider(names_from = medida, values_from = value) |>
  mutate(across(where(is.numeric), ~ signif(.x, 4)))
write_csv(desc_dep, "output/tablas/dep_descriptivas.csv")
print(desc_dep)

# ---- 2b. Sensibilidad al piso de empleo y al control por tamano -------------------------------------------
# El signo de la asociacion depende de si se controla por el tamano del departamento: los departamentos grandes
# son mas diversificados (menor HHI) y tambien tienen otra proporcion de exportadoras.
sens_umbral <- map_dfr(c(100, 500, 2000, 5000), function(u) {
  x <- dep |> filter(empleo >= u)
  rho <- cor(x$hhi_clae2, x$exportadoras_c100, method = "spearman")
  m_sin <- lm(exportadoras_c100 ~ log(hhi_clae2), data = x)
  m_con <- lm(exportadoras_c100 ~ log(hhi_clae2) + log(empleo), data = x)
  cl <- function(m) coeftest(m, vcov = vcovCL(m, cluster = x$provincia, type = "HC1"))["log(hhi_clae2)", c(1, 4)]
  tibble(piso_empleo = u, n_departamentos = nrow(x), rho_spearman = rho,
         rho_hhi_vs_empleo = cor(x$hhi_clae2, x$empleo, method = "spearman"),
         coef_sin_control = cl(m_sin)[1], p_sin_control = cl(m_sin)[2],
         coef_con_control = cl(m_con)[1], p_con_control = cl(m_con)[2])
}) |> mutate(across(where(is.numeric), ~ signif(.x, 3)))
write_csv(sens_umbral, "output/tablas/dep_sensibilidad_umbral.csv")
print(sens_umbral)

# ---- 3. Grafico -------------------------------------------------------------------------------------------
g_dep <- ggplot(dep_ok, aes(ramas_equivalentes, exportadoras_c100)) +
  geom_point(aes(colour = region), alpha = 0.6, size = 1.8) +
  geom_smooth(method = "loess", formula = y ~ x, se = FALSE, colour = "grey20", linewidth = 0.9) +
  scale_colour_manual(values = pal_regiones, name = "Región") +
  scale_x_log10(labels = label_number(decimal.mark = ",")) +
  scale_y_continuous(labels = label_number(decimal.mark = ",")) +
  coord_cartesian(ylim = c(0, quantile(dep_ok$exportadoras_c100, 0.99))) +
  guides(colour = guide_legend(override.aes = list(alpha = 1, size = 3))) +
  labs(title = "Entre departamentos no se ve la misma relación",
       subtitle = paste0("Departamentos con al menos 500 empleos (", nrow(dep_ok), "), 2022. ρ de Spearman = ",
                         sub("^-", "−", formatC(rho_d$estimate, format = "f", digits = 2, decimal.mark = ",")),
                         ".
Se omite el 1% de valores más altos del eje vertical. La línea es un suavizado local (loess)."),
       x = "Ramas equivalentes del empleo del departamento (N = 1 / HHI, escala logarítmica)",
       y = "Exportadoras cada
100 establecimientos", caption = fuente_cep) +
  tema_tp(base_size = 11)
ggsave("output/graficos/dep_fig_dispersion.png", g_dep, width = 7.5, height = 5, dpi = 200, bg = "white")

cat("\nScript 07 terminado.\n")
