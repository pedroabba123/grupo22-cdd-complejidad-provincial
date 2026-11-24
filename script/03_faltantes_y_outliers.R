# =============================================================================
# TP Ciencia de Datos - FCE UBA - Grupo 22
# ENTREGA FINAL - Script 03: faltantes, outliers y descriptivas despues de la limpieza
#
# Objetivo: diagnosticar y decidir que hacer con (1) valores faltantes y (2) valores
# atipicos, y medir el impacto de cada decision (checklist de la clase 13).
# Correr desde la raiz del proyecto (abrir grupo22_cdd_tp.Rproj o ejecutar desde esa carpeta).
# =============================================================================

suppressMessages({
  library(tidyverse)
  library(scales)
})
options(scipen = 999)
set.seed(2026)

source("utils/calcular_hhi.R")
source("utils/winsorizar.R")
source("utils/tema_tp.R")

d   <- read_csv("input/establecimientos.csv", show_col_types = FALSE)
aux <- read_csv("auxiliar/provincias_aux.csv", show_col_types = FALSE)
d22 <- d |> filter(anio == 2022)

dir.create("output/tablas", showWarnings = FALSE, recursive = TRUE)
dir.create("output/graficos", showWarnings = FALSE, recursive = TRUE)

# =============================================================================
# 1. FALTANTES
# =============================================================================

# 1.1 Faltantes explicitos (NA) y valores imposibles
explicitos <- tibble(
  variable = names(d),
  n_na = colSums(is.na(d)),
  pct_na = round(100 * colMeans(is.na(d)), 2)
)
write_csv(explicitos, "output/tablas/i2_faltantes_explicitos.csv")

chequeos <- tibble(
  chequeo = c("Filas con empleo <= 0", "Filas con establecimientos <= 0",
              "Filas con exportadoras > establecimientos", "Filas con empleo < establecimientos"),
  n = c(sum(d$empleo <= 0), sum(d$establecimientos <= 0),
        sum(d$empresas_exportadoras > d$establecimientos), sum(d$empleo < d$establecimientos))
)
write_csv(chequeos, "output/tablas/i2_chequeos_consistencia.csv")

# 1.2 Faltantes "escondidos": celdas (departamento x actividad) que no existen en uno de los anios.
# Una celda ausente significa "no hay establecimientos": es un cero, no un NA.
celdas <- d |>
  group_by(in_departamentos, clae6) |>
  summarise(n_anios = n(), anio_unico = first(anio), empleo = sum(empleo), .groups = "drop")

tot_anio <- d |> group_by(anio) |> summarise(empleo_total = sum(empleo))
celdas_un_anio <- celdas |>
  filter(n_anios == 1) |>
  group_by(anio = anio_unico) |>
  summarise(celdas = n(), empleo = sum(empleo), .groups = "drop") |>
  left_join(tot_anio, by = "anio") |>
  mutate(pct_empleo = round(100 * empleo / empleo_total, 2))
write_csv(celdas_un_anio, "output/tablas/i2_celdas_un_solo_anio.csv")
n_celdas_total <- nrow(celdas)

# 1.3 Cobertura: departamentos con poco empleo (los indices de concentracion a ese nivel serian ruido)
deptos <- d22 |>
  group_by(provincia, in_departamentos, departamento) |>
  summarise(empleo = sum(empleo), establecimientos = sum(establecimientos),
            exportadoras = sum(empresas_exportadoras), .groups = "drop")
deptos_chicos <- tibble(
  umbral = c(50, 100, 500, 1000),
  n_deptos = map_int(umbral, ~ sum(deptos$empleo < .x)),
  pct = round(100 * n_deptos / nrow(deptos), 1)
)
write_csv(deptos_chicos, "output/tablas/i2_deptos_poco_empleo.csv")

# 1.4 Provincia mas chica (nivel de analisis elegido): ¿tiene suficiente empleo?
prov <- d22 |>
  group_by(provincia) |>
  summarise(empleo = sum(empleo), establecimientos = sum(establecimientos),
            exportadoras = sum(empresas_exportadoras), .groups = "drop") |>
  mutate(int_exp = 100 * exportadoras / establecimientos)
cat("Provincia con menos empleo:", prov$provincia[which.min(prov$empleo)], min(prov$empleo), "\n")

# 1.5 Sectores sin empleo en alguna provincia (ceros estructurales a rellenar con 0)
ceros_letra <- d22 |>
  group_by(provincia, letra) |> summarise(e = sum(empleo), .groups = "drop") |>
  complete(provincia, letra, fill = list(e = 0)) |>
  filter(e == 0) |> count(letra, name = "n_provincias_sin_empleo")
write_csv(ceros_letra, "output/tablas/i2_ceros_estructurales_letra.csv")

# 1.6 Participacion de la administracion publica (letra O): evidencia de que la base es de empleo privado registrado
sh_o <- d22 |> group_by(letra) |> summarise(e = sum(empleo)) |> mutate(pct = 100 * e / sum(e)) |> filter(letra == "O")
cat("Letra O (adm. publica) =", round(sh_o$pct, 2), "% del empleo\n")

# =============================================================================
# 2. OUTLIERS
# =============================================================================

# 2.1 Variable 1 (nivel celda): empleo por establecimiento. Distribucion con cola derecha muy larga.
d22 <- d22 |> mutate(ratio = empleo / establecimientos)
d  <- d  |> mutate(ratio = empleo / establecimientos)

iqr_lim <- function(x, log_scale = FALSE) {
  v <- if (log_scale) log(x) else x
  q <- quantile(v, c(.25, .75)); i <- q[2] - q[1]
  lim <- c(q[1] - 1.5 * i, q[2] + 1.5 * i)
  if (log_scale) exp(lim) else lim
}
lim_orig <- iqr_lim(d22$ratio)
lim_log  <- iqr_lim(d22$ratio, TRUE)

z_rob <- (log(d22$ratio) - median(log(d22$ratio))) / mad(log(d22$ratio))
z_cla <- (d22$ratio - mean(d22$ratio)) / sd(d22$ratio)

out_celda <- tibble(
  criterio = c("IQR, escala original", "IQR, escala logaritmica", "z-score clasico (|z|>3), escala original",
               "z robusto (mediana/MAD, |z|>3), escala log"),
  limite_superior = c(lim_orig[2], lim_log[2], mean(d22$ratio) + 3 * sd(d22$ratio),
                      exp(median(log(d22$ratio)) + 3 * mad(log(d22$ratio)))),
  n_marcados = c(sum(d22$ratio > lim_orig[2]), sum(d22$ratio > lim_log[2]), sum(abs(z_cla) > 3), sum(abs(z_rob) > 3)),
  n_total = nrow(d22)
) |> mutate(pct = round(100 * n_marcados / n_total, 1))
write_csv(out_celda, "output/tablas/i2_outliers_celda_criterios.csv")
print(out_celda)

top_celdas <- d22 |>
  filter(ratio > lim_log[2]) |>
  arrange(desc(ratio)) |>
  select(provincia, departamento, letra, clae6, empleo, establecimientos, ratio) |>
  head(15)
write_csv(top_celdas, "output/tablas/i2_outliers_top_celdas.csv")
print(top_celdas)

# Letras donde se concentran los marcados con IQR log
marc_letra <- d22 |> filter(ratio > lim_log[2]) |> count(letra, sort = TRUE) |> mutate(pct = round(100 * n / sum(n), 1))
write_csv(marc_letra, "output/tablas/i2_outliers_por_letra.csv")
cat("Empleo en celdas marcadas (IQR log) como % del total:",
    round(100 * sum(d22$empleo[d22$ratio > lim_log[2]]) / sum(d22$empleo), 1), "\n")

g_ratio <- ggplot(d22, aes(ratio)) +
  geom_histogram(bins = 50, fill = pal_azul, colour = "white") +
  scale_x_log10(labels = label_number(big.mark = ".", decimal.mark = ",")) +
  geom_vline(xintercept = lim_log[2], colour = pal_naranja, linewidth = 1) +
  geom_vline(xintercept = lim_orig[2], colour = "grey30", linetype = "dashed", linewidth = 1) +
  annotate("text", x = lim_log[2] * 1.12, y = Inf, vjust = 1.5, hjust = 0, colour = pal_naranja, size = 3.6, lineheight = 0.95,
           label = paste0("IQR en log: ", round(lim_log[2]), "
", format(out_celda$n_marcados[2], big.mark = ".", decimal.mark = ","), " celdas")) +
  annotate("text", x = lim_orig[2] * 1.1, y = Inf, vjust = 1.5, hjust = 0, colour = "grey30", size = 3.6, lineheight = 0.95,
           label = paste0("IQR en escala
original: ", round(lim_orig[2]), "
", format(out_celda$n_marcados[1], big.mark = ".", decimal.mark = ","), " celdas")) +
  labs(title = "Según la regla, entre 0,6% y 11% de las celdas resultan atípicas",
       subtitle = "Empleo por establecimiento de cada celda departamento-actividad (escala logarítmica)",
       caption = fuente_cep,
       x = "Empleados por establecimiento (escala log)", y = "Cantidad de celdas") +
  tema_tp()
ggsave("output/graficos/i2_fig_outliers_celda.png", g_ratio, width = 9, height = 5, dpi = 150)

# 2.2 Variables a nivel provincia (las que usan los metodos de la entrega): int_exp y HHI(clae2)
hhi_c2 <- d22 |> group_by(provincia, clae2) |> summarise(e = sum(empleo), .groups = "drop") |>
  group_by(provincia) |> summarise(hhi_c2 = calcular_hhi(e), .groups = "drop")
P <- prov |> left_join(hhi_c2, by = "provincia")

atip_prov <- map_dfr(c("int_exp", "hhi_c2", "empleo"), function(v) {
  x <- P[[v]]; lg <- v == "empleo"
  xv <- if (lg) log(x) else x
  q <- quantile(xv, c(.25, .75)); i <- q[2] - q[1]
  lim <- c(q[1] - 1.5 * i, q[2] + 1.5 * i)
  fuera <- which(xv < lim[1] | xv > lim[2])
  tibble(variable = v, escala = ifelse(lg, "log", "original"),
         n_marcadas = length(fuera),
         provincias = paste(P$provincia[fuera], collapse = ", "))
})
write_csv(atip_prov, "output/tablas/i2_outliers_provincias.csv")
print(atip_prov)

# 2.3 Impacto de la decision sobre el HHI: winsorizar el empleo de las celdas al p99
p99 <- quantile(d22$empleo, 0.99)
d22 <- d22 |> mutate(empleo_w = winsorizar(empleo, 0, 0.99))
hhi_w <- d22 |> group_by(provincia, clae2) |> summarise(e = sum(empleo_w), .groups = "drop") |>
  group_by(provincia) |> summarise(hhi_c2_w = calcular_hhi(e), .groups = "drop")
impacto <- P |> select(provincia, hhi_c2) |>
  left_join(hhi_w, by = "provincia") |>
  mutate(rank_orig = rank(-hhi_c2), rank_w = rank(-hhi_c2_w), cambio_rank = rank_w - rank_orig,
         dif_pct = round(100 * (hhi_c2_w / hhi_c2 - 1), 1)) |>
  arrange(desc(abs(cambio_rank)))
write_csv(impacto, "output/tablas/i2_impacto_winsor_hhi.csv")
print(impacto, n = 24)
cat("p99 del empleo por celda:", p99, " | Spearman HHI(clae2) original vs winsorizado:",
    round(cor(impacto$hhi_c2, impacto$hhi_c2_w, method = "spearman"), 3), "\n")

# 2.4 Tabla "antes vs despues" para la variable ratio (como la lamina 53 de la clase 13)
resumen_var <- function(x, nombre) tibble(version = nombre, n = length(x), media = mean(x), mediana = median(x),
                                          desvio = sd(x), p5 = quantile(x, .05), p95 = quantile(x, .95))
antes_despues <- bind_rows(
  resumen_var(d22$ratio, "Original"),
  resumen_var(winsorizar(d22$ratio, 0, 0.99), "Winsorizado p99"),
  resumen_var(d22$ratio[d22$ratio <= lim_log[2]], "Sin atipicos (IQR en log)"),
  resumen_var(d22$ratio[d22$ratio <= lim_orig[2]], "Sin atipicos (IQR original)")
) |> mutate(across(where(is.numeric), ~ round(.x, 2)))
write_csv(antes_despues, "output/tablas/i2_antes_despues_ratio.csv")
print(antes_despues)

# 2.5 Estabilidad entre anios: ¿el HHI(clae2) de 2021 ordena igual que el de 2022?
hhi_21 <- d |> filter(anio == 2021) |> group_by(provincia, clae2) |> summarise(e = sum(empleo), .groups = "drop") |>
  group_by(provincia) |> summarise(hhi_c2_2021 = calcular_hhi(e), .groups = "drop")
est <- P |> select(provincia, hhi_c2) |> left_join(hhi_21, by = "provincia")
cat("Spearman HHI(clae2) 2021 vs 2022:", round(cor(est$hhi_c2, est$hhi_c2_2021, method = "spearman"), 3), "\n")
write_csv(tibble(medida = "Spearman HHI clae2 2021 vs 2022", valor = cor(est$hhi_c2, est$hhi_c2_2021, method = "spearman")),
          "output/tablas/i2_estabilidad_anios.csv")

# =============================================================================
# 3. DESCRIPTIVAS DESPUES DEL TRATAMIENTO: efecto de la decision sobre los datos
# =============================================================================
# Decision tomada: mantener los atipicos (son grandes empleadores reales) y trabajar en logaritmos.
# Aca se muestra que habria pasado con las otras dos opciones, a nivel celda y a nivel provincia.

# 3.1 Nivel celda: empleo por establecimiento en tres versiones de la base
versiones <- bind_rows(
  tibble(version = "Original", ratio = d22$ratio),
  tibble(version = "Winsorizado al p99", ratio = winsorizar(d22$ratio, 0, 0.99)),
  tibble(version = "Sin atípicos (IQR en log)", ratio = d22$ratio[d22$ratio <= lim_log[2]])
) |> mutate(version = factor(version, levels = unique(version)))

g_ad <- ggplot(versiones, aes(version, ratio, fill = version)) +
  geom_boxplot(outlier.size = 0.4, outlier.alpha = 0.25, linewidth = 0.4, width = 0.6, show.legend = FALSE) +
  stat_summary(fun = mean, geom = "point", shape = 23, size = 3.2, fill = "white", colour = "grey15", stroke = 0.8) +
  scale_fill_manual(values = c("Original" = pal_azul, "Winsorizado al p99" = "grey70", "Sin atípicos (IQR en log)" = "grey70")) +
  scale_y_log10(labels = label_number(big.mark = ".", decimal.mark = ",")) +
  labs(title = "La mediana casi no cambia con la limpieza, la media sí",
       subtitle = "Empleo por establecimiento de cada celda departamento × actividad, 2022.
Escala logarítmica; el rombo blanco es la media.",
       x = NULL, y = "Empleo por establecimiento
(personas, escala log)", caption = fuente_cep) +
  tema_tp(base_size = 11)
ggsave("output/graficos/post_fig_antes_despues_celdas.png", g_ad, width = 7, height = 4.3, dpi = 200, bg = "white")

# 3.2 Nivel provincia: HHI del empleo por rama (clae2) con y sin winsorizar
aux_nombres <- read_csv("auxiliar/provincias_aux.csv", show_col_types = FALSE) |> select(provincia, provincia_label)
post_hhi <- impacto |>
  left_join(aux_nombres, by = "provincia") |>
  mutate(nombre = fct_reorder(provincia_label, hhi_c2),
         destacada = provincia_label == "Tierra del Fuego")
write_csv(post_hhi |> select(provincia_label, hhi_c2, hhi_c2_w, rank_orig, rank_w, dif_pct),
          "output/tablas/post_hhi_provincias_original_vs_winsor.csv")

g_dum <- ggplot(post_hhi, aes(y = nombre)) +
  geom_segment(aes(x = hhi_c2, xend = hhi_c2_w, yend = nombre, colour = destacada), linewidth = 0.9, show.legend = FALSE) +
  geom_point(aes(x = hhi_c2, colour = destacada), size = 2.6, show.legend = FALSE) +
  geom_point(aes(x = hhi_c2_w), shape = 21, fill = "white", colour = "grey30", size = 2.6) +
  scale_colour_manual(values = c("FALSE" = "grey60", "TRUE" = pal_naranja)) +
  scale_x_continuous(labels = label_number(accuracy = 0.01, decimal.mark = ",")) +
  annotate("text", x = 0.0868, y = "Santa Cruz", label = "En naranja, Tierra del Fuego:
el HHI cae 38% y pasa
del puesto 3 al 17", hjust = 1,
           colour = pal_naranja, fontface = "bold", size = 3.6, lineheight = 1) +
  labs(title = "Winsorizar recortaría justo lo que concentra a las provincias chicas",
       subtitle = "HHI del empleo por rama (clae2), 2022: datos originales (punto lleno)
y winsorizados al p99 (círculo vacío)",
       x = "HHI del empleo por rama (mayor valor = empleo más concentrado)", y = NULL, caption = fuente_cep) +
  tema_tp(base_size = 11) + theme(panel.grid.major.y = element_blank())
ggsave("output/graficos/post_fig_hhi_original_vs_winsor.png", g_dum, width = 7, height = 5.5, dpi = 200, bg = "white")

# Resumen para la PPT
resumen_calidad <- tibble(
  indicador = c("Filas", "Variables", "NA explicitos", "Celdas depto-actividad distintas", "Celdas presentes en un solo anio",
                "Departamentos", "Departamentos con empleo < 500 (2022)", "Provincia mas chica: empleo",
                "Letra O (adm. publica), % empleo"),
  valor = c(nrow(d), ncol(d) - 1, sum(is.na(d)), n_celdas_total, sum(celdas$n_anios == 1),
            nrow(deptos), sum(deptos$empleo < 500), min(prov$empleo), round(sh_o$pct, 2))
)
write_csv(resumen_calidad, "output/tablas/i2_resumen_calidad.csv")
print(resumen_calidad)
cat("\nScript 03 terminado.\n")
