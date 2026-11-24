# =============================================================================
# TP Ciencia de Datos - FCE UBA - Grupo 22
# ENTREGA FINAL - Script 05: metodo cuantitativo 2, cluster de provincias por estructura sectorial
#
# Flujo (clase 15 y 16): estandarizar -> PCA -> cluster jerarquico (Ward) sobre los componentes ->
# consolidacion con k-means (HCPC) -> caracterizar (perfiles en unidades originales, paragons) ->
# sensibilidad (otro k, otro ncp, otro algoritmo, otra representacion, sin CABA) ->
# ¿existen los grupos? (test contra datos sin grupos) -> validacion externa (variables NO usadas).
# Correr desde la carpeta del proyecto.
# =============================================================================

suppressMessages({
  library(tidyverse)
  library(FactoMineR)
  library(factoextra)
  library(cluster)
  library(mclust)
  library(MASS)
  library(ggrepel)
  library(patchwork)
})
select <- dplyr::select
options(scipen = 999)
set.seed(2026)
source("utils/tema_tp.R")

d22 <- read_csv("input/establecimientos.csv", show_col_types = FALSE) |> filter(anio == 2022)
aux <- read_csv("auxiliar/provincias_aux.csv", show_col_types = FALSE)
maestra <- read_csv("output/tablas/i2_tabla_maestra_provincias.csv", show_col_types = FALSE)

# =============================================================================
# 1. REPRESENTACION: participaciones del empleo por macro-sector
# =============================================================================
# Las letras chicas (menos de ~1,5% del empleo nacional) se agrupan con afines o en "Otros", y se unen
# sectores afines. Es la recomendacion del curso para categorias raras (clase 15, advertencias).
macro <- c(A = "Agro", B = "Minería", C = "Industria", D = "Energía y agua", E = "Energía y agua",
           F = "Construcción", G = "Comercio", H = "Transporte", I = "Alojamiento y gastro.",
           J = "Servicios modernos", K = "Servicios modernos", L = "Servicios modernos", M = "Servicios modernos",
           N = "Apoyo administrativo", O = "Otros", P = "Educación y salud", Q = "Educación y salud",
           R = "Otros", S = "Otros", T = "Otros", U = "Otros")
write_csv(tibble(letra = names(macro), macro_sector = unname(macro)), "output/tablas/i2_macro_sectores_mapa.csv")

shares_macro <- function(datos, agrupar = TRUE) {
  datos |> mutate(cat = if (agrupar) macro[letra] else letra) |>
    group_by(provincia, cat) |> summarise(e = sum(empleo), .groups = "drop") |>
    pivot_wider(names_from = cat, values_from = e, values_fill = 0) |>
    column_to_rownames("provincia") |> as.matrix()
}
Xm <- shares_macro(d22); Sh <- 100 * Xm / rowSums(Xm)
Xl <- shares_macro(d22, FALSE); Xl <- Xl[, !colnames(Xl) %in% c("T", "U")]; Shl <- 100 * Xl / rowSums(Xl)
rownames(Sh) <- rownames(Shl) <- aux$provincia_label[match(rownames(Sh), aux$provincia)]
Sh <- as.data.frame(Sh); Shl <- as.data.frame(Shl)
cat("Macro-sectores:", ncol(Sh), "| letras usadas en la variante:", ncol(Shl), "\n")

Z <- scale(Sh); dd <- dist(Z)

# =============================================================================
# 2. ¿CUANTOS GRUPOS? silueta, codo y gap (clase 15)
# =============================================================================
hc <- hclust(dd, "ward.D2")
ward_fun <- function(x, k) list(cluster = cutree(hclust(dist(x), "ward.D2"), k))
gap <- clusGap(Z, FUNcluster = ward_fun, K.max = 8, B = 200)
sel_k <- tibble(
  k = 2:8,
  silueta = map_dbl(2:8, ~ mean(silhouette(cutree(hc, .x), dd)[, 3])),
  r2 = map_dbl(2:8, function(k) { g <- cutree(hc, k); 1 - sum(map_dbl(split(seq_len(nrow(Z)), g), ~ sum(scale(Z[.x, , drop = FALSE], scale = FALSE)^2))) / sum(scale(Z, scale = FALSE)^2) }),
  gap = gap$Tab[2:8, "gap"],
  tamaños = map_chr(2:8, ~ paste(sort(table(cutree(hc, .x)), decreasing = TRUE), collapse = "/"))
)
write_csv(sel_k |> mutate(across(where(is.numeric), ~ round(.x, 3))), "output/tablas/i2_cluster_seleccion_k.csv")
print(sel_k)

g1 <- ggplot(sel_k, aes(k, silueta)) + geom_line(colour = pal_azul) + geom_point(colour = pal_azul, size = 2.5) +
  geom_hline(yintercept = 0.25, linetype = "dashed", colour = "grey50") +
  annotate("text", x = 8, y = 0.26, label = "0,25: estructura débil", hjust = 1, size = 3.2, colour = "grey40") +
  scale_x_continuous(breaks = 2:8) + labs(title = "Silueta promedio", x = "k", y = NULL) + tema_tp(11)
g2 <- ggplot(sel_k, aes(k, r2)) + geom_line(colour = pal_azul) + geom_point(colour = pal_azul, size = 2.5) +
  scale_x_continuous(breaks = 2:8) + scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(title = "Inercia entre grupos (R²)", x = "k", y = NULL) + tema_tp(11)
g3 <- ggplot(sel_k, aes(k, gap)) + geom_line(colour = pal_azul) + geom_point(colour = pal_azul, size = 2.5) +
  scale_x_continuous(breaks = 2:8) + labs(title = "Gap statistic", x = "k", y = NULL) + tema_tp(11)
ggsave("output/graficos/i2_fig_cluster_k.png", (g1 + g2 + g3) + plot_annotation(caption = fuente_cep, theme = theme(plot.caption = element_text(colour = "grey40", size = 8, hjust = 0))), width = 12, height = 4.2, dpi = 150)

# =============================================================================
# 3. HCPC: PCA (ncp = 5) -> Ward -> consolidacion k-means, k = 4
# =============================================================================
K <- 4
res_pca <- PCA(Sh, scale.unit = TRUE, ncp = 5, graph = FALSE)
eig <- as_tibble(res_pca$eig, rownames = "componente") |> head(6)
write_csv(eig |> mutate(across(where(is.numeric), ~ round(.x, 2))), "output/tablas/i2_pca_autovalores.csv")
print(eig)
res_hcpc <- HCPC(res_pca, nb.clust = K, consol = TRUE, graph = FALSE)
grupo <- res_hcpc$data.clust$clust
names(grupo) <- rownames(Sh)

# Nombres de los grupos a partir de quienes los integran (se verifica la composicion antes de nombrar)
miembros <- split(names(grupo), grupo)
nombrar <- function(m) {
  if ("CABA" %in% m) return("CABA: servicios modernos")
  if ("Santa Cruz" %in% m) return("Patagonia minera y petrolera")
  if ("Formosa" %in% m || "Chaco" %in% m) return("Comercio y construcción")
  "Intermedio: agro, industria y servicios"
}
nom_grupo <- map_chr(miembros, nombrar)
cat("Nombres asignados:\n"); print(nom_grupo)
stopifnot(!anyDuplicated(nom_grupo))
asign <- tibble(provincia_label = names(grupo), cluster = as.integer(grupo), grupo_nombre = nom_grupo[as.character(grupo)])

# Silueta por provincia y paragons (mas cercana al centro del grupo en el espacio de componentes)
sil <- silhouette(as.integer(grupo), dd)
asign$silueta <- sil[, "sil_width"]
coords <- res_hcpc$call$X[, seq_len(5)]
centros <- aggregate(coords, list(grupo = as.integer(grupo)), mean)
asign$dist_centro <- map_dbl(seq_len(nrow(asign)), function(i) sqrt(sum((as.numeric(coords[i, ]) - as.numeric(centros[centros$grupo == asign$cluster[i], -1]))^2)))
asign <- asign |> left_join(aux |> select(provincia_label, provincia, region), by = "provincia_label")
write_csv(asign, "output/tablas/i2_cluster_asignacion.csv")
print(asign |> arrange(cluster, dist_centro), n = 24)
cat("Silueta promedio de la particion:", round(mean(asign$silueta), 3), "\n")

paragons <- asign |> group_by(grupo_nombre) |> arrange(dist_centro, .by_group = TRUE) |>
  summarise(n = n(), paragon = first(provincia_label), miembros = paste(provincia_label, collapse = ", "),
            silueta_media = round(mean(silueta), 2), .groups = "drop")
write_csv(paragons, "output/tablas/i2_cluster_paragons.csv"); print(paragons)

# Perfil en unidades originales (% del empleo por macro-sector) + indicadores del resto del trabajo
perfil_macro <- Sh |> rownames_to_column("provincia_label") |> left_join(asign |> select(provincia_label, grupo_nombre), by = "provincia_label") |>
  group_by(grupo_nombre) |> summarise(across(where(is.numeric), ~ round(mean(.x), 1)), .groups = "drop")
write_csv(perfil_macro, "output/tablas/i2_cluster_perfil_macro.csv")
perfil_ind <- maestra |> left_join(asign |> select(provincia, grupo_nombre), by = "provincia") |>
  group_by(grupo_nombre) |>
  summarise(n = n(), neq_c2 = round(mean(neq_c2), 1), int_exp = round(mean(int_exp), 2),
            diversidad = round(median(diversidad)), ubicuidad_media = round(mean(ubicuidad_media), 1),
            empleo_total = sum(empleo), fundar_eci_mediana = median(fundar_eci), .groups = "drop")
write_csv(perfil_ind, "output/tablas/i2_cluster_perfil_indicadores.csv"); print(perfil_ind)

# Descripcion automatica de HCPC (v.test): que macro-sectores distinguen a cada grupo
desc <- map_dfr(names(res_hcpc$desc.var$quanti), function(g) {
  as_tibble(res_hcpc$desc.var$quanti[[g]], rownames = "variable") |> mutate(cluster = as.integer(g))
}) |> left_join(asign |> distinct(cluster, grupo_nombre), by = "cluster") |>
  select(grupo_nombre, variable, v.test, `Mean in category`, `Overall mean`, p.value) |>
  mutate(across(where(is.numeric), ~ round(.x, 3)))
write_csv(desc, "output/tablas/i2_cluster_vtest.csv")

# =============================================================================
# 4. GRAFICOS: mapa PCA, perfil por grupo, dendrograma
# =============================================================================
sc <- as_tibble(res_pca$ind$coord[, 1:2], rownames = "provincia_label") |> rename(PC1 = Dim.1, PC2 = Dim.2) |>
  left_join(asign |> select(provincia_label, grupo_nombre), by = "provincia_label")
hull <- sc |> group_by(grupo_nombre) |> filter(n() >= 3) |> slice(chull(PC1, PC2))
v1 <- round(res_pca$eig[1, 2]); v2 <- round(res_pca$eig[2, 2])
g_map <- ggplot(sc, aes(PC1, PC2, colour = grupo_nombre, fill = grupo_nombre)) +
  geom_polygon(data = hull, alpha = 0.12, colour = NA) +
  geom_point(aes(shape = grupo_nombre), size = 3.2) +
  geom_text_repel(aes(label = provincia_label), size = 3.3, show.legend = FALSE, max.overlaps = 40) +
  scale_colour_manual(values = pal_grupos, name = NULL) + scale_fill_manual(values = pal_grupos, name = NULL) +
  scale_shape_manual(values = forma_grupos, name = NULL) +
  labs(title = "Cuatro familias de provincias, con fronteras difusas",
       subtitle = paste0("HCPC con k = 4 sobre 12 macro-sectores estandarizados. PC1 (", v1, "%) y PC2 (", v2, "%)"),
       caption = fuente_cep,
       x = paste0("PC1 (", v1, "% de la varianza)"), y = paste0("PC2 (", v2, "% de la varianza)")) +
  guides(colour = guide_legend(nrow = 2)) + tema_tp()
ggsave("output/graficos/i2_fig_cluster_mapa.png", g_map, width = 9.5, height = 6.8, dpi = 150)

# Perfil: promedio de cada macro-sector (z-score) por grupo, con el % real en la celda
zprof <- as_tibble(Z, rownames = NULL) |> mutate(provincia_label = rownames(Sh)) |>
  left_join(asign |> select(provincia_label, grupo_nombre), by = "provincia_label") |>
  pivot_longer(-c(provincia_label, grupo_nombre), names_to = "macro", values_to = "z") |>
  group_by(grupo_nombre, macro) |> summarise(z = mean(z), .groups = "drop")
perf_largo <- perfil_macro |> pivot_longer(-grupo_nombre, names_to = "macro", values_to = "pct") |> left_join(zprof, by = c("grupo_nombre", "macro"))
orden_macro <- perfil_macro |> select(-grupo_nombre) |> colMeans() |> sort(decreasing = TRUE) |> names()
g_perf <- ggplot(perf_largo |> mutate(macro = factor(macro, levels = orden_macro)), aes(macro, grupo_nombre, fill = z)) +
  geom_tile(colour = "white") + geom_text(aes(label = formatC(pct, format = "f", digits = 1, decimal.mark = ",")), size = 3.3) +
  scale_fill_gradient2(low = "#3B6FB6", mid = "#F5F5F5", high = "#D9822B", midpoint = 0, name = "Desvíos\nvs. promedio") +
  labs(title = "Cada familia se distingue por uno o dos macro-sectores", subtitle = "% del empleo por macro-sector (promedio del grupo). Color: desvíos respecto del promedio de las 24 provincias", x = NULL, y = NULL, caption = fuente_cep) +
  tema_tp(11) + theme(axis.text.x = element_text(angle = 35, hjust = 1), panel.grid = element_blank(), legend.position = "right")
ggsave("output/graficos/i2_fig_cluster_perfil.png", g_perf, width = 11, height = 4.3, dpi = 150)

gden <- fviz_dend(hclust(dd, "ward.D2"), k = K, cex = 0.7, horiz = TRUE, rect = FALSE, k_colors = rep("grey30", K), main = "Dendrograma (Ward sin consolidar, 24 provincias)")
ggsave("output/graficos/i2_fig_cluster_dendrograma.png", gden, width = 8, height = 6.5, dpi = 150)

# =============================================================================
# 5. SENSIBILIDAD: ¿cambia la particion si cambian las decisiones?
# =============================================================================
base <- as.integer(grupo)
hcpc_var <- function(X, ncp, k) {
  p <- PCA(X, scale.unit = TRUE, ncp = ncp, graph = FALSE)
  as.integer(HCPC(p, nb.clust = k, consol = TRUE, graph = FALSE)$data.clust$clust)
}
vars <- list(
  "ncp = 3" = hcpc_var(Sh, 3, 4),
  "ncp = 11 (todos los componentes)" = hcpc_var(Sh, 11, 4),
  "k = 3" = hcpc_var(Sh, 5, 3),
  "k = 5" = hcpc_var(Sh, 5, 5),
  "Ward sin consolidar" = cutree(hc, 4),
  "Linkage completo" = cutree(hclust(dd, "complete"), 4),
  "k-means (50 arranques)" = kmeans(Z, 4, nstart = 50)$cluster,
  "19 letras en vez de 12 macro" = hcpc_var(Shl, 5, 4)
)
sens <- tibble(variante = names(vars), ARI = map_dbl(vars, ~ adjustedRandIndex(base, .x)),
               tamaños = map_chr(vars, ~ paste(sort(table(.x), decreasing = TRUE), collapse = "/")))
# Sin CABA: se compara contra la particion base restringida a las otras 23
sinc <- rownames(Sh) != "CABA"
g_sinc <- hcpc_var(Sh[sinc, ], 5, 4)
sens <- bind_rows(sens, tibble(variante = "Sin CABA (23 provincias)", ARI = adjustedRandIndex(base[sinc], g_sinc),
                               tamaños = paste(sort(table(g_sinc), decreasing = TRUE), collapse = "/")))
sens <- sens |> mutate(ARI = round(ARI, 2))
write_csv(sens, "output/tablas/i2_cluster_sensibilidad.csv"); print(sens)
cat("Miembros sin CABA:\n"); print(split(rownames(Sh)[sinc], g_sinc))

# Cuantas provincias mantienen siempre al mismo companero? (estabilidad de pares)
M_co <- function(g) outer(g, g, "==") * 1
co <- Reduce(`+`, lapply(vars[c("ncp = 3", "ncp = 11 (todos los componentes)", "Ward sin consolidar", "Linkage completo", "k-means (50 arranques)", "19 letras en vez de 12 macro")], M_co)) / 6
dimnames(co) <- list(rownames(Sh), rownames(Sh))
est_prov <- tibble(provincia_label = rownames(Sh),
                   coincidencia = map_dbl(seq_len(nrow(Sh)), function(i) { mismos <- which(base == base[i] & seq_len(nrow(Sh)) != i)
                     if (length(mismos) == 0) NA_real_ else mean(co[i, mismos]) }))
write_csv(est_prov |> mutate(coincidencia = round(coincidencia, 2)), "output/tablas/i2_cluster_estabilidad_provincias.csv")

Zl <- scale(Shl); ddl <- dist(Zl)
sil19 <- tibble(variante = c("12 macro-sectores", "19 letras"),
                silueta_ward_k4 = c(mean(silhouette(cutree(hc, 4), dd)[, 3]), mean(silhouette(cutree(hclust(ddl, "ward.D2"), 4), ddl)[, 3])))
write_csv(sil19 |> mutate(silueta_ward_k4 = round(silueta_ward_k4, 3)), "output/tablas/i2_cluster_silueta_12_vs_19.csv"); print(sil19)

# =============================================================================
# 6. ¿LOS GRUPOS EXISTEN? silueta observada vs datos simulados sin grupos (clase 15, lamina 27b)
# =============================================================================
sil_obs <- mean(silhouette(kmeans(Z, K, nstart = 25)$cluster, dd)[, 3])
mu <- colMeans(Sh); S <- cov(Sh)
sims <- replicate(500, { Y <- scale(mvrnorm(nrow(Sh), mu, S)); mean(silhouette(kmeans(Y, K, nstart = 10)$cluster, dist(Y))[, 3]) })
null_res <- tibble(silueta_observada = sil_obs, silueta_simulada_media = mean(sims), p_sim = mean(sims >= sil_obs), n_sim = length(sims))
write_csv(null_res |> mutate(across(where(is.numeric), ~ round(.x, 3))), "output/tablas/i2_cluster_test_sin_grupos.csv"); print(null_res)

# =============================================================================
# 7. VALIDACION EXTERNA: variables que NO se usaron para armar los grupos
# =============================================================================
ext <- asign |> left_join(maestra |> select(provincia, int_exp, fundar_eci, hhi_c2, diversidad), by = "provincia")
epsilon2 <- function(h, n) h / (n - 1)
kw_fun <- function(v) { t <- kruskal.test(ext[[v]], factor(ext$grupo_nombre)); tibble(variable = v, H = unname(t$statistic), gl = unname(t$parameter), p = t$p.value, epsilon2 = epsilon2(unname(t$statistic), nrow(ext))) }
kw <- map_dfr(c("int_exp", "fundar_eci"), kw_fun)
# Chi-cuadrado grupo x region: esperados muy chicos -> p por simulacion (como indica la clase 9)
tab <- table(ext$grupo_nombre, ext$region)
ch <- suppressWarnings(chisq.test(tab, simulate.p.value = TRUE, B = 20000))
esp_min <- min(suppressWarnings(chisq.test(tab))$expected); pct_esp5 <- mean(suppressWarnings(chisq.test(tab))$expected >= 5) * 100
V <- sqrt(unname(ch$statistic) / (sum(tab) * (min(dim(tab)) - 1)))
chi <- tibble(variable = "region (chi-cuadrado)", H = unname(ch$statistic), gl = unname(ch$parameter), p = ch$p.value, epsilon2 = V)
val <- bind_rows(kw, chi) |> mutate(p_holm = p.adjust(p, "holm"))
write_csv(val |> mutate(across(where(is.numeric), ~ round(.x, 4))), "output/tablas/i2_cluster_validacion_externa.csv"); print(val)
cat("Esperado minimo chi2:", round(esp_min, 2), "| % celdas con esperado >= 5:", round(pct_esp5, 0), "\n")
write_csv(as.data.frame.matrix(tab) |> rownames_to_column("grupo"), "output/tablas/i2_cluster_x_region.csv"); print(tab)

# ¿Cuanto depende el resultado de la particion elegida? Kruskal-Wallis de int_exp con cada variante
kw_p <- function(g, idx = rep(TRUE, nrow(ext))) { t <- kruskal.test(ext$int_exp[idx], factor(g)); c(H = unname(t$statistic), p = t$p.value) }
kw_var <- bind_rows(
  tibble(particion = "Base (HCPC, k = 4)", H = kw_p(base)["H"], p = kw_p(base)["p"]),
  imap_dfr(vars, ~ tibble(particion = .y, H = kw_p(.x)["H"], p = kw_p(.x)["p"])),
  tibble(particion = "Sin CABA (23 provincias)", H = kw_p(g_sinc, sinc)["H"], p = kw_p(g_sinc, sinc)["p"])
) |> mutate(across(where(is.numeric), ~ round(.x, 3)))
write_csv(kw_var, "output/tablas/i2_cluster_kw_segun_particion.csv"); print(kw_var)
cat("\nScript 05 terminado.\n")
