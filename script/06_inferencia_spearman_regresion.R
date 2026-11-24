# =============================================================================
# TP Ciencia de Datos - FCE UBA - Grupo 22
# ENTREGA FINAL - Script 06: metodo de inferencia estadistica
#
# Pregunta (surgida en la Instancia 1): ¿se asocia la concentracion sectorial de una provincia con su
# orientacion exportadora?
#   H0: no hay asociacion monotona entre concentracion sectorial (HHI del empleo) e intensidad
#       exportadora (empresas exportadoras cada 100 establecimientos) entre provincias (rho = 0).
#   H1: si hay asociacion (rho != 0).
#
# DECISIONES Y SU JUSTIFICACION (clase 9: elegir el test por la pregunta y los datos, no por el p-valor)
#   - Indice de concentracion principal: HHI del empleo a nivel clae2 (87 ramas). Se elige por construccion:
#     "letra" (21 sectores) esconde la diversificacion dentro de la industria, y "clae6" (945 actividades) queda
#     dominado por el ruido en provincias chicas (Formosa tiene ~27 mil empleos repartidos en 863 celdas).
#   - Test principal: correlacion de Spearman (n = 24, variables asimetricas, Mendoza atipica) + regresion lineal con
#     errores robustos como complemento. Se calcularon los tres niveles de desagregacion y se reportan todos:
#     el principal se justifica por construccion y los otros se muestran como sensibilidad, con correccion de Holm.
# Correr desde la carpeta del proyecto.
# =============================================================================

suppressMessages({
  library(tidyverse)
  library(sandwich)
  library(lmtest)
  library(car)
  library(ggrepel)
  library(patchwork)
})
options(scipen = 999)
set.seed(2026)
source("utils/tema_tp.R")
source("utils/calcular_hhi.R")

M <- read_csv("output/tablas/i2_tabla_maestra_provincias.csv", show_col_types = FALSE)
cl <- read_csv("output/tablas/i2_cluster_asignacion.csv", show_col_types = FALSE) |> select(provincia, grupo_nombre)
M <- M |> left_join(cl, by = "provincia")
n <- nrow(M)

# Funciones del test (sin dependencias externas)
spearman <- function(x, y) suppressWarnings(cor(x, y, method = "spearman"))
perm_test <- function(x, y, B = 10000) {
  obs <- spearman(x, y); rx <- rank(x); ry <- rank(y)
  sims <- replicate(B, cor(rx, sample(ry)))
  list(rho = obs, p = (1 + sum(abs(sims) >= abs(obs))) / (B + 1), sims = sims)
}
boot_ci <- function(x, y, B = 5000) {
  b <- replicate(B, { i <- sample(length(x), replace = TRUE); spearman(x[i], y[i]) })
  quantile(b, c(.025, .975), na.rm = TRUE)
}

# =============================================================================
# 1. TEST PRINCIPAL: Spearman entre HHI(clae2) e intensidad exportadora
# =============================================================================
pt <- perm_test(M$hhi_c2, M$int_exp, B = 10000)
ct <- cor.test(M$hhi_c2, M$int_exp, method = "spearman", exact = FALSE)
ci <- boot_ci(M$hhi_c2, M$int_exp)
principal <- tibble(
  n = n, rho = pt$rho, S = unname(ct$statistic), p_asintotico = ct$p.value, p_permutacion = pt$p,
  ic95_inf = ci[1], ic95_sup = ci[2], rho_cuadrado = pt$rho^2
)
write_csv(principal |> mutate(across(where(is.numeric), ~ signif(.x, 4))), "output/tablas/i2_inferencia_spearman_principal.csv")
print(principal)

g_perm <- ggplot(tibble(r = pt$sims), aes(r)) +
  geom_histogram(bins = 45, fill = "grey75", colour = "white") +
  geom_vline(xintercept = pt$rho, colour = pal_naranja, linewidth = 1.2) +
  geom_vline(xintercept = -pt$rho, colour = pal_naranja, linewidth = 1.2, linetype = "dashed") +
  annotate("text", x = pt$rho, y = Inf, vjust = 1.5, hjust = -0.1, colour = pal_naranja, size = 3.6,
           label = paste0("ρ observado = ", format(round(pt$rho, 2), decimal.mark = ","))) +
  labs(title = "Un ρ de −0,62 es muy poco probable si no hubiera asociación",
       subtitle = "10.000 permutaciones de la intensidad exportadora entre provincias",
       caption = fuente_cep,
       x = "ρ de Spearman", y = "Cantidad de permutaciones") + tema_tp()
ggsave("output/graficos/i2_fig_inf_permutacion.png", g_perm, width = 8, height = 4.8, dpi = 150)

# =============================================================================
# 2. REGRESION: int_exp ~ log(HHI clae2) + log(empleo)   (errores robustos HC1, como en la clase 18)
# =============================================================================
m  <- lm(int_exp ~ log(hhi_c2) + log(empleo), data = M)
m0 <- lm(int_exp ~ log(hhi_c2), data = M)
rob <- coeftest(m, vcov = vcovHC(m, type = "HC1"))
ci_rob <- coefci(m, vcov. = vcovHC(m, type = "HC1"))
coefs <- tibble(termino = rownames(rob), coef = rob[, 1], ee_robusto = rob[, 2], t = rob[, 3], p = rob[, 4],
                ic95_inf = ci_rob[, 1], ic95_sup = ci_rob[, 2], ee_clasico = sqrt(diag(vcov(m))))
write_csv(coefs |> mutate(across(where(is.numeric), ~ signif(.x, 4))), "output/tablas/i2_inferencia_regresion_coef.csv"); print(coefs)

r2_full <- summary(m)$r.squared; r2_sin <- summary(update(m, . ~ . - log(hhi_c2)))$r.squared
reg_res <- tibble(R2 = r2_full, R2_ajustado = summary(m)$adj.r.squared, R2_aporte_hhi = r2_full - r2_sin,
                  R2_solo_hhi = summary(m0)$r.squared, n = nobs(m))
write_csv(reg_res |> mutate(across(where(is.numeric), ~ signif(.x, 3))), "output/tablas/i2_inferencia_regresion_ajuste.csv"); print(reg_res)

# Magnitud: pasar del percentil 25 al 75 del HHI (a igual empleo)
q <- quantile(M$hhi_c2, c(.25, .75))
efecto <- tibble(hhi_p25 = q[1], hhi_p75 = q[2], dif_log = log(q[2]) - log(q[1]),
                 efecto_pp = coefs$coef[2] * (log(q[2]) - log(q[1])),
                 efecto_ic_inf = coefs$ic95_inf[2] * (log(q[2]) - log(q[1])), efecto_ic_sup = coefs$ic95_sup[2] * (log(q[2]) - log(q[1])),
                 sd_int_exp = sd(M$int_exp), mediana_int_exp = median(M$int_exp))
write_csv(efecto |> mutate(across(where(is.numeric), ~ signif(.x, 4))), "output/tablas/i2_inferencia_efecto_p25_p75.csv"); print(efecto)

# --- Diagnostico de supuestos ---
vf <- vif(m)
bp <- bptest(m); rs <- resettest(m); sw <- shapiro.test(residuals(m))
ck <- cooks.distance(m); umbral <- 4 / n; infl <- M$provincia_label[ck > umbral]
supuestos <- tibble(
  supuesto = c("Forma funcional (RESET)", "Varianza constante (Breusch-Pagan)", "Normalidad de residuos (Shapiro-Wilk)",
               "Multicolinealidad (VIF máx.)", "Observaciones influyentes (Cook > 4/n)"),
  estadistico = c(unname(rs$statistic), unname(bp$statistic), unname(sw$statistic), max(vf), max(ck)),
  p_valor = c(rs$p.value, bp$p.value, sw$p.value, NA, NA),
  lectura = c("No rechaza: no se detecta mala forma funcional", "No rechaza: no hay heteroscedasticidad detectable",
              "No rechaza (n = 24: poca potencia)", "VIF < 10: sin problema", paste("Marca:", paste(infl, collapse = ", ")))
)
write_csv(supuestos |> mutate(across(where(is.numeric), ~ signif(.x, 3))), "output/tablas/i2_inferencia_supuestos.csv"); print(supuestos)
cook_tab <- tibble(provincia_label = M$provincia_label, cook = ck) |> arrange(desc(cook))
write_csv(cook_tab |> mutate(cook = round(cook, 3)), "output/tablas/i2_inferencia_cook.csv"); print(head(cook_tab, 5))

# Con y sin influyentes (como pide la clase 18: "se reporta el modelo con y sin ellas")
ajuste_sin <- function(excluir, etiqueta) {
  mm <- update(m, data = M |> filter(!provincia_label %in% excluir))
  r <- coeftest(mm, vcov = vcovHC(mm, type = "HC1"))
  tibble(modelo = etiqueta, n = nobs(mm), coef_hhi = r[2, 1], ee_robusto = r[2, 2], p = r[2, 4],
         rho = spearman(M$hhi_c2[!M$provincia_label %in% excluir], M$int_exp[!M$provincia_label %in% excluir]))
}
con_sin <- bind_rows(ajuste_sin(character(0), "Todas las provincias"),
                     ajuste_sin("Mendoza", "Sin Mendoza (atípica en int. exportadora)"),
                     ajuste_sin(infl, paste("Sin influyentes:", paste(infl, collapse = ", "))),
                     ajuste_sin("CABA", "Sin CABA (atípica en estructura)"),
                     ajuste_sin(c("Buenos Aires", "CABA"), "Sin Buenos Aires ni CABA (atípicas en tamaño)"))
write_csv(con_sin |> mutate(across(where(is.numeric), ~ signif(.x, 4))), "output/tablas/i2_inferencia_con_sin_atipicos.csv"); print(con_sin)

# Robustez de la variable resultado: intensidad exportadora dentro de los sectores transables (A, B, C)
m_trad <- lm(int_exp_trad ~ log(hhi_c2) + log(empleo), data = M)
r_trad <- coeftest(m_trad, vcov = vcovHC(m_trad, type = "HC1"))
cat("Regresion int_exp_trad: coef", round(r_trad[2, 1], 3), " p=", round(r_trad[2, 4], 4), "\n")

# =============================================================================
# 3. SENSIBILIDAD: nivel de desagregacion x variable resultado, con correccion de Holm
# =============================================================================
grilla <- expand_grid(indice = c("hhi_letra", "hhi_c2", "hhi_c6"), resultado = c("int_exp", "int_exp_trad")) |>
  mutate(res = map2(indice, resultado, function(i, r) {
    x <- M[[i]]; y <- M[[r]]; ct <- cor.test(x, y, method = "spearman", exact = FALSE); ci <- boot_ci(x, y, 3000)
    tibble(rho = unname(ct$estimate), p = ct$p.value, ic_inf = ci[1], ic_sup = ci[2])
  })) |> unnest(res) |>
  mutate(p_holm = p.adjust(p, "holm"),
         nivel = dplyr::recode(indice, hhi_letra = "Letra (21)", hhi_c2 = "Clae2 (87)", hhi_c6 = "Clae6 (945)"),
         resultado_lab = dplyr::recode(resultado, int_exp = "Todos los sectores", int_exp_trad = "Solo agro, minería e industria"))
write_csv(grilla |> mutate(across(where(is.numeric), ~ round(.x, 4))), "output/tablas/i2_inferencia_sensibilidad_holm.csv"); print(grilla)

g_forest <- ggplot(grilla |> mutate(nivel = factor(nivel, levels = rev(c("Letra (21)", "Clae2 (87)", "Clae6 (945)"))),
                                    sig = ifelse(p_holm < 0.05, "Significativo con Holm (< 0,05)", "No significativo con Holm")),
                   aes(rho, nivel, colour = sig)) +
  geom_vline(xintercept = 0, colour = "grey50") +
  geom_pointrange(aes(xmin = ic_inf, xmax = ic_sup), size = 0.8, linewidth = 1) +
  facet_wrap(~ resultado_lab, ncol = 1) +
  scale_colour_manual(values = c("Significativo con Holm (< 0,05)" = pal_naranja, "No significativo con Holm" = "grey55"), name = NULL) +
  labs(title = "La asociación depende del nivel de desagregación",
       subtitle = "ρ de Spearman entre HHI del empleo e intensidad exportadora, con IC 95% bootstrap (n = 24)",
       caption = fuente_cep,
       x = "ρ de Spearman", y = NULL) + tema_tp()
ggsave("output/graficos/i2_fig_inf_sensibilidad.png", g_forest, width = 8.5, height = 6, dpi = 150)

# Otros controles de sensibilidad del test principal
extra <- tibble(
  control = c("HHI clae2 winsorizado (p99 del empleo por celda)", "Theil clae2 en vez de HHI", "N equivalente clae2 (inverso del HHI)"),
  rho = c(spearman(read_csv("output/tablas/i2_impacto_winsor_hhi.csv", show_col_types = FALSE)$hhi_c2_w[match(M$provincia, read_csv("output/tablas/i2_impacto_winsor_hhi.csv", show_col_types = FALSE)$provincia)], M$int_exp),
          spearman(M$theil_c2, M$int_exp), spearman(M$neq_c2, M$int_exp))
)
write_csv(extra |> mutate(rho = round(rho, 3)), "output/tablas/i2_inferencia_otros_controles.csv"); print(extra)

# Mismo test con los datos de 2021 (HHI e intensidad exportadora de 2021)
d21 <- read_csv("input/establecimientos.csv", show_col_types = FALSE) |> filter(anio == 2021)
h21 <- d21 |> group_by(provincia, clae2) |> summarise(e = sum(empleo), .groups = "drop") |> group_by(provincia) |> summarise(hhi = calcular_hhi(e))
x21 <- d21 |> group_by(provincia) |> summarise(ie = 100 * sum(empresas_exportadoras) / sum(establecimientos)) |> left_join(h21, by = "provincia")
r21 <- cor.test(x21$hhi, x21$ie, method = "spearman", exact = FALSE)
write_csv(tibble(anio = 2021, rho = unname(r21$estimate), p = r21$p.value), "output/tablas/i2_inferencia_anio2021.csv"); cat("2021: rho =", round(r21$estimate, 3), "p =", signif(r21$p.value, 3), "\n")

# =============================================================================
# 4. GRAFICOS: dispersion principal y diagnostico
# =============================================================================
g_sc <- ggplot(M, aes(hhi_c2, int_exp)) +
  geom_smooth(method = "lm", formula = y ~ x, se = TRUE, colour = pal_naranja, fill = pal_naranja, alpha = 0.15) +
  geom_point(aes(colour = region), size = 3) +
  geom_text_repel(aes(label = provincia_label), size = 3.2, max.overlaps = 40) +
  scale_x_log10(labels = scales::label_number(decimal.mark = ",", accuracy = 0.01)) +
  scale_colour_manual(values = pal_regiones, name = NULL) +
  labs(title = "Las provincias con el empleo más concentrado tienen menos exportadoras por establecimiento",
       subtitle = paste0("24 provincias, 2022. ρ de Spearman = ", format(round(pt$rho, 2), decimal.mark = ","),
                         " (p de permutación = ", format(round(pt$p, 4), decimal.mark = ","), ")"),
       x = "HHI del empleo por rama clae2 (escala log; más a la derecha = más concentrada)",
       y = "Empresas exportadoras cada 100 establecimientos",
       caption = paste0(fuente_cep, "\nRecta: ajuste lineal de la intensidad exportadora sobre log(HHI), sin controles; banda: IC 95%. Color: región.")) + tema_tp()
ggsave("output/graficos/i2_fig_inf_dispersion.png", g_sc, width = 9, height = 6.2, dpi = 150)

dg <- tibble(provincia_label = M$provincia_label, ajustado = fitted(m), residuo = residuals(m), estandarizado = rstandard(m),
             cook = ck)
p1 <- ggplot(dg, aes(ajustado, residuo)) + geom_hline(yintercept = 0, linetype = "dashed") + geom_point(colour = pal_azul, size = 2) +
  geom_smooth(se = FALSE, colour = pal_naranja, linewidth = 0.8, method = "loess", formula = y ~ x) +
  labs(title = "Residuos vs. ajustados", x = "Valores ajustados", y = "Residuos") + tema_tp(10)
p2 <- ggplot(dg, aes(sample = estandarizado)) + stat_qq(colour = pal_azul) + stat_qq_line(colour = pal_naranja) +
  labs(title = "QQ de residuos", x = "Cuantiles teóricos", y = "Residuos estandarizados") + tema_tp(10)
p3 <- ggplot(dg |> arrange(desc(cook)) |> head(8) |> mutate(provincia_label = fct_reorder(provincia_label, cook)), aes(cook, provincia_label)) +
  geom_col(fill = pal_azul) + geom_vline(xintercept = umbral, colour = pal_naranja, linetype = "dashed") +
  labs(title = "Distancia de Cook (8 mayores)", subtitle = "Línea: 4/n", x = NULL, y = NULL) + tema_tp(10)
ggsave("output/graficos/i2_fig_inf_diagnostico.png", p1 + p2 + p3, width = 13, height = 4.3, dpi = 150)

cat("\nScript 06 terminado.\n")
