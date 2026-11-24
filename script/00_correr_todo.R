# =============================================================================
# TP Ciencia de Datos - FCE UBA - Grupo 22
# Script 00: corre todo el analisis en orden, de raw/ hasta output/.
#
# Uso: abrir grupo22_cdd_tp.Rproj en RStudio (asi la carpeta de trabajo es la raiz del proyecto) y ejecutar este
# script, o desde una terminal parada en la raiz del proyecto: Rscript script/00_correr_todo.R
# Cada script es autonomo: lee los archivos que guardo el anterior, no depende de objetos en memoria.
# Tarda alrededor de un minuto.
# =============================================================================

if (!dir.exists("script") || !dir.exists("utils")) {
  stop("Correr desde la raiz del proyecto (abrir grupo22_cdd_tp.Rproj).")
}

scripts <- c("01_carga_y_exploracion_inicial.R",
             "02_descriptivas_variables_principales.R",
             "03_faltantes_y_outliers.R",
             "04_indices_lq_hhi_theil.R",
             "05_cluster_provincias_hcpc.R",
             "06_inferencia_spearman_regresion.R",
             "07_robustez_nivel_departamental.R",
             "08_graficos_instancia3.R")

for (s in scripts) {
  cat("\n>>>", s, "\n")
  source(file.path("script", s), encoding = "UTF-8")
}
cat("\nListo. Tablas en output/tablas/ y graficos en output/graficos/.\n")
