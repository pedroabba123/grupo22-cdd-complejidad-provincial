# Tema, paletas y notas al pie de los graficos del TP (criterios de la clase de visualizacion:
# color con funcion y mismo significado en todos los graficos, fuente y universo en cada grafico).
library(ggplot2)

tema_tp <- function(base_size = 12) {
  theme_minimal(base_size = base_size) +
    theme(
      plot.title.position = "plot", plot.caption.position = "plot",
      plot.title    = element_text(face = "bold", size = base_size + 1),
      plot.subtitle = element_text(colour = "grey35"),
      plot.caption  = element_text(colour = "grey40", size = base_size - 3, hjust = 0),
      panel.grid.minor = element_blank(),
      legend.position = "bottom"
    )
}

pal_azul    <- "#1F4E79"   # acento general
pal_naranja <- "#D9822B"   # acento: dato o resultado a destacar
pal_gris    <- "grey60"

# REGIONES: siempre estos colores (paleta Okabe-Ito, apta para daltonismo)
pal_regiones <- c("Pampeana y GBA" = "#0072B2", "NOA" = "#E69F00", "NEA" = "#009E73", "Cuyo" = "#CC79A7", "Patagonia" = "#D55E00")

# GRUPOS DEL CLUSTER: siempre estos colores y formas (otra familia de colores, para no confundirlos con las regiones)
pal_grupos <- c("Intermedio: agro, industria y servicios" = "#7F7F7F", "Patagonia minera y petrolera" = "#7B3294",
                "CABA: servicios modernos" = "#111111", "Comercio y construcción" = "#A6611A")
forma_grupos <- c("Intermedio: agro, industria y servicios" = 16, "Patagonia minera y petrolera" = 15,
                  "CABA: servicios modernos" = 17, "Comercio y construcción" = 18)

fuente_cep   <- "Fuente: CEP XXI, Ministerio de Economía (datos.produccion.gob.ar). Empleo privado registrado, 2022. Elaboración propia."
fuente_cep_f <- paste0(fuente_cep, " ECI: Luvini (2024), Fundar.")
