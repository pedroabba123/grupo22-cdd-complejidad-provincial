# Cociente de localizacion (LQ), el analogo regional del RCA de Balassa (clase 11):
#   LQ_ps = (E_ps / E_p.) / (E_.s / E_..)
# Recibe una matriz provincia x sector (empleo). Devuelve otra matriz del mismo tamano.
# LQ > 1: el sector pesa mas en la provincia que en el total del pais.
calcular_lq <- function(M) {
  part_prov <- M / rowSums(M)
  part_nac  <- colSums(M) / sum(M)
  sweep(part_prov, 2, part_nac, "/")
}
