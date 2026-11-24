# Winsorizacion: recorta los valores a los percentiles elegidos sin eliminar observaciones (clase 13).
winsorizar <- function(x, p_inf = 0, p_sup = 0.99) {
  lims <- quantile(x, probs = c(p_inf, p_sup), na.rm = TRUE)
  pmin(pmax(x, lims[1]), lims[2])
}
