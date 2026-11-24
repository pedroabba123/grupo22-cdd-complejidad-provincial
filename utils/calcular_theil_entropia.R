# Theil de concentracion sectorial: T = ln(N) - H, con H la entropia de Shannon de las
# participaciones y N la cantidad TOTAL de categorias posibles (la misma para todas las
# provincias, para que sean comparables). T = 0 si el empleo se reparte parejo entre las N
# categorias; T = ln(N) si se concentra todo en una. Clase 18, parte 3.
calcular_theil_entropia <- function(x, n_categorias = length(x)) {
  s <- x[x > 0] / sum(x)
  log(n_categorias) + sum(s * log(s))
}
