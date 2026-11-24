# p-valor de permutacion para la correlacion de Spearman (bilateral).
# Se permuta y entre las unidades B veces y se cuenta cuantas veces |rho simulado| >= |rho observado|,
# sumando uno al numerador y al denominador (asi el p nunca es exactamente 0).
p_permutacion_spearman <- function(x, y, B = 10000, semilla = 2026) {
  set.seed(semilla)
  rx <- rank(x); ry <- rank(y)
  obs <- abs(cor(rx, ry))
  sims <- replicate(B, abs(cor(rx, sample(ry))))
  (1 + sum(sims >= obs - 1e-12)) / (B + 1)
}
