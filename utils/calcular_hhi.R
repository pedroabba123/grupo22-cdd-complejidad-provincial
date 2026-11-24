# HHI de un vector de participaciones o de valores (se normaliza para que sumen 1).
# Rango [1/N, 1]. Mismo criterio que la clase 11 / clase 12 del curso.
calcular_hhi <- function(x) {
  x <- x[x > 0]
  s <- x / sum(x)
  sum(s^2)
}

# Numero equivalente: cuantas ramas de igual tamano darian el mismo HHI.
n_equivalente <- function(hhi) 1 / hhi
