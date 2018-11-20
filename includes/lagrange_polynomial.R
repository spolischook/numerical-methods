# Title     : Lagrange polynomial
# Objective : https://en.wikipedia.org/wiki/Lagrange_polynomial
# Created by: spolischook
# Created on: 15.11.18

lagrangePolynomial <- function(x, m) {
  result <- 0
  for (i in seq_along(m[, 'x'])) {
    if (! is.finite(m[i, 'y']))next
    result <- result + m[i, 'y'] * lLagrange(i, x, m)
  }

  return (result)
}

lLagrange <- function(i, x, m) {
  if (class(m) != 'matrix') stop(sprintf('Third argument must be a "matrix", "%s" given', class(m)))
  if (class(i) != 'integer') stop(sprintf('First argument must be a "number", "%s" given', class(i)))
  if (class(x) != 'numeric') stop('Second argument must be a number')

  numerator <- 1
  denominator <- 1

  for (j in seq_along(m[, 'x'])) {
    if (j == i)next
    if (j == 0)next

    numerator <- numerator * (x - m[j, 'x'])
    denominator <- denominator * (m[i, 'x'] - m[j, 'x'])
  }

  return (numerator / denominator)
}

