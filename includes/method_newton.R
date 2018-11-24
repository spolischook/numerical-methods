# Title     : Newton method
# Objective : Finding successively better approximations to the roots (or zeroes) of a real-valued function
# Created by: spolischook
# Created on: 22.11.18

source('./makeXYmatrix.R', chdir = T)

newton_method_f <- function(x, f, df) {
  return (x - f(x)/df(x))
}

newton_method <- function(f, df, Xs, Xe, y, precision) {
  Ys  <- f(Xs)
  Yd  <- df(Xs)

  calc    <- makeXYmatrix(c(Xs), f)
  results <- makeXYmatrix(double())

  return (list(calc, results))
}

f1 <- function(x) {
  return (x ^ 2 - cos(x))
}

df1 <- function(x) {
  return (2*x + sin(x))
}

result <- newton_method(f1, df1, -2, 2, 0, 0.01)
print(result)

