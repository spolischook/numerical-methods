# Title     : Newton method
# Objective : Finding successively better approximations to the roots (or zeroes) of a real-valued function
# Created by: spolischook
# Created on: 22.11.18

source('./makeXYmatrix.R', chdir = T)

nm.newton.Xn <- function(x, f, df) {
  return (x - f(x)/df(x))
}

is.in_range <- function(x, x0, x1) {
  if (x0 <= x & x <= x1) return(TRUE)
  if (x1 <= x & x <= x0) return(TRUE)
  return (FALSE)
}

nm.newton.findX <- function(x, f, df, Xs, Xe, precision) {
  X0 <- x
  Xn <- nm.newton.Xn(x, f, df)
  Yx <- f(Xn)
  results <- makeXYmatrix(double())
  calc    <- makeXYmatrix(c(x, Xn), f)

  while (is.in_range(Xn, Xs, Xe) & abs(X0 - Xn) > precision) {
    X0 <- Xn
    Xn <- nm.newton.Xn(X0, f, df)
    Yx <- f(Xn)
    calc <- rbind(calc, c(Xn,  Yx))
  }

  if (abs(X0 - Xn) <= precision) {
    results <- rbind(results, c(Xn, Yx))
  }

  return (list(calc, results))
}

nm.newton <- function(f, df, Xs, Xe, precision) {
  resultsS <- nm.newton.findX(Xs, f, df, Xs, Xe, precision)
  results  <- resultsS[[2]]
  calc     <- resultsS[[1]]

  resultsE <- nm.newton.findX(Xe, f, df, Xs, Xe, precision)
  results  <- merge(results, resultsE[[2]], all = TRUE)
  calc     <- merge(calc,    resultsE[[1]], all = TRUE)

  return (list(calc, results))
}
