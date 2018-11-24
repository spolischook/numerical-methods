# Title     : Iteration method
# Objective : Solving linear equations by iteration method
# Created by: spolischook
# Created on: 19.11.18

source('./makeXYmatrix.R', chdir = T)

iteration <- function(f, Xs, Xe, y, precision) {
  Ys <- f(Xs)
  DI <- Ys > y 
  
  calc    <- makeXYmatrix(c(Xs), f)
  results <- makeXYmatrix(double())
  
  for (x in seq(Xs + precision, Xe, by=precision)) {
    y2 <- f(x)
    if (y == y2) {
      DI <- !DI
      results <- rbind(results, c(x, y))
    }
    if (DI != (y2 > y)) {
      DI <- y2 > y
      results <- rbind(
        results,
        c(
          'x' = x - precision/2,
          'y' = f(x - precision/2)
        )
      )
    }
    calc <- rbind(
      calc,
      c(
        'x' = x,
        'y' = y2 
      )
    )
  }

  calc    <- merge(calc,    makeXYmatrix(double()), all = TRUE) # add line numbers
  results <- merge(results, makeXYmatrix(double()), all = TRUE) # add line numbers

  return (list(calc, results))
}
