# Title     : Numerical methods
# Objective : Solving linear equations
# Created by: spolischook
# Created on: 19.11.18

iteration <- function(f, Xs, Xe, y, step) {
  Ys <- f(Xs)
  DI <- Ys > y 
  
  calc    <- makeXYmatrix(c(Xs), f)
  results <- makeXYmatrix(double())
  
  for (x in seq(Xs + step, Xe, by=step)) {
    y2 <- f(x)
    if (y == y2) {
      DI <- !DI
      rbind(results, c(x, y))
    }
    if (DI != (y2 > y)) {
      DI <- y2 > y
      results <- rbind(
        results,
        c(
          'x' = x - step/2,
          'y' = f(x - step/2) 
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
  
  return (list(calc, results))
}
