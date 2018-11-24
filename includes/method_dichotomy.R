# Title     : Dichotomy method
# Objective : Solving linear equations by dichotomy method
# Created by: spolischook
# Created on: 20.11.18

source('makeXYmatrix.R', chdir = T)

dichotomy <- function(f, Xs, Xe, y, precision, ...) {
  x  <- (Xs + Xe)/2
  Yx <- f(x)
  Ys <- f(Xs)
  Ye <- f(Xe)
  results <- if(length(list(...))) list(...)[[2]] else makeXYmatrix(double())
  calc    <- if(length(list(...))) list(...)[[1]] else makeXYmatrix(c(Xs, Xe), f)

  calc <- rbind(calc, c(x,  Yx))
  if (abs(Xe - Xs) <= precision) {
    results <- rbind(results, c(x,  Yx))
    return (list(calc, results))
  }

  if (Yx == y) results <- rbind(results, c(x,  y))
  if (Ys == y) results <- rbind(results, c(Xs, y))
  if (Ye == y) results <- rbind(results, c(Xe, y))


  if ((Yx - y) * (Ye - y) <= 0) {
    l <- dichotomy(f, x, Xe, y, precision, calc, results)
    results <- merge(results, l[[2]], all = TRUE)
    calc    <- merge(calc,    l[[1]],    all = TRUE)
  }

  if ((Ys - y) * (Yx - y) < 0) {
    l <- dichotomy(f, Xs, x, y, precision, calc, results)
    results <- merge(results, l[[2]], all = TRUE)
    calc    <- merge(calc,    l[[1]],    all = TRUE)
  }

  return (list(calc, results))
}
