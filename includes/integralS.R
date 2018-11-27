# Title     : Methods for numerical integration
# Objective : Numerical approximation of definite integrals
# Created by: spolischook
# Created on: 25.11.18

integralS.rectangle <- function(a, b, f, length.out, plot=FALSE) {
  seq <- seq(a, b, length.out=length.out)
  S <- 0
  i <-1
  while(length(seq) > i) {
    S <- S + (f(seq[i])*(seq[i+1]-seq[i]))
    i <- i+1
  }

  return (S)
}

integralS.trapeze <- function(a, b, f, length.out, plot=FALSE) {
  seq <- seq(a, b, length.out=length.out)
  S <- 0
  i <-1
  while(length(seq) > i) {
    S <- S + ((f(seq[i]) + f(seq[i+1]))/2*(seq[i+1]-seq[i]))
    i <- i+1
  }

  return (S)
}

integralS.monteCarlo <- function(a, b, f, length.out, plot=FALSE) {
  runs <- length.out
  x <- seq(a, b, length.out=runs)
  y <- f(x)
  minY <- ifelse(min(y) > 0, 0, min(y))
  maxY <- ifelse(max(y) < 0, 0, max(y))

  xs <- runif(runs, min=a, max=b)
  ys <- runif(runs, min=minY, max=maxY)

  fys <- f3(xs)
  in.f <- fys >= ys & fys > 0 & ys > 0 | fys <= ys & fys < 0 & ys <  0

  ratio <- (sum(in.f)/runs)

  Y <- maxY - minY
  X <- b - a
  S <- Y * X * ratio
if (length.out == 10) {
  print(in.f)
  print(paste('Sum:', sum(in.f)))
  print(paste('Ratio:', sum(in.f)/runs))
  print(paste('Y:', maxY - minY))
  print(paste('X:', b - a))
  print(paste('S:', Y * X * ratio))
  print(paste('plot:', plot))
}
  if (plot) {
    points(xs,ys,pch='0',col=ifelse(in.f,"blue","grey")
     ,xlab='',ylab='',asp=1)
  }

  return (S)
}

integralS.simpson <- function(a, b, f, length.out, plot=FALSE) {
  x <- seq(a, b, length.out=length.out)
  S <- 0
  for(i in 1:(length(x)-1)) {
    x0 <- x[i]
    x1 <- (x[i+1] + x[i])/2
    x2 <- x[i+1]

    y0 <- f(x0)
    y1 <- f(x1)
    y2 <- f(x2)
    h <- (x2 - x1)

    S <- S + (h/3 * (y0 + 4 * y1 + y2))
  }

  return (S)
}
