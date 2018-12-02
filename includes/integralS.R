# Title     : Methods for numerical integration
# Objective : Numerical approximation of definite integrals
# Created by: spolischook
# Created on: 25.11.18

source('./lagrange_polynomial.R', chdir = T)
source('./makeXYmatrix.R', chdir = T)

integralS.rectangle <- function(a, b, f, length.out, plot=FALSE) {
  seq <- seq(a, b, length.out=length.out)
  S <- 0
  i <-1

  while(length(seq) > i) {
    x  <- seq[i]
    x1 <- seq[i+1]
    y = f(x)
    S <- S + (y*(x1 - x))
    i <- i+1
    if (plot) lines(
      c(x, x, x1, x1), c(0, y, y, f(x1)), col='red', lty='longdash'
    )
  }

  if (plot) lines(c(x1, x1), c(y, 0), col='red', lty='longdash')

  return (S)
}

integralS.trapeze <- function(a, b, f, length.out, plot=FALSE) {
  seq <- seq(a, b, length.out=length.out)
  S <- 0
  i <-1

  while(length(seq) > i) {
    x  <- seq[i]
    x1 <- seq[i+1]
    y = f(x)
    S <- S + ((y + f(x1))/2*(x1 - x))
    i <- i+1
    if (plot) lines(
      c(x, x, x1), c(0, y, f(x1)), col='red', lty='longdash'
    )
  }

  if (plot) lines(c(x1, x1), c(f(x1), 0), col='red', lty='longdash')

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

  fys <- f(xs)
  in.f <- fys >= ys & fys > 0 & ys > 0 | fys <= ys & fys < 0 & ys <  0

  ratio <- (sum(in.f)/runs)

  Y <- maxY - minY
  X <- b - a
  S <- Y * X * ratio

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
    x2 <- x[i+1]
    x1 <- (x2 + x0)/2

    y0 <- f(x0)
    y1 <- f(x1)
    y2 <- f(x2)
    h <- (x2 - x1)

    if (plot) {
      xm  <- seq((x0 - h*10), (x2 + h*10), by=(h/100))
      XYm <- makeXYmatrix(c(x0, x1, x2), f)
      ym  <- lagrangePolynomial(xm, XYm)
      lines(xm, ym, col=i, lty='dotted')
      lines(c(x0, x0), c(y0, 0), col=i, lty='longdash')
      lines(c(x2, x2), c(y2, 0), col=i, lty='longdash')
      points(c(x0, x1, x2), c(y0, y1, y2))
    }

    S <- S + (h/3 * (y0 + 4 * y1 + y2))
  }

  return (S)
}
