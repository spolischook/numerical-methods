# Title     : Task 4
# Objective : Course Project
# Created by: spolischook
# Created on: 16.11.18

lagrangePolynomial <- function(x, m) {
    result <- 0
    for (i in seq_along(m[ , 'x'])) {
        if (!is.finite(m[i, 'y'])) next
        result <- result + m[i, 'y'] * lLagrange(i, x, m)
    }

    return (result)
}

lLagrange <- function(i, x, m) {
    if (class(m) != 'matrix') stop(sprintf('Third argument must be a "matrix", "%s" given', class(m)))
    if (class(i) != 'integer') stop(sprintf('First argument must be a "number", "%s" given', class(i)))
    if (class(x) != 'numeric') stop('Second argument must be a number')

    numerator   <- 1
    denominator <- 1

    for (j in seq_along(m[ , 'x'])) {
        if (j == i) next

        numerator   <- numerator * (x - m[j, 'x'])
        denominator <- denominator * (m[i, 'x'] - m[j, 'x'])
    }

    return (numerator/denominator)
}

f <- function(x) {
    return (1/(1+x))
}

x = seq(-1.2, -0.8, by=0.1)
f3 <- function(x) {
    return (1/(1+x))
}

makeXYmatrix <- function(v, f) {
    m = matrix(
        nrow = length(v),
        ncol = 2,
        dimnames = list(
            NULL,
            c('x', 'y')
        )
    )

    for (i in seq_along(v)) {
        x <- v[i]
        y <- f(x)
        m[i, 'x'] = x
        m[i, 'y'] = y
    }

    return (m)
}

m = makeXYmatrix(x, f3)
result = lagrangePolynomial(-0.96, m)
r=1
