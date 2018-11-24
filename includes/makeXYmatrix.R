# Title     : X\Y matrix
# Objective : Make matrix with x and y rows
# Created by: spolischook
# Created on: 16.11.18

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


