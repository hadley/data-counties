
poly_area <- function(x, y) {
  n <- length(x)
  abs(1/2 * sum(x[-n] * y[-1] - x[-1] * y[-n]))
}

midrng <- function(x) mean(range(x))

centroid <- function(x, y) {
  n <- length(x)
  c(
    sum((x[-n] + x[-1]) * (x[-1] * y[-n] - x[-n] * y[-1])),
    sum((y[-n] + y[-1]) * (x[-1] * y[-n] - x[-n] * y[-1]))
  ) / (6 * poly_area(x, y))
}
