# Implementation of the Douglas-Peucker algorithm for line thinning
# http://en.wikipedia.org/wiki/Ramer-Douglas-Peucker_algorithm
thin <- function(points, tol = 0.01) {
  points <- unique(points)
  n <- nrow(points)
  
  points[simplify_rec(points, tol = tol), ]
}

simplify_rec <- function(points, keep = c(), tol = 0.01) {
  n <- nrow(points)
  dist <- with(points, point_line_dist(x, y, x[1], y[1], x[n], y[n]))
  
  if (max(dist) > tol) {
    furthest <- which.max(dist)
    c(
      simplify_rec(points[1:(furthest - 1), ], keep, tol),
      furthest,
      simplify_rec(points[(furthest + 1):n, ], keep, tol) + furthest 
    )
  } else {
    c(1, n)
  }  
}

# From http://mathworld.wolfram.com/Point-LineDistance2-Dimensional.html
point_line_dist <- function(px, py, lx_1, ly_1, lx_2, ly_2) {
  abs((lx_2 - lx_1) * (ly_1 - py) - (lx_1 - px) * (ly_2 - ly_1)) /
    sqrt((lx_2 - lx_1) ^ 2 + (ly_2 - ly_1) ^ 2)
}

# points <- unique(points)
# plot(points)
# lines(thin(points, 0.01), col = "red", pch=18)
# n <- nrow(points)
# points(points[c(1, n), ], col ="blue", pch=18)
