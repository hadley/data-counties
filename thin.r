# Implementation of the Douglas-Peucker algorithm for line thinning
# http://en.wikipedia.org/wiki/Ramer-Douglas-Peucker_algorithm
simplify_rec <- function(points, tol = 0.01) {
  n <- nrow(points)
  if (n <= 2) return() # c(1, n))
  dist <- with(points, point_line_dist(x, y, x[1], y[1], x[n], y[n]))
  
  if (max(dist, na.rm = T) > tol) {
    furthest <- which.max(dist)
    c(
      simplify_rec(points[1:(furthest - 1), ], tol),
      furthest,
      simplify_rec(points[(furthest + 1):n, ], tol) + furthest 
    )
  } else {
    c()
  }  
}

# From http://mathworld.wolfram.com/Point-LineDistance2-Dimensional.html
point_line_dist <- function(px, py, lx_1, ly_1, lx_2, ly_2) {
  abs((lx_2 - lx_1) * (ly_1 - py) - (lx_1 - px) * (ly_2 - ly_1)) /
    sqrt((lx_2 - lx_1) ^ 2 + (ly_2 - ly_1) ^ 2)
}

# Precompute all tolerances so that we can post-select quickly
compute_tol <- function(points, offset = 0) {
  n <- nrow(points)
  if (n <= 2) {
    c()
  } else if (n == 3) {
    with(points,
      point_line_dist(long[2], lat[2], long[1], lat[1], long[3], lat[3]))
  } else {
    dist <- with(points, 
      point_line_dist(long[2:(n-1)], lat[2:(n-1)], long[1], lat[1], long[n], lat[n])
    )
  
    furthest <- which.max(dist)
    if (length(furthest) == 0) browser()
    c(
      compute_tol(points[1:(furthest + 1), ], offset),
      dist[furthest],
      compute_tol(points[(furthest + 1):n, ], furthest + offset)
    )
  }
}