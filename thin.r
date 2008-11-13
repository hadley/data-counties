# Implementation of the Douglas-Peucker algorithm for line thinning
# http://en.wikipedia.org/wiki/Ramer-Douglas-Peucker_algorithm
thin <- function(points, tol = 0.01) {
  points <- unique(points)
  
  points[simplify_rec(points, tol = tol), ]
}

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
    c(1, n)
    c()
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
# 
# al2 <- subset(raw, id == "02.001")
# 
trace <- function(df, tol) {
  thin <- c(1, simplify_rec(al2, tol), nrow(df))
  qplot(x, y, data = al2, geom="path") + 
    geom_point(size = 2) + 
    geom_path(data = al2[thin, ], colour = alpha("grey", 0.8), size = 3) +
    geom_point(data = al2[thin, ], colour = "white", size = 1) 
}

# Precompute all distances so that we can post-select quickly
simplify_dist <- function(points, offset = 0) {
  n <- nrow(points)
  if (n <= 2) {
    rep(0, n)
  } else {
    dist <- with(points, point_line_dist(x, y, x[1], y[1], x[n], y[n]))
  
    furthest <- which.max(dist)
    c(
      simplify_dist(points[1:(furthest - 1), ], offset),
      dist[furthest],
      simplify_dist(points[(furthest + 1):n, ], furthest + offset)
    )
  }
}

# al2 <- subset(raw, id == "02.020")
# qplot(x, y, data=al2)
# simplify_dist(al2)
# al2$eps <- simplify_dist(al2)