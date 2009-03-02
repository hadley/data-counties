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
    geom_path(data = df[thin, ], colour = alpha("grey", 0.8), size = 3) +
    geom_point(data = df[thin, ], colour = "white", size = 1) 
}

trace2 <- function(df, tol) {
  thin <- df[df$tol >= tol, ]
  
  qplot(x, y, data = df, geom="path") + 
    geom_point(size = 2) + 
    geom_polygon(data = thin, colour = alpha("grey", 0.6), size = 3, fill=NA) +
    geom_point(data = thin, colour = "white", size = 1) 
}

trace3 <- function(df, tol) {
  thin <- df[df$tol >= tol, ]
  
  ggplot(thin, aes(x, y)) + 
    geom_polygon(fill = NA, colour = "grey20", data = df) +
    geom_point(data = subset(df, (!change) & tol == Inf), size = 2, colour = "red") + 
    geom_point(data = subset(df, change), size = 2) + 
    geom_polygon(fill = alpha("gray50", 0.4), colour="#3366ff")
}