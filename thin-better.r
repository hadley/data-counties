source("thin.r")

# Rotate starting & ending points so that they coincide with a 
# neighbour change
rotate <- function(df) {
  n <- nrow(df)
  first <- which.max(df$change)
  rbind(df[first:n, ], df[1:first, ])
}


thin_poly <- function(df) {
  breaks <- which(df$change)
  pieces <- as.data.frame(embed(breaks, 2)[, c(2, 1), drop = FALSE])
  colnames(pieces) <- c("start", "end")
  pieces$last <- rep(c(F, T), c(nrow(pieces) - 1, 1))

  mdply(pieces, thin_piece, data = df)  
}


# Given a data frame representing a region, and the starting and 
# end pointing of a single polyline, augment that line with dp tolerances
thin_piece <- function(data, start, end, last = FALSE) {
  sub <- data[start:end, ]
  # Remove any duplicated locations
  sub <- sub[!duplicated(sub[c("long", "lat")]), ]

  if (nrow(sub) < 3) {
    sub$tol <- Inf
    return(sub)
  }
  tol <- c(Inf, compute_tol(sub), Inf)
  
  if (length(tol) != nrow(sub)) browser()
  sub$tol <- tol

  # The last row is duplicated for all pieces except the last
  if (!last) {
    sub[-nrow(sub), ]    
  } else {
    sub
  }
}

