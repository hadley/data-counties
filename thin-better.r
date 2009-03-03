source("thin.r")

# Rotate starting & ending points so that they coincide with a 
# neighbour change
rotate <- function(df) {
  n <- nrow(df)
  first <- which.max(df$change)
  rbind(df[first:n, ], df[1:first, ])
}

add_tol <- function(df) {
  df$hash <- paste(df$long, df$lat)
  if (is.null(df$order)) df$order <- 1:nrow(df)
  neighbours <- ddply(df, .(hash), nrow)
  names(neighbours) <- c("hash", "count")
  
  df <- merge(df, neighbours, by = "hash")
  df <- df[order(df$order), ]
  
  # A point is a change point if the count is:
  #  * one greater than the surrounding points (neighbour has joined)
  #  * two or more (must be a corner)
  df <- ddply(df, .(group), transform, 
    change = diff(c(count[length(count)], count)) > 0  | count > 2
  )
  df <- ddply(df, .(group), rotate)

  df <- ddply(df, .(group), thin_poly, .progress = "text")
  df$hash <- NULL

  df  
}


thin_poly <- function(df) {
  breaks <- unique(c(1, which(df$change), nrow(df)))
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

