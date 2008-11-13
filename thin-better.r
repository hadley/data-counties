library(ggplot2)
source("thin.r")
raw <- read.csv("county-boundaries-raw.csv", 
  colClasses = c("numeric", "numeric", "character"))
raw <- unique(raw)
raw$order <- seq_len(nrow(raw))
raw$hash <- paste(raw$x, raw$y)


# Break each region polygon up into a set of polylines
# Thin each polyline
# Join back together

co <- subset(raw, substr(id, 0, 2) == "08")
co <- unique(co)
co$order <- seq_len(nrow(co))
counts <- as.data.frame(table(co$hash))
names(counts) <- c("hash", "freq")
co <- merge(co, counts, by = "hash", sort = FALSE)
co <- co[order(co$order), ]

co <- ddply(co, .(id), transform, change = c(F, diff(freq) > 0))

qplot(x, y, data = co, geom = "path", group = id) + 
  geom_point(aes(colour = factor(freq)), data = subset(co, change))



thin_piece <- function(data, start, end, last = FALSE) {
  sub <- data[start:end, ]
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

thin_poly <- function(df) {
  breaks <- c(1, which(df$change), nrow(df))
  pieces <- as.data.frame(embed(breaks, 2)[, c(2, 1)])
  colnames(pieces) <- c("start", "end")
  pieces$last <- rep(c(F, T), c(nrow(pieces) - 1, 1))

  mdply(pieces, thin_piece, data = df)  
}

co2 <- ddply(co, .(id), thin_poly)
trace3(co2, 0.3) + aes(group = id)
