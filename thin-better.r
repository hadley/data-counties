source("thin.r")
raw <- read.csv("county-boundaries-raw.csv")
raw$hash <- paste(raw$x, raw$y)


thin_careful <- function(df, tol = 0.01) {
  regions <- levels(factor(df$id))

  keepers <- c()
  for(r in regions) {
    cat(".")

    region <- df[df$id == r, ]
    keep_here <- which(region$hash %in% keepers)

    rows <- simplify_rec(region, keep_here, tol = tol)
    keep <- seq_len(nrow(region)) %in% rows

    keepers <- unique(c(keepers, region$hash[keep]))
  }
  cat("\n")
  
  df[df$hash %in% keepers, ]
}

map <- ggplot(mapping = aes(x, y, group = id)) + geom_polygon(colour = "black")


al <- subset(raw, substr(id, 0, 2) == "02")
al_t1 <- ddply(al, .(id), thin, tol = 0.3)
al_t2 <- thin_careful(al, tol = 0.4)

map %+% al
map %+% al_t1
map %+% al_t2

raw_t2 <- thin_careful(raw, tol = 0.5)

co <- subset(raw, substr(id, 0, 2) == "08")
al_t1 <- ddply(al, .(id), thin, tol = 0.1)
cot <- thin_careful(co, tol = 0.4)
map %+% cot
