if(!file.exists("rawtol.rdata")) {
  raw <- read.csv("county-boundaries-raw.csv")
  raw$long <- round_any(raw$long, 0.001)
  raw$lat <- round_any(raw$lat, 0.001)
  raw$order <- NULL
  raw <- unique(raw)

  rawtol <- add_tol(raw)
  save(rawtol, file = "rawtol.rdata")
} else {
  load("rawtol.rdata")  
}

rawtol <- transform(rawtol, long = ifelse(long > 150, -long, long))
thin01 <- subset(rawtol, tol > 0.1)
qplot(long, lat, data = thin01)

qplot(long, lat, data = thin01, geom="polygon", group = group)

last_plot() + xlim(-125, -65) + ylim(25, 50)

thin001 <- subset(rawtol, tol > 0.02)
dim(thin001)
qplot(long, lat, data = thin001, geom="polygon", group = group) + xlim(-125, -65) + ylim(25, 50)
