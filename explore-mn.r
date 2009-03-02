library(ggplot2)
source("thin-better.r")
raw <- read.csv("county-boundaries-raw.csv")
raw$long <- round_any(raw$long, 0.001)
raw$lat <- round_any(raw$lat, 0.001)
raw <- unique(raw)
raw$order <- seq_len(nrow(raw))
raw$hash <- paste(raw$long, raw$lat)

mn <- subset(raw, substr(id, 0, 2) == "27")

# Find out which points belong to which ids
neighbours <- ddply(mn, .(hash), function(df) length(unique(df$id)))
names(neighbours) <- c("hash", "count")

# Identify points on the boundary where the neighbour changes
mn <- merge(mn, neighbours, by = "hash")
mn <- mn[order(mn$order), ]
mn <- ddply(mn, .(id), transform, 
  change = diff(c(count[length(count)], count)) > 0,
)

midrng <- function(x) mean(range(x))
centres <- ddply(mn, .(id), colwise(midrng, .(x, y)))

ggplot(mn, aes(x, y)) + 
  geom_polygon(aes(group = id), fill = NA, colour = "grey50") +
  geom_point(aes(colour = factor(count)), subset(mn, change)) + 
  geom_text(aes(label = id), data = centres, size = 2)

# Rotate counties to ensure that each polygon starts on an intersection
co2 <- ddply(mn, .(id), rotate)
# Check that it worked - every id should have a change point 
# at the start and the end
ddply(co2, .(id), function(df) df$change[c(1, nrow(df))])


co3 <- ddply(co2, .(id), thin_poly, .progress = "text")
trace3(co2, 0.1) + aes(group = id)


zoom <- subset(mn, x > -105.12 & x < -105 & y > 39.5 & y < 39.8)
zoom2 <- subset(co2, id %in% unique(zoom$id))
zoom2$x <- round_any(zoom2$x, 0.001)
zoom2$y <- round_any(zoom2$y, 0.001)

qplot(x, y, data = zoom2, geom = "polygon", group = id, fill = id) + scale_fill_hue(alpha = 0.4) + geom_point(aes(colour = factor(freq), shape = change), subset(zoom2, change | freq > 2))


# Explore incorrect change detection -----------------------------------------

# Why aren't we capturing this break correctly?
z1 <- subset(mn, id %in% c("27.079", "27.131", "27.139"))
ggplot(z1, aes(x, y)) + geom_point(aes(colour = id, size = factor(count), shape=change))
last_plot() + xlim(-93.6, -93.4) + ylim(44.50, 44.55)
last_plot() + xlim(-93.55, -93.50) + ylim(44.54, 44.55)
# Looks like the intersection of the two states does not take place
# on a point on the boundary of the third state

z2 <- subset(mn, id %in% c("27.079", "27.131", "27.139"))
ggplot(z1, aes(x, y)) + geom_point(aes(colour = id, size = factor(count), shape=change))