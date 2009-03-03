library(ggplot2)
source("thin-better.r")

if (!file.exists("mn.rdata")) {
  raw <- read.csv("county-boundaries-raw.csv")
  raw$long <- round_any(raw$long, 0.001)
  raw$lat <- round_any(raw$lat, 0.001)
  raw$order <- NULL
  raw <- unique(raw)

  mn <- subset(raw, substr(id, 0, 2) == "27")
  save(mn, file = "mn.rdata")  
} else {
  load("mn.rdata")  
}

# Find out which points belong to which ids
neighbours <- ddply(mn, .(hash), nrow)
names(neighbours) <- c("hash", "count")

# Identify points on the boundary where the neighbour changes
mn <- merge(mn, neighbours, by = "hash")
mn <- mn[order(mn$order), ]
mn <- ddply(mn, .(group), transform, 
  change = diff(c(count[length(count)], count)) > 0
)

source("poly.r")
centres <- ddply(mn, .(id), colwise(midrng, .(long, lat)))

ggplot(mn, aes(long, lat)) + 
  geom_polygon(aes(group = group), fill = NA, colour = "grey50") +
  geom_point(aes(colour = factor(count)), subset(mn, change)) + 
  geom_text(aes(label = id), data = centres, size = 2)

# Rotate counties to ensure that each polygon starts on an intersection
mn2 <- ddply(mn, .(id), rotate)
# Check that it worked - every id should have a change point 
# at the start and the end
ddply(mn2, .(id), function(df) df$change[c(1, nrow(df))])

mn3 <- ddply(mn2, .(id), thin_poly, .progress = "text")
qplot(tol, data = mn2, binwidth = 0.01)
qplot(log(tol), data = mn2, binwidth = 0.5)

ggplot(subset(mn3, tol > 0.01), aes(long, lat)) + 
  geom_polygon(aes(group = group), fill = "grey50", colour = "white") + 
  geom_polygon(aes(group = group), fill = NA, colour = alpha("red", 0.5), data = mn3)

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

# 27.031, 27.075