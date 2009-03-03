tx <- subset(raw, substr(id, 0, 3) == "48.")
tx <- add_tol(tx)

qplot(long, lat, data = subset(tx, tol > 0.05), geom="polygon", group=group)

qplot(long, lat, data = subset(tx, tol > 0.05), colour = change)


source("poly.r")
centres <- ddply(tx, .(id), colwise(midrng, .(long, lat)))
names(centres)[2:3] <- c("long", "lat")

ggplot(subset(tx, tol > 0.1), aes(long, lat, group = group)) + 
  geom_polygon(fill = "grey80", colour = NA)

ggplot(subset(tx, tol > 0.1), aes(long, lat, group = group)) + 
  geom_polygon(fill = "grey80", colour = "white", data = tx) +
  geom_polygon(fill = NA, colour = "red") +
  geom_text(aes(label = id, group = NULL), data = centres, size = 2)


# Investigate problem area
prob <- c("48.197", "48.155", "48.101", "48.269", "48.275")
probdf <- subset(tx, id %in% prob)

ggplot(probdf, aes(long, lat)) + 
  geom_polygon(aes(group = group), fill = NA, colour = "grey50") +
  geom_point(aes(colour = group), position = position_jitter(0.01, 0.01))

ggplot(probdf, aes(long, lat, group = group)) + 
  geom_polygon(fill = NA, colour = "grey50", size = 3) +
  geom_polygon(fill = NA, colour = "white", data = subset(probdf, tol > 0.1))+
  geom_point(aes(colour = factor(id)), position=position_jitter(0.01, 0.01)) 
  
# Missing a line segment from one change point to the next in 48.269
probdf2 <- subset(tx, id == "48.269")[c("long", "lat", "group", "count", "change", "tol")]

qplot(long, lat, data = probdf2, colour = factor(count))

probdf2$change2 <- with(probdf2, 
  diff(c(count[length(count)], count)) > 0 | count > 2
)
