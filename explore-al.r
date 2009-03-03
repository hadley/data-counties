al <- subset(raw, substr(id, 0, 2) == "2.")

# Weird error in data?
al <- transform(al, long = ifelse(long > 150, -long, long))

ggplot(al, aes(long, lat, group = group)) + 
  geom_polygon(fill = NA, colour = "grey50") 

centres <- ddply(al, .(id), colwise(midrng, .(long, lat)))

al$hash <- paste(al$long, al$lat)
if (is.null(al$order)) al$order <- 1:nrow(al)
neighbours <- ddply(al, .(hash), nrow)
names(neighbours) <- c("hash", "count")

al <- merge(al, neighbours, by = "hash")
al <- al[order(al$order), ]
al <- ddply(al, .(group), transform, 
  change = diff(c(count[length(count)], count)) > 0
)

ggplot(al, aes(long, lat, group = group)) + 
  geom_polygon(fill = NA, colour = "grey50") +
  geom_point(aes(colour = factor(count)), subset(al, change))

al <- ddply(al, .(group), rotate)
ddply(al, .(group), function(df) df$change[c(1, nrow(df))])

al <- ddply(al, .(id), thin_poly, .progress = "text")
al$hash <- NULL


ggplot(subset(al, tol > 0.1), aes(long, lat, group = group)) + 
  geom_polygon(fill = "grey80", colour = NA) 