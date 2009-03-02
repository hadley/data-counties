# Shapefiles downloaded from http://www.census.gov/geo/www/cob/cs2000.html
# Description of data at http://www.census.gov/geo/www/cob/cs_metadata.html

library(maptools)
library(plyr)

# files <- dir("shape-files", full = T, pattern = "\\.shp")
# shapes <- llply(files, readShapeSpatial, .progress = "text")
# save(shapes, file = "shapes.rdata")

load("shapes.rdata")
l(ggplot)

get_attributes <- function(shape) {
  attr <- as.data.frame(shape)
  names(attr) <- tolower(names(attr))
  
  attr
}
source("extract.r")

# Extract county attributes
attributes <- ldply(shapes, get_attributes, .progress = "text")
attributes <- unique(attributes[, c("state", "county", "name")])
# Doesn't work! :(
write.table(attributes, "county-attr.csv", col=T, row=F, sep=",")


# Extract and thin 
raw <- ldply(shapes, get_borders, tol = 0, .progress = "text")
write.table(raw, "county-boundaries-raw.csv", col=T, row=F, sep=",")


fine <- ldply(shapes, get_borders, tol = 0.01, .progress = "text")
coarse <- ldply(shapes, get_borders, tol = 0.5, .progress = "text")

write.table(coarse, "county-boundaries.csv", col=T, row=F, sep=",")

library(ggplot2)
qplot(x, y, data=coarse, geom="path", group = id)
qplot(x, y, data=coarse, geom="path", group = id, xlim=c(-130, -70), ylim=c(25, 50))
qplot(x, y, data=coarse, geom="polygon", group = id, xlim=c(-130, -70), ylim=c(25, 50))
