# Shapefiles downloaded from http://www.census.gov/geo/www/cob/cs2000.html
# Description of data at http://www.census.gov/geo/www/cob/cs_metadata.html

library(maptools)
library(plyr)

if (!file.exists("shapes.rdata")) {
  files <- dir("shape-files", full = T, pattern = "\\.shp")
  shapes <- llply(files, readShapeSpatial, .progress = "text")
  save(shapes, file = "shapes.rdata")  
} else {
  load("shapes.rdata")
  
}

get_attributes <- function(shape) {
  attr <- as.data.frame(shape)
  names(attr) <- tolower(names(attr))
  
  attr
}

# Extract county attributes
attributes <- ldply(shapes, get_attributes, .progress = "text")
attributes <- unique(attributes[, c("state", "county", "name", "area", "perimeter")])
write.table(attributes, "county-attr.csv", col=T, row=F, sep=",")

# Extract boundaries
raw <- ldply(shapes, fortify, region = c("STATE", "COUNTY"), 
  .progress = "text")
write.table(raw, "county-boundaries-raw.csv", col=T, row=F, sep=",")
