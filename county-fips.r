# http://www.census.gov/datamap/fipslist/AllSt.txt
#
# This file lists all counties and equivalent areas in the United States
# defined as of January 1, 1990, alphabetically by State with related codes.
# There are four codes shown:  the first code is the FIPS MSA/CMSA/NECMA code,
# the second code is the FIPS PMSA code, the third code is a combination of 
# the FIPS State and county codes, and the fourth code is the geographic 
# summary level (0 = United States total; 1 = state total; 2 = county in CMSA/
# PMSA; 3 = county in MSA; 4 = county in NECMA; 5 = county not in any metro
# area).  (MSA = metropolitan statistical area; CMSA = consolidated MSA; 
# NECMA = New England county MA; PMSA = primary MSA)

fips <- read.fwf("county-fips.txt", c(8, 8, 8, 6, 30))

names(fips) <- c("msa", "pmsa", "fips", "geo", "name")
fips$geo <- NULL

# Tidy and split names
library(reshape)
fips$name <- as.character(fips$name)
fips <- fips[!is.na(fips$name), ]
fips <- fips[fips$name != toupper(fips$name), ]
fips <- cbind(fips, colsplit(fips$name, ", ", c("county", "state")))
fips$name <- NULL


lengths <- nchar(fips$fips)
fips$county_fips <- as.numeric(substr(fips$fips, lengths - 2, 5))
fips$state_fips <- as.numeric(substr(fips$fips, 1, lengths - 3))
fips$fips <- NULL

write.table(fips, "county-fips.csv", sep=",", row=F)