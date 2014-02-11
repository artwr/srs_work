# Import the results tables and convert to wide form with aggregation on sample_seq

require(reshape2)
require(plyr)
# require(dplyr)
require(data.table)

# Load the cleaned up version of the tables
results <- readRDS("./srs_data/processed/resultsnonegnona.rdata")
resultsC <- readRDS("./srs_data/processed/resultsCnonegnona.rdata")


## Separate the table through Boyce Codd Normal Decomposition
# Create a stations object
stationswithscreen <- unique(as.data.table(results[, c("STATION_SEQ", "STATION_ID", "EASTING", "NORTHING", "GROUND_ELEVATION", "REFERENCE_ELEVATION", "TOP_ELEV", "BOTTOM_ELEV")]))
# Note that 3 stations have 2 screens FEX8 FEX9 and FOB 16D
stations <- unique(as.data.table(results[, c("STATION_SEQ", "STATION_ID", "EASTING", "NORTHING", "GROUND_ELEVATION", "REFERENCE_ELEVATION")]))
saveRDS(stationswithscreen, file = "./srs_data/processed/stationswithscreen.rdata")
saveRDS(stations, file = "./srs_data/processed/stations.rdata")

# Create a stations object
stationsCwithscreen <- unique(as.data.table(resultsC[, c("STATION_SEQ", "STATION_ID", "EASTING", "NORTHING", "GROUND_ELEVATION", "REFERENCE_ELEVATION", "TOP_ELEV", "BOTTOM_ELEV")]))
# 
stationsC <- unique(as.data.table(resultsC[, c("STATION_SEQ", "STATION_ID", "EASTING", "NORTHING", "GROUND_ELEVATION", "REFERENCE_ELEVATION")]))
saveRDS(stationsCwithscreen, file = "./srs_data/processed/stationsCwithscreen.rdata")
saveRDS(stationsC, file = "./srs_data/processed/stationsC.rdata")

# Create an analyte table to store the units
analyte_units <- unique(as.data.table(results[, c("ANALYTE_NAME", "RESULT_UNITS")]))
length(unique(results$ANALYTE_NAME))
length(unique(results$RESULT_SEQ))



analyte_units <- analyte_units[order(analyte_units$ANALYTE_NAME),]
# write.table(analyte_units$ANALYTE_NAME, file = "./srs_data/processed/analytenames.txt", sep = "|", row.names = FALSE)

# Use a subset of the columns to simply the cross tabulation/casting

names(results[, c(1, 27)])
results.molten <- results[ , c(1, 5:8, 10, 13)]
names(results.molten)
# 
dcast(results.molten, STATION_SEQ + ZDATE + SAMPLE_SEQ ~ ANALYTE_NAME, mean)
# results.wide <- 

# stations <- 

names(results)
