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


varofinterest <- c("TRITIUM", "NONVOLATILE BETA", "GROSS ALPHA", "NITRATE NITRITE AS NITROGEN", "SPECIFIC CONDUCTANCE", "IODINE 129", "URANIUM 235", "URANIUM 238", "CESIUM 137", "TECHNETIUM 99", "STRONTIUM 90", "TOTAL ACTIVITY", "PH")


resultssub <- subset(results, (ANALYTE_NAME %in% varofinterest))
resultsCsub <- resultsC[(resultsC$ANALYTE_NAME %in% varofinterest), ]
# Use a subset of the columns to simply the cross tabulation/casting

names(resultssub)
names(results[, c(1, 20, 27, 14, 15)])
results.molten <- results[, c(1, 20, 27, 14, 15)]
names(results.molten)
# 
results.wide.test <- dcast(results.molten, STATION_SEQ + ZDATE + SAMPLE_SEQ ~ RESULT, mean, margins = results.molten$ANALYTE_NAME)
# results.wide <- 

# stations <- 

names(results)
