# import_and_clean_results

require(data.table)

# results.raw <- read.csv("./srs_data/raw/results4R.csv",sep="|", strip.white = TRUE, nrows = 100)
# resultsC.raw <- read.csv("./srs_data/raw/resultsC4R.csv",sep="|", strip.white = TRUE, nrows = 100)

# names(results.raw)
# "STATION_SEQ"         "STATION_ID"          "EASTING"             "NORTHING"           
# [5] "LATITUDE"            "LONGITUDE"           "ANALYSIS_DATE"       "COLLECTION_DATE"    
# [9] "MDATE"               "MYEAR"               "MMONTH"              "MDAY"               
# [13] "MQUARTER"            "ANALYTE_NAME"        "RESULT"              "RESULT_UNITS"       
# [17] "PQL"                 "PQL_UNITS"           "RESULT_SEQ"          "SAMPLE_SEQ"         
# [21] "GROUND_ELEVATION"    "REFERENCE_ELEVATION" "TOP_ELEV"            "BOTTOM_ELEV" 

resultsrawClasses <- c("factor", "factor", "numeric", "numeric", "numeric", "numeric", "character", "character", "Date", "integer", "integer", "integer", "integer", "factor", "numeric", "character", "numeric", "character", "factor", "factor", "numeric", "numeric", "numeric", "numeric")

results.raw <- read.csv("./srs_data/raw/results4R.csv",sep="|", strip.white = TRUE, colClasses = resultsrawClasses)
resultsC.raw <- read.csv("./srs_data/raw/resultsC4R.csv",sep="|", strip.white = TRUE, colClasses = resultsrawClasses)


# results.raw$PQL_UNITS[results.raw$PQL_UNITS == ""] 
# Testing that the PQL_UNITS are the same as the RESULT_UNITS, except when PQL = NA
# all.equal(results.raw$RESULT_UNITS, results.raw$PQL_UNITS)
# identical(results.raw$RESULT_UNITS, results.raw$PQL_UNITS)
# head(results.raw[results.raw$RESULT_UNITS != results.raw$PQL_UNITS, ])
# head(results.raw[results.raw$PQL_UNITS == "" & !is.na(results.raw$PQL),])


# Remove MDATE
# results <- results.raw[, c(1:8, 14:24)]

resultsDT <- as.data.table(results.raw)
resultsCDT <- as.data.table(resultsC.raw)

# Make the conversion to Date objects
resultsDT$CDATE <- as.Date(resultsDT$COLLECTION_DATE)
resultsDT$ADATE <- as.Date(resultsDT$ANALYSIS_DATE)
resultsCDT$CDATE <- as.Date(resultsCDT$COLLECTION_DATE)
resultsCDT$ADATE <- as.Date(resultsCDT$ANALYSIS_DATE)

# Create ZDATE which will be the reference date for comparison between datapoints
# and ZYEAR for yearly aggregation
resultsDT$ZDATE <- resultsDT$CDATE
resultsDT$ZDATE[is.na(resultsDT$ZDATE)] <- resultsDT$ADATE[is.na(resultsDT$ZDATE)]
resultsDT$ZYEAR <- as.integer(format(resultsDT$ZDATE,"%Y"))
resultsCDT$ZDATE <- resultsCDT$CDATE
resultsCDT$ZDATE[is.na(resultsCDT$ZDATE)] <- resultsCDT$ADATE[is.na(resultsCDT$ZDATE)]
resultsCDT$ZYEAR <- as.integer(format(resultsCDT$ZDATE,"%Y"))
saveRDS(as.data.frame(resultsDT), "./srs_data/processed/results.rdata")
saveRDS(as.data.frame(resultsCDT), "./srs_data/processed/resultsC.rdata")

message("Date column types created, results saved as RDS object","\r",appendLF=TRUE)

rm(list = c("results.raw", "resultsC.raw", "resultsDT", "resultsCDT", "resultsrawClasses"))
