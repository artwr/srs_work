# Computation Functions
require(data.table)
require(plyr)

# getAnalyteResults <- function(analyte_name, results.df) {
#   require(data.table)
# #   if(!exists("results")) {results <- readRDS("srs_data/processed/resultsnonegnona.rdata")}
# #   if(!exists("resultsC")) {resultsC <- readRDS("srs_data/processed/resultsCnonegnona.rdata")}
#   resultsDT <- as.data.table(results.df)
#   setkey(resultsDT,ANALYTE_NAME)
#   <-resultsDT[paste()]
# }

aggregateAnalyteByYear <- function(analyte.df){
  analyte.aggreg<-ddply(analyte.df, c('STATION_SEQ','STATION_ID','ZYEAR','EASTING','NORTHING'), function(x) c(count=nrow(x),mean=mean(x$RESULT),median=median(x$RESULT),sd=sd(x$RESULT),mad=mad(x$RESULT),min=min(x$RESULT),max=max(x$RESULT)))
}