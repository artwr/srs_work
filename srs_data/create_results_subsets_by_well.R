# create subsets by well
require(data.table)

results <- readRDS("./srs_data/processed/resultsclean.rdata")
resultsC <- readRDS("./srs_data/processed/resultsCclean.rdata")

resultsDT <- as.data.table(results)
resultsCDT <- as.data.table(resultsC)

stationsDT <- as.data.table(results[,1:2])
stationsCDT <- as.data.table(resultsC[,1:2])

setkey(resultsDT,STATION_SEQ)
setkey(resultsCDT,STATION_SEQ)
# anames <- unique(resultsDT$ANALYTE_NAME)
# tables()
stationsDT <- unique(stationsDT, by = c("STATION_SEQ", "STATION_ID"))
stationsCDT <- unique(stationsCDT, by = c("STATION_SEQ", "STATION_ID"))
stationsDT$filetag <- gsub("[ -]","", stationsDT$STATION_ID)
stationsCDT$filetag <- gsub("[ -]","", stationsCDT$STATION_ID)



for(i in 1:length(stationsDT$STATION_SEQ)) {
  resultswella <- resultsDT[J(stationsDT$STATION_SEQ[i])]
  resultswella <- droplevels(resultswella)
  saveRDS(resultswella,paste0("srs_data/processed/bywell/results_", stationsDT$filetag[i],".rdata"))
}

for(iC in 1:length(stationsCDT$STATION_SEQ)) {
  resultsCwella <- resultsCDT[J(stationsCDT$STATION_SEQ[iC])]
  resultsCwella <- droplevels(resultsCwella)
  saveRDS(resultsCwella,paste0("srs_data/processed/bywell/resultsC_", stationsCDT$filetag[iC],".rdata"))
}

rm(list=ls())

message("Wells RDS files created")