require(plyr)
require
#require(reshape2)

rm(list=ls())
#read the data
corr.raw<-read.csv("./srs_data/raw/corr4R.csv", header = TRUE, sep = "|")
# dim(corr.raw)
corr.raw$MDATE<-as.Date(corr.raw$MDATE)
# head(corr.raw)
# str(corr.raw)
saveRDS(corr.raw, file = "srs_data/processed/corrrawwdates.rdata")

# Clean the data from the negative values, useless in this case.
corr.temp <- corr.raw
DataValuesMatrix <- as.matrix(corr.temp[, 16:100])
DataValuesMatrix[DataValuesMatrix<0] <- NA
corr.temp[, 16:100] <- DataValuesMatrix

corr.clean <- corr.temp[rowSums(is.na(corr.temp[, 16:100])) != ncol(corr.temp[, 16:100]),]


saveRDS(corr.clean, file = "srs_data/processed/corrclean.rdata")

rm(corr.raw)
rm(corr.temp)
rm(corr.clean)

#c("STATION_SEQ","STATION_ID","EASTING","NORTHING","LATITUDE","LONGITUDE","GROUND_ELEVATION","REFERENCE_ELEVATION","TOP_ELEV","BOTTOM_ELEV","MDATE","MYEAR","MMONTH","MDAY","MQUARTER")

#MQUARTER = floor(MMONTH/3+2/3)
#Aggregate by year+quarters

#Aggregate by year












