require(plyr)
#require(reshape2)

rm(list=ls())
#read the data
corr.raw<-read.csv("./srs_data/raw/corr4R.csv")

head(corr.raw)

#c("STATION_SEQ","STATION_ID","EASTING","NORTHING","LATITUDE","LONGITUDE","GROUND_ELEVATION","REFERENCE_ELEVATION","TOP_ELEV","BOTTOM_ELEV","MDATE","MYEAR","MMONTH","MDAY","MQUARTER")

#MQUARTER = floor(MMONTH/3+2/3)
#Aggregate by year+quarters

#Aggregate by year












