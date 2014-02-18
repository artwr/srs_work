#Import the water level data
#require(doBy)
require(plyr)


rm(list=ls())
#read the data
# wl.raw<-read.table("./srs_data/raw/wl4R.csv",sep="|",header = TRUE, nrows = 15)
wl.raw<-read.table("./srs_data/raw/wl4R.csv",sep="|",header = TRUE)
wld3.raw<-read.table("./srs_data/raw/wld34R.csv",sep="|",header = TRUE)
#Remove FEX8 because of the 2 screens.
wl.clean<-wl.raw[!wl.raw$STATION_ID=='FEX  8' & !wl.raw$STATION_ID=='FEX  9',]
wld3.clean<-wld3.raw[!wld3.raw$STATION_ID=='FEX  8' & !wld3.raw$STATION_ID=='FEX  9',]
#wl.clean<-wl.raw[!wl.raw$STATION_ID=='FEX  9',]
#Visual inspection of the data
#wl.fex<-wl.raw[wl.raw$STATION_ID=='FEX  8' | wl.raw$STATION_ID=='FEX  9',]

saveRDS(wl.clean, file = "./srs_data/processed/wlclean.rdata")
saveRDS(wld3.clean, file = "./srs_data/processed/wld3clean.rdata")

#For the water levels
wl.plyr1<-ddply(wl.clean, c('STATION_SEQ','STATION_ID','MYEAR','EASTING','NORTHING'), function(x) c(count=nrow(x),mean=mean(x$WATER_ELEV),median=median(x$WATER_ELEV),sd=sd(x$WATER_ELEV),mad=mad(x$WATER_ELEV),min=min(x$WATER_ELEV),max=max(x$WATER_ELEV)))

#average across years
wl.plyra<-ddply(wl.clean, c('STATION_SEQ','STATION_ID','EASTING','NORTHING'), function(x) c(count=nrow(x),mean=mean(x$WATER_ELEV),median=median(x$WATER_ELEV),sd=sd(x$WATER_ELEV),mad=mad(x$WATER_ELEV),min=min(x$WATER_ELEV),max=max(x$WATER_ELEV)))

saveRDS(wl.plyr1, file = "./srs_data/processed/wlavgpwpy.rdata")
saveRDS(wl.plyra, file = "./srs_data/processed/wlavgpw.rdata")

rm(list = ls())
