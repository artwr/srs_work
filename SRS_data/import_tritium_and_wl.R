#require(doBy)
require(plyr)

rm(list=ls())
#read the data
tritium.raw<-read.csv("tritium4R.csv")
wl.raw<-read.csv("wl4R.csv")

#Remove FEX8 because of the 2 screens.
wl.clean<-wl.raw[!wl.raw$STATION_ID=='FEX  8' & !wl.raw$STATION_ID=='FEX  9',]
#wl.clean<-wl.raw[!wl.raw$STATION_ID=='FEX  9',]
tritium.clean1<-tritium.raw[!tritium.raw$STATION_ID=='FEX  8',]

#Replace Negative values with NA
tritium.clean1[tritium.clean1$RESULT<0,]$RESULT<-NA
#tritium.see<-tritium.raw[tritium.raw$RESULT<0,]
tritium.clean<-tritium.clean1[!is.na(tritium.clean1$RESULT),]

#Visual inspection of the data
#wl.fex<-wl.raw[wl.raw$STATION_ID=='FEX  8' | wl.raw$STATION_ID=='FEX  9',]
#tritium.fex8<-tritium.raw[tritium.raw$STATION_ID=='FEX  8',]

#Compute aggregates per station, year
#
#tritium.avg<-aggregate(cbind(RESULT, PQL) ~ STATION_SEQ + STATION_ID + EASTING + NORTHING + LATITUDE + LONGITUDE + MYEAR,tritium.clean,mean, na.action = na.pass)

#tritium.sd<-aggregate(cbind(RESULT, PQL) ~ STATION_SEQ + STATION_ID + EASTING + NORTHING + LATITUDE + LONGITUDE + MYEAR,tritium.clean, sd, na.action = na.pass)

#tritium.mad<-aggregate(cbind(RESULT, PQL) ~ STATION_SEQ + STATION_ID + EASTING + NORTHING + LATITUDE + LONGITUDE + MYEAR,tritium.clean,mad, na.action = na.pass)

# Using doBy
#tritium.summary1<-summaryBy(RESULT ~ STATION_SEQ + STATION_ID + MYEAR + EASTING + NORTHING, data=tritium.clean, FUN=function(x) c(count=length(x), mean=mean(x), median=median(x), sd=sd(x), mad=mad(x)))

#Using plyr
tritium.plyr1<-ddply(tritium.clean, c('STATION_SEQ','STATION_ID','MYEAR','EASTING','NORTHING'), function(x) c(count=nrow(x),mean=mean(x$RESULT),median=median(x$RESULT),sd=sd(x$RESULT),mad=mad(x$RESULT),min=min(x$RESULT),max=max(x$RESULT)))
tritium.plyra<-ddply(tritium.clean, c('MYEAR'), function(x) c(count=nrow(x),mean=mean(x$RESULT),median=median(x$RESULT),sd=sd(x$RESULT),mad=mad(x$RESULT),min=min(x$RESULT),max=max(x$RESULT)))

#


#For the water levels
wl.plyr1<-ddply(wl.clean, c('STATION_SEQ','STATION_ID','MYEAR','EASTING','NORTHING'), function(x) c(count=nrow(x),mean=mean(x$WATER_ELEV),median=median(x$WATER_ELEV),sd=sd(x$WATER_ELEV),mad=mad(x$WATER_ELEV),min=min(x$WATER_ELEV),max=max(x$WATER_ELEV)))

#average across years
wl.plyra<-ddply(wl.clean, c('STATION_SEQ','STATION_ID','EASTING','NORTHING'), function(x) c(count=nrow(x),mean=mean(x$WATER_ELEV),median=median(x$WATER_ELEV),sd=sd(x$WATER_ELEV),mad=mad(x$WATER_ELEV),min=min(x$WATER_ELEV),max=max(x$WATER_ELEV)))


#Save as R datasets
saveRDS(tritium.plyr1, file = "tritium.rdata")
saveRDS(tritium.plyra, file = "tritiumavg.rdata")
saveRDS(wl.plyr1, file = "wl.rdata")
saveRDS(wl.plyra, file = "wlavg.rdata")



