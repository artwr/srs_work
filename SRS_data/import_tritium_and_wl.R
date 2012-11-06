require(doBy)
require(plyr)

#read the data
tritium.raw<-read.csv("tritium4R.csv")

wl.raw<-read.csv("wl4R.csv")

#Compute aggregates per station, year
#tritium.avg<-ddply(tritium.raw, c("STATION_SEQ","MYEAR"), colMeans)
tritium.avg<-aggregate(cbind(RESULT, PQL) ~ STATION_SEQ + STATION_ID + EASTING + NORTHING + LATITUDE + LONGITUDE + MYEAR,tritium.raw,mean, na.action = na.pass)

tritium.sd<-aggregate(cbind(RESULT, PQL) ~ STATION_SEQ + STATION_ID + EASTING + NORTHING + LATITUDE + LONGITUDE + MYEAR,tritium.raw, sd, na.action = na.pass)

tritium.mad<-aggregate(cbind(RESULT, PQL) ~ STATION_SEQ + STATION_ID + EASTING + NORTHING + LATITUDE + LONGITUDE + MYEAR,tritium.raw,mad, na.action = na.pass)

tritium.summary1<-summaryBy(RESULT ~ STATION_SEQ + STATION_ID + MYEAR + EASTING + NORTHING, data=tritium.raw, FUN=function(x) c(count=length(x), mean=mean(x), median=median(x), sd=sd(x), mad=mad(x)))

tritium.plyr1<-ddply(tritium.raw, c('STATION_SEQ','STATION_ID','MYEAR','EASTING','NORTHING'), function(x) c(count=nrow(x),mean=mean(x$RESULT),median=median(x$RESULT),sd=sd(x$RESULT)))
#format(tritium.raw$LATITUDE[1:10], digits=10)
#For the water levels
wl.plyr1<-ddply(wl.raw, c('STATION_SEQ','STATION_ID','MYEAR','EASTING','NORTHING'), function(x) c(count=nrow(x),mean=mean(x$WATER_ELEV),median=median(x$WATER_ELEV),sd=sd(x$WATER_ELEV)))
wl.plyra<-ddply(wl.raw, c('STATION_SEQ','STATION_ID','EASTING','NORTHING'), function(x) c(count=nrow(x),mean=mean(x$WATER_ELEV),median=median(x$WATER_ELEV),sd=sd(x$WATER_ELEV),min=min(x$WATER_ELEV),max=max(x$WATER_ELEV)))


#Save as R datasets
saveRDS(tritium.plyr1, file = "tritium.rdata")
saveRDS(wl.plyr1, file = "wl.rdata")
saveRDS(wl.plyra, file = "wlavg.rdata")



