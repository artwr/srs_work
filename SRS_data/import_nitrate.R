#require(doBy)
require(plyr)

rm(list=ls())
#read the data
iodine.raw<-read.csv("iodine4R.csv")
strontium.raw<-read.csv("strontium4R.csv")
cesium137.raw<-read.csv("cesium1374R.csv")
technetium.raw<-read.csv("technetium4R.csv")
nitrate.raw<-read.csv("nitrate4R.csv")
nitrateC.raw<-read.csv("nitrateC4R.csv")
# seeextent <- ggplot(data=tritiumC.raw) + geom_point(aes(x=EASTING,y=NORTHING))
# print(seeextent)

#Remove FEX8 because of the 2 screens.
iodine.clean1<-iodine.raw[!iodine.raw$STATION_ID=='FEX  8',]
strontium.clean1<-strontium.raw[!strontium.raw$STATION_ID=='FEX  8',]
cesium137.clean1<-cesium137.raw[!cesium137.raw$STATION_ID=='FEX  8',]
technetium.clean1<-technetium.raw[!technetium.raw$STATION_ID=='FEX  8',]
nitrate.clean1<-nitrate.raw[!nitrate.raw$STATION_ID=='FEX  8',]
nitrateC.clean1<-nitrateC.raw[!nitrateC.raw$STATION_ID=='FEX  8',]

#Replace Negative values with NA
#Nitrate
nitrate.clean1$RESULT[nitrate.clean1$RESULT<0]<-NA
nitrateC.clean1$RESULT[nitrateC.clean1$RESULT<0]<-NA
nitrate.clean<-nitrate.clean1[!is.na(nitrate.clean1$RESULT),]
nitrateC.clean<-nitrateC.clean1[!is.na(nitrateC.clean1$RESULT),]

nitrate.clean1$RESULT[nitrate.clean1$RESULT<0]<-NA
nitrate.clean<-nitrate.clean1[!is.na(nitrate.clean1$RESULT),]

iodine.clean1$RESULT[iodine.clean1$RESULT<0]<-NA
iodine.clean<-iodine.clean1[!is.na(iodine.clean1$RESULT),]
strontium.clean1$RESULT[strontium.clean1$RESULT<0]<-NA
strontium.clean<-strontium.clean1[!is.na(strontium.clean1$RESULT),]
cesium137.clean1$RESULT[cesium137.clean1$RESULT<0]<-NA
cesium137.clean<-cesium137.clean1[!is.na(cesium137.clean1$RESULT),]
technetium.clean1$RESULT[technetium.clean1$RESULT<0]<-NA
technetium.clean<-technetium.clean1[!is.na(technetium.clean1$RESULT),]

#Using plyr

nitrate.plyr1<-ddply(nitrate.clean, c('STATION_SEQ','STATION_ID','MYEAR','EASTING','NORTHING'), function(x) c(count=nrow(x),mean=mean(x$RESULT),median=median(x$RESULT),sd=sd(x$RESULT),mad=mad(x$RESULT),min=min(x$RESULT),max=max(x$RESULT)))
iodine.plyr1<-ddply(iodine.clean, c('STATION_SEQ','STATION_ID','MYEAR','EASTING','NORTHING'), function(x) c(count=nrow(x),mean=mean(x$RESULT),median=median(x$RESULT),sd=sd(x$RESULT),mad=mad(x$RESULT),min=min(x$RESULT),max=max(x$RESULT)))
strontium.plyr1<-ddply(strontium.clean, c('STATION_SEQ','STATION_ID','MYEAR','EASTING','NORTHING'), function(x) c(count=nrow(x),mean=mean(x$RESULT),median=median(x$RESULT),sd=sd(x$RESULT),mad=mad(x$RESULT),min=min(x$RESULT),max=max(x$RESULT)))
cesium137.plyr1<-ddply(cesium137.clean, c('STATION_SEQ','STATION_ID','MYEAR','EASTING','NORTHING'), function(x) c(count=nrow(x),mean=mean(x$RESULT),median=median(x$RESULT),sd=sd(x$RESULT),mad=mad(x$RESULT),min=min(x$RESULT),max=max(x$RESULT)))
technetium.plyr1<-ddply(technetium.clean, c('STATION_SEQ','STATION_ID','MYEAR','EASTING','NORTHING'), function(x) c(count=nrow(x),mean=mean(x$RESULT),median=median(x$RESULT),sd=sd(x$RESULT),mad=mad(x$RESULT),min=min(x$RESULT),max=max(x$RESULT)))
nitrate.plyra<-ddply(nitrate.clean, c('MYEAR'), function(x) c(count=nrow(x),mean=mean(x$RESULT),median=median(x$RESULT),sd=sd(x$RESULT),mad=mad(x$RESULT),min=min(x$RESULT),max=max(x$RESULT)))
nitrateC.plyr1<-ddply(nitrateC.clean, c('STATION_SEQ','STATION_ID','MYEAR','EASTING','NORTHING'), function(x) c(count=nrow(x),mean=mean(x$RESULT),median=median(x$RESULT),sd=sd(x$RESULT),mad=mad(x$RESULT),min=min(x$RESULT),max=max(x$RESULT)))
nitrateC.plyra<-ddply(nitrateC.clean, c('MYEAR'), function(x) c(count=nrow(x),mean=mean(x$RESULT),median=median(x$RESULT),sd=sd(x$RESULT),mad=mad(x$RESULT),min=min(x$RESULT),max=max(x$RESULT)))


#Save as R datasets
saveRDS(nitrate.clean,file="nitratecleanall.rdata")
saveRDS(iodine.clean,file="iodinecleanall.rdata")
saveRDS(strontium.clean,file="strontiumcleanall.rdata")
saveRDS(cesium137.clean,file="cesium137cleanall.rdata")
saveRDS(technetium.clean,file="technetiumcleanall.rdata")
saveRDS(iodine.plyr1, file = "iodine.rdata")
saveRDS(strontium.plyr1, file = "strontium.rdata")
saveRDS(cesium137.plyr1, file = "cesium137.rdata")
saveRDS(technetium.plyr1, file = "technetium.rdata")
saveRDS(nitrate.plyr1, file = "nitrate.rdata")
saveRDS(nitrateC.plyr1, file = "nitrateC.rdata")
saveRDS(nitrate.plyra, file = "nitrateavg.rdata")
saveRDS(nitrateC.plyra, file = "nitrateCavg.rdata")


