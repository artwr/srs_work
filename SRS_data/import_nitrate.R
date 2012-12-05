#require(doBy)
require(plyr)

rm(list=ls())
#read the data
nitrate.raw<-read.csv("nitrate4R.csv")
nitrateC.raw<-read.csv("nitrateC4R.csv")
# seeextent <- ggplot(data=tritiumC.raw) + geom_point(aes(x=EASTING,y=NORTHING))
# print(seeextent)

#Remove FEX8 because of the 2 screens.
nitrate.clean1<-nitrate.raw[!nitrate.raw$STATION_ID=='FEX  8',]
nitrateC.clean1<-nitrateC.raw[!nitrateC.raw$STATION_ID=='FEX  8',]

#Replace Negative values with NA
#Nitrate
nitrate.clean1$RESULT[nitrate.clean1$RESULT<0]<-NA
nitrateC.clean1$RESULT[nitrateC.clean1$RESULT<0]<-NA

nitrate.clean<-nitrate.clean1[!is.na(nitrate.clean1$RESULT),]
nitrateC.clean<-nitrateC.clean1[!is.na(nitrateC.clean1$RESULT),]

#Using plyr

nitrate.plyr1<-ddply(nitrate.clean, c('STATION_SEQ','STATION_ID','MYEAR','EASTING','NORTHING'), function(x) c(count=nrow(x),mean=mean(x$RESULT),median=median(x$RESULT),sd=sd(x$RESULT),mad=mad(x$RESULT),min=min(x$RESULT),max=max(x$RESULT)))
nitrate.plyra<-ddply(nitrate.clean, c('MYEAR'), function(x) c(count=nrow(x),mean=mean(x$RESULT),median=median(x$RESULT),sd=sd(x$RESULT),mad=mad(x$RESULT),min=min(x$RESULT),max=max(x$RESULT)))
nitrateC.plyr1<-ddply(nitrateC.clean, c('STATION_SEQ','STATION_ID','MYEAR','EASTING','NORTHING'), function(x) c(count=nrow(x),mean=mean(x$RESULT),median=median(x$RESULT),sd=sd(x$RESULT),mad=mad(x$RESULT),min=min(x$RESULT),max=max(x$RESULT)))
nitrateC.plyra<-ddply(nitrateC.clean, c('MYEAR'), function(x) c(count=nrow(x),mean=mean(x$RESULT),median=median(x$RESULT),sd=sd(x$RESULT),mad=mad(x$RESULT),min=min(x$RESULT),max=max(x$RESULT)))


#Save as R datasets
saveRDS(nitrate.clean,file="nitratecleanall.rdata")
saveRDS(nitrate.plyr1, file = "nitrate.rdata")
saveRDS(nitrateC.plyr1, file = "nitrateC.rdata")
saveRDS(nitrate.plyra, file = "nitrateavg.rdata")
saveRDS(nitrateC.plyra, file = "nitrateCavg.rdata")


