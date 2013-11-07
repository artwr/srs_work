#Importing CSV into R and doing the aggregation per year.

#require(doBy)
require(plyr)

rm(list=ls())
#read the data

# UAZ
tritium.raw<-read.table("tritium4R.csv",sep="|",header = TRUE)
#LAZ
tritiumC.raw<-read.table("tritiumC4R.csv",sep="|",header = TRUE)

# extentdomC <- ggplot(data=tritiumC.raw) + geom_point(aes(x=EASTING,y=NORTHING))
# print(extentdomC)

#Remove FEX8 because of the double screen
tritium.clean1<-tritium.raw[!tritium.raw$STATION_ID=='FEX  8',]
tritiumC.clean1<-tritiumC.raw[!tritiumC.raw$STATION_ID=='FEX  8',]

#Store FEX8 separate;y for further inspection
# tritium.FEX8<-tritium.raw[tritium.raw$STATION_ID=='FEX  8',]
# tritiumC.FEX8<-tritiumC.raw[tritiumC.raw$STATION_ID=='FEX  8',]
#Visual inspection of the data
#tritium.fex8<-tritium.raw[tritium.raw$STATION_ID=='FEX  8',]


#Replace Negative values with NA
tritium.clean1[tritium.clean1$RESULT<0,]$RESULT<-NA
tritiumC.clean1[tritiumC.clean1$RESULT<0,]$RESULT<-NA
#tritium.see<-tritium.raw[tritium.raw$RESULT<0,]
#Remove NA values since I cannot use them
tritium.clean<-tritium.clean1[!is.na(tritium.clean1$RESULT),]
tritiumC.clean<-tritiumC.clean1[!is.na(tritiumC.clean1$RESULT),]

#Maybe QC?


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
tritiumC.plyr1<-ddply(tritiumC.clean, c('STATION_SEQ','STATION_ID','MYEAR','EASTING','NORTHING'), function(x) c(count=nrow(x),mean=mean(x$RESULT),median=median(x$RESULT),sd=sd(x$RESULT),mad=mad(x$RESULT),min=min(x$RESULT),max=max(x$RESULT)))
tritiumC.plyra<-ddply(tritiumC.clean, c('MYEAR'), function(x) c(count=nrow(x),mean=mean(x$RESULT),median=median(x$RESULT),sd=sd(x$RESULT),mad=mad(x$RESULT),min=min(x$RESULT),max=max(x$RESULT)))


#Save as R datasets
saveRDS(tritium.clean,file="tritiumcleanall.rdata")
saveRDS(tritium.plyr1, file = "tritium.rdata")
saveRDS(tritiumC.plyr1, file = "tritiumC.rdata")
saveRDS(tritium.plyra, file = "tritiumavg.rdata")
saveRDS(tritiumC.plyra, file = "tritiumCavg.rdata")

# saveRDS(tritium.FEX8, file = "tritiumFEX8.rdata")
# saveRDS(tritiumC.FEX8, file = "tritiumCFEX8.rdata")




