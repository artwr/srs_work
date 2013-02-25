#FSB 78, FSB 78C, decay constant
require(plyr)
require(sqldf)

#Load data
#read the data
corr.raw<-read.csv("../SRS_data/corr4R.csv")
corrC.raw<-read.csv("../SRS_data/corrC4R.csv")

# FSB 78
FSB78.raw<-corr.raw[corr.raw$STATION_ID=='FSB 78',c("STATION_ID","MDATE","MYEAR","TRITIUM","NITRATE.NITRITE.AS.NITROGEN")]

#FSB 78C
FSB78C.raw<-corrC.raw[corrC.raw$STATION_ID=='FSB 78C',c("STATION_ID","MDATE","MYEAR","TRITIUM","NITRATE.NITRITE.AS.NITROGEN")]

summary(FSB78.raw)

#Clean NA values
FSB78.clean<-FSB78.raw[!is.na(FSB78.raw$TRITIUM) | !is.na(FSB78.raw$NITRATE.NITRITE.AS.NITROGEN),]

FSB78C.clean<-FSB78C.raw[!is.na(FSB78C.raw$TRITIUM) | !is.na(FSB78C.raw$NITRATE.NITRITE.AS.NITROGEN),]

#Plyr magic aggregation
FSB78.plyr1<-ddply(FSB78.clean, c('STATION_ID','MYEAR'), function(x) c(meanT=mean(x$TRITIUM, na.rm = TRUE),medianT=median(x$TRITIUM , na.rm = TRUE),sdT=sd(x$TRITIUM, na.rm = TRUE),madT=mad(x$TRITIUM, na.rm = TRUE), meanNO3=mean(x$NITRATE.NITRITE.AS.NITROGEN, na.rm = TRUE),medianNO3=median(x$NITRATE.NITRITE.AS.NITROGEN, na.rm = TRUE),sdNo3=sd(x$NITRATE.NITRITE.AS.NITROGEN, na.rm = TRUE),madNO3=mad(x$NITRATE.NITRITE.AS.NITROGEN, na.rm = TRUE)))

FSB78C.plyr1<-ddply(FSB78C.clean, c('STATION_ID','MYEAR'), function(x) c(meanT=mean(x$TRITIUM, na.rm = TRUE),medianT=median(x$TRITIUM , na.rm = TRUE),sdT=sd(x$TRITIUM, na.rm = TRUE),madT=mad(x$TRITIUM, na.rm = TRUE), meanNO3=mean(x$NITRATE.NITRITE.AS.NITROGEN, na.rm = TRUE),medianNO3=median(x$NITRATE.NITRITE.AS.NITROGEN, na.rm = TRUE),sdNo3=sd(x$NITRATE.NITRITE.AS.NITROGEN, na.rm = TRUE),madNO3=mad(x$NITRATE.NITRITE.AS.NITROGEN, na.rm = TRUE)))

saveRDS(FSB78.plyr1, file = "FSB78.rdata")
saveRDS(FSB78C.plyr1, file = "FSB78C.rdata")



# #Replace Negative values with NA
# tritium.clean1[tritium.clean1$RESULT<0,]$RESULT<-NA
# tritiumC.clean1[tritiumC.clean1$RESULT<0,]$RESULT<-NA
# #tritium.see<-tritium.raw[tritium.raw$RESULT<0,]
# tritium.clean<-tritium.clean1[!is.na(tritium.clean1$RESULT),]
# tritiumC.clean<-tritiumC.clean1[!is.na(tritiumC.clean1$RESULT),]








