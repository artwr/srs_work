#Clean_data
setwd("D:/work/Code/srs_work/TCCZ_krig/TCCZ")

TCCZdata<-read.csv("TCCZ_nearF.csv")
TCCZdatac<-TCCZdata
##Convert non meaningful zeros to NAs
TCCZdatac[TCCZdatac==0]<-NA

colnames(TCCZdatac)[1:3]<-c("STATION_ID","EASTING","NORTHING")
colnames(TCCZdatac)
##Save to compressed object
saveRDS(TCCZdatac, file = "TCCZ_o.rdata")