TCCZdata<-read.csv("raw/TCCZ_nearF.csv")
TCCZdatac<-TCCZdata
##Convert non meaningful zeros to NAs
TCCZdatac[TCCZdatac==0]<-NA

##Save as flat object
saveRDS(TCCZdatac, file = "processed/TCCZ_0toNA.rdata")

colnames(TCCZdatac)[1:3]<-c("STATION_ID","EASTING","NORTHING")
colnames(TCCZdatac)

# Remove the points with no TCCZ pick. 
TCCZdatac2<-TCCZdatac[!is.na(TCCZdatac$TCCZ_top),]

##Save to compressed object
saveRDS(TCCZdatac, file = "processed/TCCZ_all.rdata")
saveRDS(TCCZdatac2, file = "processed/TCCZ_wtoppick.rdata")
