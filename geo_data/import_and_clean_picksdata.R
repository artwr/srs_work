##read table
picks_data<-read.csv("geo_data/raw/picks_nearF.csv")

##Convert non meaningful zeros to NAs
picks_data[picks_data==0]<-NA

colnames(picks_data)[1:3]<-c("STATION_ID","EASTING","NORTHING")
colnames(picks_data)



saveRDS(picks_data, file = "geo_data/processed/picks_all.rdata")
