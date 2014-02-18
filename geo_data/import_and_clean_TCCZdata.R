#TCCZdata<-read.csv("./geo_data/raw/TCCZ_nearF.csv")
TCCZdata<-read.csv("./geo_data/processed/TCCZ_definitive_all.csv")
##Convert non meaningful zeros to NAs
TCCZdata[TCCZdata==0]<-NA
# Remove the points with no TCCZ pick. 
TCCZdata2<-TCCZdata[!is.na(TCCZdata$TCCZ_top),]

##Save as flat object
# saveRDS(TCCZdata, file = "./geo_data/processed/TCCZ_all.rdata")
saveRDS(TCCZdata, file = "./geo_data/processed/TCCZ_definitive_all.rdata")
saveRDS(TCCZdata2, file = "./geo_data/processed/TCCZ_wtoppick.rdata")

TCCZ_nearF <- TCCZdata2[TCCZdata2$EASTING > 435000 & TCCZdata2$EASTING < 438000 & TCCZdata2$NORTHING < 3682700 & TCCZdata2$NORTHING > 3680500, ]

saveRDS(TCCZ_nearF, "./geo_data/processed/TCCZ_nearF_final.rdata")
