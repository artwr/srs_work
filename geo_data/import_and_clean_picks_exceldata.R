#
#
require(data.table)
picks_raw <- read.table( "./geo_data/raw/hydropick.txt" ,sep = "\t", header = TRUE)
dim(picks_raw)
source('./geo_data/srs2utm.R')

utm1 <- c(437504.53, 3681779.12)
srs1 <- c(52773.20, 73738.20)
utm2 <- c(436555.12, 3681710.48)
srs2 <- c(50121.60, 75389.80)
utm3 <- c(436871.42, 3681290.47)
srs3 <- c(50149.60, 73664.50)

srsc <- rbind(srs1,srs2,srs3)
utmc <- rbind(utm1, utm2, utm3)

Mtransform <- findTransformationMatrix(srsc, utmc)

picksutm27coords <- srs2utm27(as.matrix(picks_raw[,c("SRSEasting", "SRSNorthing")]), Mtransform)
picks_all <- cbind(picks_raw, picksutm27coords)
# dim(picks_all)
# names(picks_all)
names(picks_all)[c(1,15,16)] <- c("STATION_ID","EASTING","NORTHING")
picks_all$STATION_ID <- sub("-","",picks_all$STATION_ID)
picks_all$TCCZ_bot <- picks_all$LAZ_top
picks_all$TCCZ_th_ft <- picks_all$TCCZ_top - picks_all$TCCZ_bot
saveRDS(picks_all,"./geo_data/processed/picks_all_from_excel.rdata")
write.csv(picks_all[,c("STATION_ID","EASTING","NORTHING","SurfaceElevation","TCCZ_top","TCCZ_bot","TCCZ_th_ft")],"./geo_data/processed/picks_all_from_excel_file.csv")
picks_all_nearF <- subset(picks_all, EASTING > 435000 & EASTING < 438000 & NORTHING < 3682700 & NORTHING > 3680500)
# picks_all[picks_all$WellID == "HSB-143C",c("EASTING","NORTHING")]
# picks_all[picks_all$WellID == "FSB-98A",c("EASTING","NORTHING")]
# picks_all[picks_all$WellID == "FSB-79A",c("EASTING","NORTHING")]

write.csv(picks_all[c("STATION_ID","EASTING","NORTHING",)],"./geo_data/processed/picks_all_from_excel_file.csv")
saveRDS(picks_all,"./geo_data/processed/picks_all_from_excel.rdata")
saveRDS(picks_all_nearF,"./geo_data/processed/picks_all_nearF_from_excel.rdata")
#
names(picks_all_nearF)
# picks_all<-readRDS("./geo_data/processed/picks_all_from_excel.rdata")
picks_all_nearF$TCCZ_bot <- picks_all_nearF$LAZ_top
picks_all_nearF$TCCZ_th_ft <- picks_all_nearF$TCCZ_top - picks_all_nearF$TCCZ_bot 
saveRDS(picks_all_nearF[,c("STATION_ID","EASTING","NORTHING","SurfaceElevation","TCCZ_top","TCCZ_bot","TCCZ_th_ft")], "./geo_data/processed/TCCZ_all_nearF_from_excel.rdata")
