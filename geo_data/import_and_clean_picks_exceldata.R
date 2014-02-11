#
#
require(data.table)
picks_raw <- read.table( "./geo_data/raw/hydropick.txt" ,sep = "\t", header = TRUE)

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
# names(picks_all)
names(picks_all)[15:16] <- c("EASTING","NORTHING")

# picks_all[picks_all$WellID == "HSB-143C",c("EASTING","NORTHING")]
# picks_all[picks_all$WellID == "FSB-98A",c("EASTING","NORTHING")]
# picks_all[picks_all$WellID == "FSB-79A",c("EASTING","NORTHING")]

saveRDS(picks_all,"./geo_data/processed/picks_all_from_excel.rdata")
saveRDS(picks_all[,c("WellID","EASTING","NORTHING", "TCCZ_top")], "./geo_data/processed/TCCZ_all_from_excel.rdata")
