pairs(TCCZe[,c("EASTING","NORTHING","g_elev_ft","TCCZ_top")])
TCCZe<-readRDS("./geo_data/processed/TCCZ_wtoppick.rdata")
picks<-readRDS("./geo_data/processed/picks_all.rdata")

pairs(picks[,c("EASTING", "NORTHING", "g_elev_ft", "Barnwell_top", "TCCZ_top", "Santee_top")])
picks[is.na(picks$TCCZ_top), ]
