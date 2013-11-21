fmbpoly27<-read.table("raw/fmbpoly_utm27.txt", sep="\t", col.names=c("EASTING","NORTHING"))
saveRDS(fmbpoly27,"processed/fmbpoly27.rdata")
