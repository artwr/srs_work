fmbpoly27<-read.table("../basin_coords/fmbpoly_utm27.txt", sep="\t", col.names=c("EASTING","NORTHING"))
saveRDS(fmbpoly27,"../basin_coords/fmbpoly27.rdata")