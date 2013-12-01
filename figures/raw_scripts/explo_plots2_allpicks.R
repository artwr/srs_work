##Exploratory plotting
# setwd("D:/SRS/data_SRS/TCCZ_krig/R_work")
rm(list=ls())

require(scatterplot3d)
require(geoR)
##
TCCZ<-readRDS("../../geo_data/processed/TCCZ.rdata")

scatterplot3d(TCCZ$EASTING,y=TCCZ$NORTHING,z=TCCZ$TCCZ_top, type = "h",
              xlab = "UTM Easting", ylab = "UTM Northing",
              zlab = "Elevation TCCZ (ft)",
              main = "F-area Elevation Data", angle=50)

grd_elev<-as.geodata(TCCZ, coords.col = 2:3, data.col = 4)

TCCZelev<-as.geodata(TCCZ, coords.col = 2:3, data.col=6)

TCCZt<-as.geodata(TCCZ, coords.col = 2:3, data.col = 7)

points(grd_elev, cex.min = 1, cex.max = 4)

points(grd_elev, cex.min = 2, cex.max =2, pt.div = "quint")

points(TCCZelev, cex.min = 2, cex.max =2, pt.div = "quint")

points(TCCZt, cex.min = 1, cex.max = 4)




