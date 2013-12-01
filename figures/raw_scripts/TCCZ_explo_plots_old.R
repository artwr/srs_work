##Exploratory plotting
#setwd("D:/work/Code/srs_work/TCCZ_krig/TCCZ")
rm(list=ls())
require(scatterplot3d)
require(geoR)

##
TCCZe<-readRDS("../../geo_data/processed/TCCZ_wtoppick.rdata")
summary(TCCZe)

scatterplot3d(x=TCCZe$UTM_N,y=TCCZe$UTM_E,z=TCCZe$TCCZ_top, type = "h",
              xlab = "UTM Easting", ylab = "UTM Northing",
              zlab = "Elevation TCCZ (ft)",
              main = "F-area Elevation Data", angle=80)

TCCZg<-as.geodata(TCCZe, coords.col=2:3, data.col=5)

TCCZge<-as.geodata(TCCZe, coords.col=2:3, data.col=4)

points(TCCZg, cex.min = 1, cex.max = 1, col = "gray")

points(TCCZge, cex.min = 2, cex.max = 2, col = "gray")

plot(TCCZge, lowess = T)

#points(TCCZg, cex.min = 1, cex.max = 4)