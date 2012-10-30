##Exploratory plotting
#setwd("D:/work/Code/srs_work/TCCZ_krig/TCCZ")
rm(list=ls())
require(scatterplot3d)
require(geoR)
require(akima)

##
TCCZe<-readRDS("TCCZ_o.rdata")
summary(TCCZe)

hist(TCCZe$TCCZ_top, breaks = 20)

scatterplot3d(x=TCCZe$UTM_N,y=TCCZe$UTM_E,z=TCCZe$TCCZ_top, type = "h",
              xlab = "UTM Easting", ylab = "UTM Northing",
              zlab = "Elevation TCCZ (ft)",
              main = "F-area Elevation Data", angle=80)

TCCZg<-as.geodata(TCCZe, coords.col=2:3, data.col=5)

TCCZge<-as.geodata(TCCZe, coords.col=2:3, data.col=4)

points(TCCZg, cex.min = 1, cex.max = 1, col = "gray")

points(TCCZge, cex.min = 2, cex.max = 2, col = "gray")

plot(TCCZge)

with(TCCZge, hist(data, main = "", xlab = "ground elevation"))

with(TCCZge, plot(coords[, 1], data, xlab = "UTM E", ylab = "ground elevation data", pch = 20, cex = 0.7))
lines(lowess(TCCZge$data ~ TCCZge$coords[, 1]))

with(TCCZge, plot(coords[, 2], data, xlab = "UTM N", ylab = "ground elevation data", pch = 20, cex = 0.7))
lines(with(TCCZge,lowess(data ~ coords[, 2])))

points(TCCZge, cex.max=2.5)
#Residuals of 
#
points(TCCZge, trend = "1st", pt.div = 2, abs = T, cex.max = 2.5)
points(TCCZge, trend = "2nd", pt.div = 2, abs = T, cex.max = 2.5)
#points(TCCZg, cex.min = 1, cex.max = 4)

TCCZ.vm<-voronoi.mosaic(TCCZe$UTM_E,y=TCCZe$UTM_N,duplicate="error")
plot.voronoi(TCCZ.vm)
TCCZ.vp <- voronoi.polygons(TCCZ.vm)
plot(TCCZ.vp)








