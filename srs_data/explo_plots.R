##Exploratory plotting
setwd("D:/SRS/data_SRS/TCCZ_krig/R_work")
rm(list=ls())
require(scatterplot3d)
require(geoR)

##
tritium<-readRDS("tritium.rdata")
wl<-readRDS("wl.rdata")


# scatterplot3d(TCCZe$UTM_N,y=TCCZe$UTM_E,z=TCCZe$TCCZ_top, type = "h",
#               xlab = "UTM Easting", ylab = "SRS Northing",
#               zlab = "Elevation TCCZ (ft)",
#               main = "F-area Elevation Data", angle=50)

wlgd.mean<-as.geodata(wl[wl$MYEAR==1992,], coords.col=4:5, data.col=6)
wlgd.median<-as.geodata(wl[wl$MYEAR==1992,], coords.col=4:5, data.col=7)

points(wlgd.mean, cex.min = 1, cex.max = 1, col = "gray")
