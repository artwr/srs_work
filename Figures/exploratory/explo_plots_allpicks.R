##Exploratory plotting
setwd("D:/SRS/data_SRS/TCCZ_krig/R_work")
rm(list=ls())
require(scatterplot3d)

##
TCCZe<-readRDS("TCCZ.rdata")


scatterplot3d(TCCZe$UTM_N,y=TCCZe$UTM_E,z=TCCZe$TCCZ_top, type = "h",
              xlab = "UTM Easting", ylab = "SRS Northing",
              zlab = "Elevation TCCZ (ft)",
              main = "F-area Elevation Data", angle=50)