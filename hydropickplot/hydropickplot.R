##Plot aquifer
setwd("D:/SRS/data_SRS/R_data_files")
# Load data
srs1<-read.table("hydropicks.dat",skip = 1, header = FALSE)
names(srs1)<-c("x","y","Surface","TCCZ","LAZ","GCU","GAU","MBCS","well")
library(scatterplot3d)

s3d1<-scatterplot3d(srs1$x,srs1$y,srs1$Surface, type = "h", angle=30,
			  zlim=c(0,300),
              xlab = "Easting (ft)", ylab = "Northing (ft)",
              zlab = "Elevation (ft)",
              main = "F-area")

s3d1$points3d(srs1$x,srs1$y,srs1$TCCZ,col="red",type="h",pch=8)
s3d1$points3d(srs1$x,srs1$y,srs1$LAZ,col="green",type="h",pch=8)
s3d1$points3d(srs1$x,srs1$y,srs1$GCU,col="blue",type="h",pch=8)
s3d1$points3d(srs1$x,srs1$y,srs1$GAU,col="lightblue",type="h",pch=8)
s3d1$points3d(srs1$x,srs1$y,srs1$MBCS,col="purple",type="h",pch=8)

##