##Plot Haruko's data
require(ggplot2)
require(lattice)
require(geoR)
rm(list=ls())
TCCZkH.tmp<-read.table("../../geo_data/raw/TCCZ_krigged.txt",sep="\t", header=TRUE)
TCCZkH<-TCCZkH.tmp[order(TCCZkH.tmp$X, TCCZkH.tmp$Y),]
head(TCCZkH)
# persp(TCCZkH$X, TCCZkH$Y, TCCZkH$Ztccz, phi = 45, theta = 45,
#       xlab = "UTM E (m)", ylab = "UTM N (m)",
#       main = "TCCZ top elevation data"
# )
summary(TCCZkH)

hist(TCCZkH$Ztccz)

wireframe(Ztccz ~ X * Y, data=TCCZkH)

levelplot(Ztccz ~ X * Y, data=TCCZkH, xlab = "UTM E (m)", ylab = "UTM N (m)",main = "TCCZ top elevation data", col.regions = terrain.colors(100), aspect="xy")

#TCCZ.loess <- loess(Ztccz ~ X*Y, data = TCCZkH, degree = 2, span = 0.25)

#image(TCCZkH$X, TCCZkH$Y, TCCZkH$Ztccz, xlab = "UTM E (m)", ylab = "UTM N (m)",main = "TCCZ top elevation data")