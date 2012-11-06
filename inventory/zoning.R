#Simple Computation
require(geoR)
require(geoRglm)
require(fields)
require(akima)
#require()
#load data
#rm(list=ls())
#tritium<-readRDS("tritium.rdata")
wl<-readRDS("wl.rdata")
TCCZe_all<-readRDS("TCCZ_o.rdata")
TCCZe<-TCCZe_all[!is.na(TCCZe_all$TCCZ_top),]
wlavg<-readRDS("wlavg.rdata")

#basin coords
f3basin<-readRDS("f3basin.rdata")
f3basin27<-readRDS("f3basin27.rdata")


#
selectyear<-function (x,y) {subset(x, x$MYEAR == y)}

#summary(tritium$MYEAR)
#summary(wl$MYEAR)

#tritium.1984<-selectyear(tritium, 1984)
#summary(tritium[tritium$STATION_ID=='FSB115D',])
#TCCZ.lm<-lm(TCCZ_top~UTM_E+UTM_N,data=TCCZe,na.action=na.omit)
#TCCZ.loess1 = loess(TCCZ_top~UTM_E+UTM_N, data = TCCZe, degree = 1, span = 0.25)
#TCCZ.loess2 = loess(TCCZ_top~UTM_E+UTM_N, data = TCCZe, degree = 2, span = 0.25)
#plot(TCCZ.lm)

#2D interpolation within the bounds of interest.
no.min<-3680929
no.max<-3682109
ea.min<-436175
ea.max<-437158

TCCZ.interp<-interp(TCCZe$UTM_E, TCCZe$UTM_N, TCCZe$TCCZ_top, xo=seq(ea.min, ea.max, length = 30), yo=seq(no.min, no.max, length = 50), linear = TRUE, extrap=FALSE, duplicate = "error")
wlavg.interp<-interp(wlavg$EASTING, wlavg$NORTHING, wlavg$mean, xo=seq(ea.min, ea.max, length = 30), yo=seq(no.min, no.max, length = 50), linear = TRUE, extrap=FALSE, duplicate = "mean")

#create aquifer thickness

#image and contour plot for the TCCZ
image.plot(TCCZ.interp)
contour(TCCZ.interp$x,TCCZ.interp$y,TCCZ.interp$z)
plot(f3basin$UTM_E,f3basin$UTM_N, add=TRUE)

#image and contour plot for the water level
image.plot(wlavg.interp)
contour(wlavg.interp$x,wlavg.interp$y,wlavg.interp$z)

#
#image and contour plot for the water level
#image.plot(wlavg.interp)
contour(wlavg.interp$x,wlavg.interp$y,wlavg.interp$z-TCCZ.interp$z, xlim=c(ea.min,ea.max), ylim=c(no.min,no.max), asp = 1, bty ="n")
#points(f3basin$UTM_E,f3basin$UTM_N,type="l")
points(f3basin27$UTM_E,f3basin27$UTM_N,type="l")
points(wlavg$EASTING, wlavg$NORTHING, pch=19)
text(wlavg$EASTING, wlavg$NORTHING, labels=wlavg$STATION_ID)

#Define n zones

#Compute trend planes in these zones for waterlevels and TCCZ

#Do the difference to get the thickness

