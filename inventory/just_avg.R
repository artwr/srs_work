#Simple Computation
require(geoR)
require(geoRglm)
require(fields)
require(akima)
require(splancs)
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
wll<-split(wl,wl$MYEAR)

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

d.ea<-c(436175,436175,437158,438158,436175)
d.no<-c(3680929,3682109,3682109,3680929,3680929)
pp<-cbind(d.ea,d.no)
#plot(pp, type="b")
area.dom<-areapl(pp)

#

TCCZ.interp<-interp(TCCZe$UTM_E, TCCZe$UTM_N, TCCZe$TCCZ_top, xo=seq(ea.min, ea.max, length = 30), yo=seq(no.min, no.max, length = 50), linear = TRUE, extrap=FALSE, duplicate = "error")
wlavg.interp<-interp(wlavg$EASTING, wlavg$NORTHING, wlavg$mean, xo=seq(ea.min, ea.max, length = 30), yo=seq(no.min, no.max, length = 50), linear = TRUE, extrap=FALSE, duplicate = "mean")

#create aquifer thickness

#image and contour plot for the TCCZ
image.plot(TCCZ.interp)
contour(TCCZ.interp$x,TCCZ.interp$y,TCCZ.interp$z)
points(f3basin27$UTM_E,f3basin27$UTM_N,type="l")
points(TCCZe$UTM_E, TCCZe$UTM_N, pch=19)
text(TCCZe$UTM_E, TCCZe$UTM_N, labels=TCCZe$UWI)

#image and contour plot for the water level
image.plot(wlavg.interp)
contour(wlavg.interp$x,wlavg.interp$y,wlavg.interp$z)

#
#image and contour plot for the water level
#image.plot(wlavg.interp)
contour(wlavg.interp$x,wlavg.interp$y,wlavg.interp$z-TCCZ.interp$z,  asp = 1, bty ="n")
#xlim=c(ea.min,ea.max), ylim=c(no.min,no.max),
#points(f3basin$UTM_E,f3basin$UTM_N,type="l")
points(f3basin27$UTM_E,f3basin27$UTM_N,type="l")
points(wlavg$EASTING, wlavg$NORTHING, pch=19)
text(wlavg$EASTING, wlavg$NORTHING, labels=wlavg$STATION_ID)

#Define n zones

#Compute trend planes in these zones for waterlevels and TCCZ

#Do the difference to get the thickness

