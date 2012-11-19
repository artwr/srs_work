#Simple Computation
require(geoR)
require(geoRglm)
require(fields)
require(akima)
require(splancs)
require(plyr)
#require(foreach)

#
rm(list=ls())
options(na.action="na.exclude")

#load data
tritium<-readRDS("../SRS_data/tritium.rdata")
wl<-readRDS("../SRS_data/wl.rdata")
TCCZe_all<-readRDS("../TCCZ_krig/TCCZ/TCCZ_o.rdata")
TCCZe<-TCCZe_all[!is.na(TCCZe_all$TCCZ_top),]
wlavg<-readRDS("../SRS_data/wlavg.rdata")

#basin coords
f3basin<-readRDS("../basin_coords/f3basin.rdata")
f3basin27<-readRDS("../basin_coords/f3basin27.rdata")

#
wll<-split(wl,wl$MYEAR)
tritiuml<-split(tritium,tritium$MYEAR)
#
#selectyear<-function (x,y) {subset(x, x$MYEAR == y)}
#tritium.1984<-selectyear(tritium, 1984)
#summary(tritium[tritium$STATION_ID=='FSB115D',])
#TCCZ.lm<-lm(TCCZ_top~UTM_E+UTM_N,data=TCCZe,na.action=na.omit)

#Local polynomial fit (1st order)
TCCZ.loess1 <- loess(TCCZ_top~EASTING+NORTHING, data = TCCZe, degree = 1, span = 0.25)
#second order polynomial
#TCCZ.loess2 = loess(TCCZ_top~UTM_E+UTM_N, data = TCCZe, degree = 2, span = 0.25)
#predict(TCCZ.loess1,newdata = wlavg[,c("EASTING","NORTHING")], na.action = na.omit)

#Create the aquifer thickness data frame
thavg<-wlavg
#Add the TCCZ values predicted by the linear model
#thavg$TCCZ<-predict(TCCZ.loess1,newdata = wlavg[,c("EASTING","NORTHING")], na.action = na.exclude)
#Same with standard error estimates
pre1<-predict(TCCZ.loess1,newdata = wlavg[,c("EASTING","NORTHING")],se = TRUE ,na.action = na.exclude)
thavg$TCCZ.fit<-pre1$fit
thavg$TCCZ.se.fit<-pre1$se.fit

#Compute the thickness at the relevant points
thavg$thick<-thavg$mean-thavg$TCCZ.fit

summary(thavg$thick)




#2D interpolation within the bounds of interest.
no.min<-3680930
no.max<-3682110
ea.min<-436175
ea.max<-437155

#number of breaks
ea.b<-1+(ea.max-ea.min)/20
no.b<-1+(no.max-no.min)/20

d.ea<-c(ea.min,ea.min,ea.max,ea.max,ea.min)
d.no<-c(no.min,no.max,no.max,no.min,no.min)
pp<-cbind(d.ea,d.no)
#plot(pp, type="b")
area.dom<-areapl(pp)

interp.peryear<- function(x) interp(x$EASTING, x$NORTHING, x$mean, xo=seq(ea.min, ea.max, length = ea.b), yo=seq(no.min, no.max, length = no.b), linear = TRUE, extrap=FALSE, duplicate = "mean");

interpext.peryear<- function(x) interp(x$EASTING, x$NORTHING, x$mean, xo=seq(ea.min, ea.max, length = ea.b), yo=seq(no.min, no.max, length = no.b), linear = TRUE, extrap=TRUE, duplicate = "mean");

#

TCCZ.interp<-interp(TCCZe$EASTING, TCCZe$NORTHING, TCCZe$TCCZ_top, xo=seq(ea.min, ea.max, length = ea.b), yo=seq(no.min, no.max, length = no.b), linear = TRUE, extrap=FALSE, duplicate = "error")

wlavg.interp<-interp(wlavg$EASTING, wlavg$NORTHING, wlavg$mean, xo=seq(ea.min, ea.max, length = ea.b), yo=seq(no.min, no.max, length = no.b), linear = TRUE, extrap=FALSE, duplicate = "mean")

tritium.interp<-interp(tritium$EASTING, tritium$NORTHING, tritium$mean, xo=seq(ea.min, ea.max, length = ea.b), yo=seq(no.min, no.max, length = no.b), linear = TRUE, extrap=FALSE, duplicate = "mean")

wll.interp<-llply(wll, interp.peryear)

tritiuml.interp<-llply(tritiuml, interp.peryear)

wll.interpext<-llply(wll, interpext.peryear, .progress = "text", .inform = FALSE)

tritiuml.interpext<-llply(tritiuml, interpext.peryear, .progress = "text", .inform = FALSE)


#create aquifer thickness
thickness<-llply(wll.interp,function(ll){list(x=ll$x, y=ll$y, z=as.matrix(0.3048*(ll$z-TCCZ.interp$z)))}, .progress = "text", .inform = FALSE, .parallel = FALSE )

#The result is a nested list argh!
#is.list(testthickness['1984'][[1]])
#To access 
#testthickness['1984'][[1]][[3]]
#contour(testthickness['1988'][[1]][[1]],testthickness['1988'][[1]][[2]],testthickness['1988'][[1]][[3]])
#image.plot(testthickness['1985'][[1]])

#A quick look at the tritium data per year
image.plot(tritiuml.interp['1990'][[1]])
points(tritium$EASTING, tritium$NORTHING, pch=19)


replaceNAwithzero<-function(m){m[is.na(m)]<-0};



# #image and contour plot for the TCCZ
# image.plot(TCCZ.interp)
# contour(TCCZ.interp$x,TCCZ.interp$y,TCCZ.interp$z)
# points(f3basin27$UTM_E,f3basin27$UTM_N,type="l")
# points(TCCZe$UTM_E, TCCZe$UTM_N, pch=19)
# text(TCCZe$UTM_E, TCCZe$UTM_N, labels=TCCZe$UWI)
# 
# #image and contour plot for the water level
# image.plot(wlavg.interp)
# contour(wlavg.interp$x,wlavg.interp$y,wlavg.interp$z)
# 
# #
# #image and contour plot for the water level
# #image.plot(wlavg.interp)
# contour(wlavg.interp$x,wlavg.interp$y,wlavg.interp$z-TCCZ.interp$z,  asp = 1, bty ="n")
# #xlim=c(ea.min,ea.max), ylim=c(no.min,no.max),
# #points(f3basin$UTM_E,f3basin$UTM_N,type="l")
# points(f3basin27$UTM_E,f3basin27$UTM_N,type="l")
# points(wlavg$EASTING, wlavg$NORTHING, pch=19)
# text(wlavg$EASTING, wlavg$NORTHING, labels=wlavg$STATION_ID)

#Define n zones

#Compute trend planes in these zones for waterlevels and TCCZ

#Do the difference to get the thickness

