#Simple Computation
require(geoR)
require(geoRglm)
require(fields)
require(akima)
require(splancs)
require(plyr)
require(ggplot2)
#require(reshape2)

#Clear all, set na.exclude
rm(list=ls())
#options(na.action="na.omit")
#options("na.action")

#1.
#load data
tritium<-readRDS("../SRS_data/tritium.rdata")
tritiumavg<-readRDS("../SRS_data/tritiumavg.rdata")
wl<-readRDS("../SRS_data/wl.rdata")
TCCZe_all<-readRDS("../TCCZ_krig/TCCZ/TCCZ_o.rdata")
TCCZe<-TCCZe_all[!is.na(TCCZe_all$TCCZ_top),]
wlavg<-readRDS("../SRS_data/wlavg.rdata")


#basin coords for plotting if needed
#f3basin<-readRDS("../basin_coords/f3basin.rdata")
#f3basin27<-readRDS("../basin_coords/f3basin27.rdata")

#
#Split per measurement year
wll<-split(wl,wl$MYEAR)
tritiuml<-split(tritium,tritium$MYEAR)

#2.
#Define interpolation domain and compute area
#
no.min<-3680930
no.max<-3682110
ea.min<-436175
ea.max<-437155

#number of breaks ~ 20 m apart
ea.b<-1+(ea.max-ea.min)/20
no.b<-1+(no.max-no.min)/20

#ea
ea.v<-seq(ea.min, ea.max, length = ea.b)
no.v<-seq(no.min, no.max, length = no.b)

#Create polygon to compute area.
d.ea<-c(ea.min,ea.min,ea.max,ea.max,ea.min)
d.no<-c(no.min,no.max,no.max,no.min,no.min)
pp<-cbind(d.ea,d.no)
#plot(pp, type="b")
area.dom<-areapl(pp)

#Define porosity value
porosity.mean<-.3
porosity.sd<-.03

#########################################################
#3. Do simple average calculations on the well positions only
# \int_D C h p dx dy = \int_D dx dy * \hat{C} * \hat{h} * porosity p
#Thickness is computed using a linear prediction

#Local polynomial fit (1st order)
TCCZ.loess1 <- loess(TCCZ_top~EASTING+NORTHING, data = TCCZe, degree = 1, span = 0.25)
#second order polynomial
#TCCZ.loess2 = loess(TCCZ_top~EASTING+NORTHING, data = TCCZe, degree = 2, span = 0.25)
#predict(TCCZ.loess1,newdata = wlavg[,c("EASTING","NORTHING")], na.action = na.omit)

#Create the aquifer thickness data frame
thavg<-wlavg
thperyear<-wl
#Add the TCCZ values predicted by the linear model
#thavg$TCCZ<-predict(TCCZ.loess1,newdata = wlavg[,c("EASTING","NORTHING")], na.action = na.exclude)
#Same with standard error estimates
pre1<-predict(TCCZ.loess1,newdata = wlavg[,c("EASTING","NORTHING")],se = TRUE ,na.action = na.omit)
pre2<-predict(TCCZ.loess1,newdata = wl[,c("EASTING","NORTHING")],se = TRUE ,na.action = na.omit)
#summary(pre1)
thavg$TCCZ.fit<-pre1$fit
thavg$TCCZ.se.fit<-pre1$se.fit

thperyear$TCCZ.fit<-pre2$fit
thperyear$TCCZ.se.fit<-pre2$se.fit

#Compute the thickness in feet
thavg$h<-thavg$mean-thavg$TCCZ.fit
thperyear$h<-thperyear$mean-thperyear$TCCZ.fit

# Replace negative values with NA
thavg$h[thavg$h<=0]<-NA
thperyear$h[thperyear$h<=0]<-NA
#Remove NAs
thavg.clean<-thavg[!is.na(thavg$h),]
thperyear.clean<-thperyear[!is.na(thperyear$h),]

#Compute the avg per year
th.avg.peryear<-ddply(thperyear.clean, c('MYEAR'), function(x) c(count=nrow(x),h.mean=mean(x$h),h.median=median(x$h),h.sd=sd(x$h),h.mad=mad(x$h),h.min=min(x$h),h.max=max(x$h)))

#Create inventory df
inventoryja<-merge(tritiumavg[tritiumavg$MYEAR>=1984,],th.avg.peryear,by="MYEAR")
#inventoryja[,c("count","h.mean","h.median","h.sd","h.mad","h.min","h.max")]<-th.avg.peryear[, c("count","h.mean","h.median","h.sd","h.mad","h.min","h.max")]
inventoryja$inventory<-area.dom*porosity.mean*inventoryja$h.mean*inventoryja$mean*.3048*1e-9*porosity.mean
#qplot(MYEAR, inventory, data=inventoryja)

########################################
#4. Do the calculation again, this time with interpolation on a regular grid.
# We should compute both 
# \int_D C * h * p dx dy = \int_D C dx dy/area.dom * \int_D h dx dy/area.dom * p
# = (\Sigma C)/N points on the regular grid * (\Sigma h)/N * p
# and
# \int_D C h dx dy = \int_D C*h dx dy * p
#The later will allow for the matching of concentration and aquifer thickness.
###########################################

#Define short hand for AKIMA interpolation functions without and with extrapolation
interp.peryear<- function(x) interp(x$EASTING, x$NORTHING, x$mean, xo=ea.v, yo=no.v, linear = TRUE, extrap=FALSE, duplicate = "mean");
interpext.peryear<- function(x) interp(x$EASTING, x$NORTHING, x$mean, xo=ea.v, yo=no.v, linear = FALSE, extrap=TRUE, duplicate = "mean");

#
TCCZ.interp<-interp(TCCZe$EASTING, TCCZe$NORTHING, TCCZe$TCCZ_top, xo=ea.v, yo=no.v, linear = TRUE, extrap=FALSE, duplicate = "error")
wlavg.interp<-interp(wlavg$EASTING, wlavg$NORTHING, wlavg$mean, xo=ea.v, yo=no.v, linear = TRUE, extrap=FALSE, duplicate = "mean")

# Use of the loess model for TCCZ prediction
testgrid1<-expand.grid(EASTING=ea.v, NORTHING=no.v)
pre3<-predict(TCCZ.loess1,newdata = testgrid1,se = TRUE ,na.action = na.omit)
TCCZ.loess.interp<-list(x=)
# Plot
# image.plot(x=matrix(testgrid1$EASTING,nrow=60, ncol=50, byrow=TRUE),y=matrix(testgrid1$NORTHING,nrow=60, ncol=50, byrow=TRUE),z=matrix(pre3$fit,nrow=60, ncol=50, byrow=TRUE))
# points(TCCZe$EASTING, TCCZe$NORTHING, pch=19)
# text(TCCZe$EASTING, TCCZe$NORTHING, labels=TCCZe$STATION_ID)

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
#image.plot(tritiuml.interp['1990'][[1]])
#points(tritium$EASTING, tritium$NORTHING, pch=19)


#replaceNAwithzero<-function(m){m[is.na(m)]<-0};



# #image and contour plot for the TCCZ
# image.plot(TCCZ.interp)
# contour(TCCZ.interp$x,TCCZ.interp$y,TCCZ.interp$z)
# points(f3basin27$UTM_E,f3basin27$UTM_N,type="l")
# points(TCCZe$EASTING, TCCZe$NORTHING, pch=19)
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





#Leftovers
# 
#

#
#selectyear<-function (x,y) {subset(x, x$MYEAR == y)}
#tritium.1984<-selectyear(tritium, 1984)
#TCCZ.lm<-lm(TCCZ_top~UTM_E+UTM_N,data=TCCZe,na.action=na.omit)
#tritium.interp<-interp(tritium$EASTING, tritium$NORTHING, tritium$mean, xo=seq(ea.min, ea.max, length = ea.b), yo=seq(no.min, no.max, length = no.b), linear = TRUE, extrap=FALSE, duplicate = "mean")