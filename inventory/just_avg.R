#Simple Computation
require(geoR)
require(geoRglm)
require(fields)
require(akima)
require(splancs)
require(plyr)
require(ggplot2)
require(scales)

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
rm(TCCZe_all)
wlavg<-readRDS("../SRS_data/wlavg.rdata")


#basin coords for plotting if needed
#f3basin<-readRDS("../basin_coords/f3basin.rdata")
#f3basin27<-readRDS("../basin_coords/f3basin27.rdata")

#
#Split per measurement year
wll<-split(wl,wl$MYEAR)
tritiuml<-split(tritium,tritium$MYEAR)


#########################################################
#2.
#Define interpolation domain and compute area, define other parameters
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

#Create the expandgrid df for predictions
testgrid1<-expand.grid(EASTING=ea.v, NORTHING=no.v)

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

#Local polynomial fit (1st order) and linear model
TCCZ.loess1 <- loess(TCCZ_top~EASTING+NORTHING, data = TCCZe, degree = 1, span = 0.25)
TCCZ.loess1b <- loess(TCCZ_top~EASTING+NORTHING, data = TCCZe, degree = 1, span = 0.4)
TCCZ.lm<-lm(TCCZ_top~EASTING+NORTHING, data = TCCZe)
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
pre1b<-predict(TCCZ.loess1b,newdata = wlavg[,c("EASTING","NORTHING")],se = TRUE ,na.action = na.omit)
pre2b<-predict(TCCZ.loess1b,newdata = wl[,c("EASTING","NORTHING")],se = TRUE ,na.action = na.omit)
pre1lm<-predict(TCCZ.lm,newdata = wlavg[,c("EASTING","NORTHING")],se = TRUE ,na.action = na.omit)
pre2lm<-predict(TCCZ.lm,newdata = wl[,c("EASTING","NORTHING")],se = TRUE ,na.action = na.omit)
#summary(pre1)
thavg$TCCZ.fit<-pre1$fit
thavg$TCCZ.se.fit<-pre1$se.fit
thavg$TCCZ.fitb<-pre1b$fit
thavg$TCCZ.se.fitb<-pre1b$se.fit
thavg$TCCZ.fitlm<-pre1lm$fit
thavg$TCCZ.se.fitlm<-pre1lm$se.fit

thperyear$TCCZ.fit<-pre2$fit
thperyear$TCCZ.se.fit<-pre2$se.fit
thperyear$TCCZ.fitb<-pre2b$fit
thperyear$TCCZ.se.fitb<-pre2b$se.fit
thperyear$TCCZ.fitlm<-pre2lm$fit
thperyear$TCCZ.se.fitlm<-pre2lm$se.fit

#Compute the thickness in feet
thavg$h<-thavg$mean-thavg$TCCZ.fit
thperyear$h<-thperyear$mean-thperyear$TCCZ.fit
thperyear$hb<-thperyear$mean-thperyear$TCCZ.fitb
thperyear$hlm<-thperyear$mean-thperyear$TCCZ.fitlm

# Replace negative values with NA
thavg$h[thavg$h<=0]<-NA
thperyear$h[thperyear$h<=0]<-NA
thperyear$hb[thperyear$hb<=0]<-NA
thperyear$hlm[thperyear$hlm<=0]<-NA
#Remove NAs
thavg.clean<-thavg[!is.na(thavg$h),]
thperyear.cleanh<-thperyear[!is.na(thperyear$h),]
thperyear.cleanhb<-thperyear[!is.na(thperyear$hb),]
thperyear.cleanhlm<-thperyear[!is.na(thperyear$hlm),]

#Compute the avg per year
th.avg.peryearh<-ddply(thperyear.cleanh, c('MYEAR'), function(x) c(counth=nrow(x),h.mean=mean(x$h),h.median=median(x$h),h.sd=sd(x$h),h.mad=mad(x$h),h.min=min(x$h),h.max=max(x$h)))
th.avg.peryearhb<-ddply(thperyear.cleanhb, c('MYEAR'), function(x) c(counthb=nrow(x),hb.mean=mean(x$hb),hb.median=median(x$hb),hb.sd=sd(x$hb),hb.mad=mad(x$hb),h.min=min(x$hb),h.max=max(x$hb)))
th.avg.peryearhlm<-ddply(thperyear.cleanhlm, c('MYEAR'), function(x) c(counthlm=nrow(x),hlm.mean=mean(x$hlm),hlm.median=median(x$hlm),hlm.sd=sd(x$hlm),hlm.mad=mad(x$h),hlm.min=min(x$h),hlm.max=max(x$hlm)))
#Create inventory df
inventoryjah<-merge(tritiumavg[tritiumavg$MYEAR>=1984,],th.avg.peryearh,by="MYEAR")
inventoryjahb<-merge(tritiumavg[tritiumavg$MYEAR>=1984,],th.avg.peryearhb,by="MYEAR")
inventoryjahlm<-merge(tritiumavg[tritiumavg$MYEAR>=1984,],th.avg.peryearhlm,by="MYEAR")
inventoryja<-merge(inventoryjah,th.avg.peryearhb,by="MYEAR")
inventoryja<-merge(inventoryja,th.avg.peryearhlm,by="MYEAR")
#inventoryja[,c("count","h.mean","h.median","h.sd","h.mad","h.min","h.max")]<-th.avg.peryear[, c("count","h.mean","h.median","h.sd","h.mad","h.min","h.max")]
inventoryjah$inventory<-area.dom*porosity.mean*inventoryjah$h.mean*inventoryjah$mean*.3048*1e-9
inventoryjahb$inventory<-area.dom*porosity.mean*inventoryjahb$hb.mean*inventoryjahb$mean*.3048*1e-9
inventoryjahlm$inventory<-area.dom*porosity.mean*inventoryjahlm$hlm.mean*inventoryjahlm$mean*.3048*1e-9
inventoryja$inventory1<-area.dom*porosity.mean*inventoryja$h.mean*inventoryja$mean*.3048*1e-9
inventoryja$inventory1b<-area.dom*porosity.mean*inventoryja$hb.mean*inventoryja$mean*.3048*1e-9
inventoryja$inventory1lm<-area.dom*porosity.mean*inventoryja$hlm.mean*inventoryja$mean*.3048*1e-9

rm(inventoryjah)
rm(inventoryjahb)
rm(inventoryjahlm)
rm(thperyear.cleanh)
rm(thperyear.cleanhb)
rm(thperyear.cleanhlm)
rm(th.avg.peryearh)
rm(th.avg.peryearhb)
rm(th.avg.peryearhlm)

#Draft ggplot for the inventory
#qplot(MYEAR, inventory, data=inventoryja)
ja.plot<-ggplot(data=inventoryja, aes(x=MYEAR))
ja.plot<- ja.plot +geom_line(aes(y=inventory1), colour='blue')
ja.plot<- ja.plot +geom_line(aes(y=inventory1b), colour='red')
ja.plot<- ja.plot +geom_line(aes(y=inventory1lm), colour='green')
ja.plot<-ja.plot+labs(title="Tritium Inventory")+xlab("Year")+ylab("Tritium (Ci)")
ja.plot


# log2 scaling of the y axis (with visually-equal spacing)
ja.plotlog1<- ja.plot + scale_y_continuous(trans=log10_trans())
ja.plotlog1
# log2 coordinate transformation (with visually-diminishing spacing)
ja.plotlog2<- ja.plot + coord_trans(y="log2")
ja.plotlog2

saveRDS(inventoryja, file = "inventoryja.rdata")

inventoryja.csv<-inventoryja[,c("MYEAR","inventory1","inventory1b","inventory1lm")]
write.csv(inventoryja.csv, file="inventoryja.csv",eol="\r\n")

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
#wlavg.lm<-lm(mean~EASTING+NORTHING, data =wlavg)
#prewllm<-predict(wlavg.lm,newdata = testgrid1,se = TRUE ,na.action = na.omit)

#Create the loess models for wl and tritium
#loess<- function(zzl) loess()


# Use of the loess model and linear models for TCCZ prediction
pre3<-predict(TCCZ.loess1,newdata = testgrid1,se = TRUE ,na.action = na.omit)
pre3b<-predict(TCCZ.loess1b,newdata = testgrid1,se = TRUE ,na.action = na.omit)
pre3lm<-predict(TCCZ.lm,newdata = testgrid1,se = TRUE ,na.action = na.omit)
#TCCZ.loess1.interp<-list(x=matrix(testgrid1$EASTING,nrow=60, ncol=50, byrow=TRUE),y=matrix(testgrid1$NORTHING,nrow=60, ncol=50, byrow=TRUE),z=matrix(pre3$fit,nrow=60, ncol=50, byrow=TRUE))
#TCCZ.loess1b.interp<-list(x=matrix(testgrid1$EASTING,nrow=60, ncol=50, byrow=TRUE),y=matrix(testgrid1$NORTHING,nrow=60, ncol=50, byrow=TRUE),z=matrix(pre3$fit,nrow=60, ncol=50, byrow=TRUE))
# Plot
# 




#Selected matrix functions from Henrik Bengtsson
# https://stat.ethz.ch/pipermail/r-help/2003-October/040484.html
# http://www1.maths.lth.se/help/R/image/image.R
# Mirror matrix (left-right)
mirror.matrix <- function(x) {
  xx <- as.data.frame(x);
  xx <- rev(xx);
  xx <- as.matrix(xx);
  xx;
}

# Rotate matrix 90 clockworks
rotate90.matrix <- function(x) {
  t(mirror.matrix(x))
}

# Rotate matrix 180 clockworks
rotate180.matrix <- function(x) { 
  xx <- rev(x);
  dim(xx) <- dim(x);
  xx;
}

# Rotate matrix 270 clockworks
rotate270.matrix <- function(x) {
  mirror.matrix(t(x))
}


# image.plot(x=matrix(testgrid1$EASTING,nrow=60, ncol=50, byrow=TRUE),y=matrix(testgrid1$NORTHING,nrow=60, ncol=50, byrow=TRUE),z=matrix(pre3$fit,nrow=60, ncol=50, byrow=TRUE))
# image.plot(x=matrix(testgrid1$EASTING,nrow=60, ncol=50, byrow=TRUE),y=matrix(testgrid1$NORTHING,nrow=60, ncol=50, byrow=TRUE),z=matrix(pre3b$fit,nrow=60, ncol=50, byrow=TRUE))
# image.plot(x=matrix(testgrid1$EASTING,nrow=60, ncol=50, byrow=TRUE),y=matrix(testgrid1$NORTHING,nrow=60, ncol=50, byrow=TRUE),z=matrix(pre3lm$fit,nrow=60, ncol=50, byrow=TRUE))
# 
# image.plot(x=matrix(testgrid1$EASTING,nrow=60, ncol=50, byrow=TRUE),y=matrix(testgrid1$NORTHING,nrow=60, ncol=50, byrow=TRUE),z=rotate270.matrix(wlavg.interp$z)-matrix(pre3$fit,nrow=60, ncol=50, byrow=TRUE))
# image.plot(x=matrix(testgrid1$EASTING,nrow=60, ncol=50, byrow=TRUE),y=matrix(testgrid1$NORTHING,nrow=60, ncol=50, byrow=TRUE),z=rotate90.matrix(wlavg.interp$z)-matrix(pre3b$fit,nrow=60, ncol=50, byrow=TRUE))
# image.plot(x=matrix(testgrid1$EASTING,nrow=60, ncol=50, byrow=TRUE),y=matrix(testgrid1$NORTHING,nrow=60, ncol=50, byrow=TRUE),z=rotate90.matrix(wlavg.interp$z)-matrix(pre3lm$fit,nrow=60, ncol=50, byrow=TRUE))
# 
# image.plot(x=matrix(testgrid1$EASTING,nrow=60, ncol=50, byrow=TRUE),y=matrix(testgrid1$NORTHING,nrow=60, ncol=50, byrow=TRUE),z=wlavg.interp$z-matrix(pre3$fit,nrow=60, ncol=50, byrow=TRUE))
# image.plot(x=matrix(testgrid1$EASTING,nrow=60, ncol=50, byrow=TRUE),y=matrix(testgrid1$NORTHING,nrow=60, ncol=50, byrow=TRUE),z=wlavg.interp$z-matrix(pre3b$fit,nrow=60, ncol=50, byrow=TRUE))
# image.plot(x=matrix(testgrid1$EASTING,nrow=60, ncol=50, byrow=TRUE),y=matrix(testgrid1$NORTHING,nrow=60, ncol=50, byrow=TRUE),z=wlavg.interp$z-matrix(pre3lm$fit,nrow=60, ncol=50, byrow=TRUE))


# image.plot(wlavg.interp)
# dim(wlavg.interp$z)
# image.plot(TCCZ.loess1.interp)
# points(TCCZe$EASTING, TCCZe$NORTHING, pch=19)
# text(TCCZe$EASTING, TCCZe$NORTHING, labels=TCCZe$STATION_ID)

wll.interp<-llply(wll, interp.peryear)
tritiuml.interp<-llply(tritiuml, interp.peryear)

wll.interpext<-llply(wll, interpext.peryear, .progress = "text", .inform = FALSE)
tritiuml.interpext<-llply(tritiuml, interpext.peryear, .progress = "text", .inform = FALSE)


#create aquifer thickness
# thickness<-llply(wll.interp,function(ll){list(x=ll$x, y=ll$y, z=as.matrix(0.3048*(ll$z-TCCZ.interp$z)))}, .progress = "text")

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

# #Test interpolation debugging with smaller matrix
# #number of breaks 
# ea.b2<-15
# no.b2<-20
# 
# #ea
# ea.v2<-seq(ea.min, ea.max, length = ea.b2)
# no.v2<-seq(no.min, no.max, length = no.b2)
# 
# wlavg.interp2<-interp(wlavg$EASTING, wlavg$NORTHING, wlavg$mean, xo=ea.v2, yo=no.v2, linear = TRUE, extrap=FALSE, duplicate = "mean")
# # Flip matrix (upside-down)
# flip.matrix <- function(x) {
#   mirror.matrix(rotate180.matrix(x))
# }
# # Rotate matrix 180 clockworks
# rotate180.matrix <- function(x) { 
#   xx <- rev(x);
#   dim(xx) <- dim(x);
#   xx;
# }
# 
# # Rotate matrix 270 clockworks
# rotate270.matrix <- function(x) {
#   mirror.matrix(t(x))
# }
# # Debug Statements for interp output
# tx<-matrix(testgrid1$EASTING,nrow=60, ncol=50, byrow=TRUE)
# ty<-matrix(testgrid1$NORTHING,nrow=60, ncol=50, byrow=TRUE)
# tz<-wlavg.interp2$z
# tzprime<-t(wlavg.interp2$z)
# tz90<-rotate90.matrix(tz)







