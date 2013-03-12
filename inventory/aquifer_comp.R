#Computation of the phreatic aquifer thickness
#Source the file at the beginning of each analysis for a contaminant.

#Load packages
require(fields)
require(splancs)
require(plyr)
require(ggplot2)
require(scales)

#Clean if necessary
# rm(list=ls())

#Import the water level data
wl<-readRDS("../SRS_data/wl.rdata")
wlavg<-readRDS("../SRS_data/wlavg.rdata")
#Import picks for the TCCZ
TCCZe_all<-readRDS("../TCCZ_krig/TCCZ/TCCZ_o.rdata")
TCCZe<-TCCZe_all[!is.na(TCCZe_all$TCCZ_top),]
rm(TCCZe_all)

#basin coords for plotting if needed
#f3basin<-readRDS("../basin_coords/f3basin.rdata")
#f3basin27<-readRDS("../basin_coords/f3basin27.rdata")

#Split per measurement year
wll<-split(wl,wl$MYEAR)
#Select 1988 and after
wll2<-wll[5:length(wll)]

#########################################################
#2.
#Define interpolation domain and compute area, define other parameters

#Boundaries
no.min<-3680930
no.max<-3682110
ea.min<-436175
ea.max<-437155
#number of breaks ~ 20 m apart
ea.b<-1+(ea.max-ea.min)/20
no.b<-1+(no.max-no.min)/20
#Create the vectors
ea.v<-seq(ea.min, ea.max, length = ea.b)
no.v<-seq(no.min, no.max, length = no.b)
#Create the expandgrid df for predictions
testgrid1<-expand.grid(EASTING=ea.v, NORTHING=no.v)

# Create polygon to compute area. 
# Needs splancs lib for more complex polygons
d.ea<-c(ea.min,ea.min,ea.max,ea.max,ea.min)
d.no<-c(no.min,no.max,no.max,no.min,no.min)
pp<-cbind(d.ea,d.no)
#plot(pp, type="b")
area.dom<-areapl(pp)

#Define porosity value
porosity.mean<-.3
#porosity.sd<-.03

# LAZ Aquifer C assumed thickness in m 
# (corresponds to the screen interval for the C wells)
Cth<-10*.3048

#Local polynomial fit (1st order) and linear model
TCCZ.loess1<-loess(TCCZ_top~EASTING+NORTHING,data=TCCZe,degree= 1, span= 0.25)
TCCZ.loess1b<-loess(TCCZ_top~EASTING+NORTHING,data=TCCZe,degree= 1,span= 0.4)
TCCZ.lm<-lm(TCCZ_top~EASTING+NORTHING,data=TCCZe)

#Create the aquifer thickness data frame
thperyear<-wl
#Add the TCCZ values predicted by the linear models at the wells coordinates
# with standard error estimates
pre2<-predict(TCCZ.loess1,newdata = wl[,c("EASTING","NORTHING")],se = TRUE)
pre2b<-predict(TCCZ.loess1b,newdata = wl[,c("EASTING","NORTHING")],se = TRUE)
pre2lm<-predict(TCCZ.lm,newdata = wl[,c("EASTING","NORTHING")],se = TRUE, interval = "prediction",level = 0.95)
# summary(pre2)
#
thperyear$TCCZ.fit<-pre2$fit
thperyear$TCCZ.se.fit<-pre2$se.fit
thperyear$TCCZ.fitb<-pre2b$fit
thperyear$TCCZ.se.fitb<-pre2b$se.fit
thperyear$TCCZ.fitlm<-pre2lm$fit[,1]
thperyear$TCCZ.se.fitlm<-pre2lm$se.fit
#Upper
thperyear$TCCZ.fitlmupr<-pre2lm$fit[,2]
#Lower Bound
thperyear$TCCZ.fitlmlwr<-pre2lm$fit[,3]

#Compute the thickness in feet
thperyear$h<-thperyear$mean-thperyear$TCCZ.fit
thperyear$hb<-thperyear$mean-thperyear$TCCZ.fitb
thperyear$hlm<-thperyear$mean-thperyear$TCCZ.fitlm

# #Diagnostics on the thickness
# summary(thperyear$hlm)
# #ggplot
# ph <- ggplot(thperyear, aes(x=EASTING,y=NORTHING)) + geom_point(aes(colour=hlm), size = 10) + scale_colour_gradient2(low="red", high="blue") 
# print(ph)
# #Close up in the area + 100m margin to the interpolation domain.
# ph2 <- ph + xlim(ea.min-100,ea.max+100) + ylim(no.min-100,no.max+100)
# print(ph2)
#In this case, I am removing points that are attributed a negative aq thickness. They will not be taken into account for the inventory
# Replace negative values with NA
thperyear$h[thperyear$h<=0]<-NA
thperyear$hb[thperyear$hb<=0]<-NA
thperyear$hlm[thperyear$hlm<=0]<-NA
#Remove NAs
thperyear.cleanh<-thperyear[!is.na(thperyear$h),]
thperyear.cleanhb<-thperyear[!is.na(thperyear$hb),]
thperyear.cleanhlm<-thperyear[!is.na(thperyear$hlm),]

#Compute the avg per year
th.avg.peryearh<-ddply(thperyear.cleanh, c('MYEAR'), function(x) c(counth=nrow(x),h.mean=mean(x$h),h.median=median(x$h),h.sd=sd(x$h),h.mad=mad(x$h),h.min=min(x$h),h.max=max(x$h)))
th.avg.peryearhb<-ddply(thperyear.cleanhb, c('MYEAR'), function(x) c(counthb=nrow(x),hb.mean=mean(x$hb),hb.median=median(x$hb),hb.sd=sd(x$hb),hb.mad=mad(x$hb),hb.min=min(x$hb),hb.max=max(x$hb)))
th.avg.peryearhlm<-ddply(thperyear.cleanhlm, c('MYEAR'), function(x) c(counthlm=nrow(x),hlm.mean=mean(x$hlm),hlm.median=median(x$hlm),hlm.sd=sd(x$hlm),hlm.mad=mad(x$hlm),hlm.min=min(x$hlm),hlm.max=max(x$hlm)))


