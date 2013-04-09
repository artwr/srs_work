#Computation of the phreatic aquifer thickness
#Source the file at the beginning of each analysis for a contaminant.

#Load packages
# require(fields)
require(splancs)
require(plyr)

#Clean if necessary
# rm(list=ls())

#################
# 0. Import the need data TCCZ and Water levels
#
#################


#Import picks for the TCCZ
TCCZe_all<-readRDS("../TCCZ_krig/TCCZ/TCCZ_o.rdata")
# Remove the points with no TCCZ pick. 
TCCZe<-TCCZe_all[!is.na(TCCZe_all$TCCZ_top),]
rm(TCCZe_all)

#Import the water level data
wl<-readRDS("../SRS_data/wl.rdata")
wlavg<-readRDS("../SRS_data/wlavg.rdata")

#Split per measurement year
wll<-split(wl,wl$MYEAR)
#Select 1988 and after
wll2<-wll[5:length(wll)]

#basin coords for plotting if needed
#f3basin<-readRDS("../basin_coords/f3basin.rdata")
#f3basin27<-readRDS("../basin_coords/f3basin27.rdata")



#########################################################
#1.
#Define interpolation domain and compute area, define other parameters

# source('interpolation_domain.R')

#Define porosity value
porosity.mean<-.3
#porosity.sd<-.03

# LAZ Aquifer C assumed thickness in m 
# (corresponds to the screen interval for the C wells)
Cth<-10*.3048

#Alpha loess
alphaloess1<-0.25
alphaloess2<-0.4
alphaloesswl<-0.25

#Local polynomial fit (1st order) and linear model
TCCZ.loess1<-loess(TCCZ_top~EASTING+NORTHING,data=TCCZe,degree= 1, span= alphaloess1)
TCCZ.loess1b<-loess(TCCZ_top~EASTING+NORTHING,data=TCCZe,degree= 1,span= alphaloess2)
TCCZ.lm<-lm(TCCZ_top~EASTING+NORTHING,data=TCCZe)

####################
# 2. Computation of thickness on the water level points for aggregate/average computation
#
#
####################


#Create the aquifer thickness data frame
thperyear<-wl
#Add the TCCZ values predicted by the linear models 
# at the wells coordinates
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

#Compute the standard error of that estimate
thperyear$hse<-sqrt(max(1,thperyear$sd, na.rm =TRUE)^2+thperyear$TCCZ.se.fit^2)
thperyear$hbse<-sqrt(max(1,thperyear$sd, na.rm =TRUE)^2+thperyear$TCCZ.se.fitb^2)
thperyear$hlmse<-sqrt(max(1,thperyear$sd, na.rm =TRUE)^2+thperyear$TCCZ.se.fitlm^2)

# #Diagnostics on the thickness
# summary(thperyear$hlm)
# #ggplot
# ph <- ggplot(thperyear, aes(x=EASTING,y=NORTHING)) + geom_point(aes(colour=hlm), size = 10) + scale_colour_gradient2(low="red", high="blue") 
# print(ph)
# #Close up in the area + 100m margin to the interpolation domain.
# ph2 <- ph + xlim(ea.min-100,ea.max+100) + ylim(no.min-100,no.max+100)
# print(ph2)

# In this case, I am removing points that are attributed a negative aq thickness. 
# They will not be taken into account for the inventory
# Replace negative values with NA
thperyear$h[thperyear$h<=0]<-NA
thperyear$hb[thperyear$hb<=0]<-NA
thperyear$hlm[thperyear$hlm<=0]<-NA
#Remove NAs
thperyear.cleanh<-thperyear[!is.na(thperyear$h),]
thperyear.cleanhb<-thperyear[!is.na(thperyear$hb),]
thperyear.cleanhlm<-thperyear[!is.na(thperyear$hlm),]

thperyear.cleanh$hserel<-thperyear.cleanh$hse/thperyear.cleanh$h
thperyear.cleanhb$hbserel<-thperyear.cleanhb$hbse/thperyear.cleanh$hb
thperyear.cleanhlm$hlmserel<-thperyear.cleanhlm$hlmse/thperyear.cleanh$hlm

#Compute the avg per year Careful does not take into account
#The previous computation of error. To keep on the side for other computation
th.avg.peryearh<-ddply(thperyear.cleanh, c('MYEAR'), function(x) c(counth=nrow(x),h.mean=mean(x$h),h.median=median(x$h),h.sd=sd(x$h),h.mad=mad(x$h),h.min=min(x$h),h.max=max(x$h)))
th.avg.peryearhb<-ddply(thperyear.cleanhb, c('MYEAR'), function(x) c(counthb=nrow(x),hb.mean=mean(x$hb),hb.median=median(x$hb),hb.sd=sd(x$hb),hb.mad=mad(x$hb),hb.min=min(x$hb),hb.max=max(x$hb)))
th.avg.peryearhlm<-ddply(thperyear.cleanhlm, c('MYEAR'), function(x) c(counthlm=nrow(x),hlm.mean=mean(x$hlm),hlm.median=median(x$hlm),hlm.sd=sd(x$hlm),hlm.mad=mad(x$hlm),hlm.min=min(x$hlm),hlm.max=max(x$hlm)))


####################
# 3. Computation of thickness on the regular grid
#
#
####################

#Look at the representation of the error on the regularly spaced grid
pre3<-predict(TCCZ.loess1,newdata = testgrid1,se = TRUE)
pre3b<-predict(TCCZ.loess1b,newdata = testgrid1,se = TRUE)
pre3lm<-predict(TCCZ.lm,newdata = testgrid1,se = TRUE, interval = "prediction",level = 0.95)

wll.loess<-llply(wll2, function(zzl) {loess(mean~EASTING+NORTHING, data=zzl,degree=1, span=alphaloesswl)})
wll.pred<-llply(wll.loess, function(m) {predict(m,newdata=testgrid1,se=TRUE)})

thicknessaq<-testgrid1
thicknessaq$TCCZfit<-as.vector(pre3$fit)
thicknessaq$TCCZfitb<-as.vector(pre3b$fit)
thicknessaq$TCCZfitlm<-as.vector(pre3lm$fit[,1])
thicknessaq$TCCZsefit<-as.vector(pre3$se.fit)
thicknessaq$TCCZsefitb<-as.vector(pre3b$se.fit)
thicknessaq$TCCZsefitlm<-as.vector(pre3lm$se.fit)

ov <- dim(thicknessaq)[2]
nbparam1<-3

# dimpredgrid<-dim(testgrid1)

nbparam1*(kk-1)+ov+1
nbparam1*(kk-1)+ov+2

nbnegthickvals<-vector(mode = "integer", length = length(wll2))

for (kk in 1:length(wll2)) {
  # Compute the locally weighted regression model
  w.loess<-loess(mean~EASTING+NORTHING, data=wll2[[kk]],degree=1, span=alphaloesswl)
  # Predict on the regular grid in the interpolation domain
  predw<-predict(w.loess,newdata = testgrid1 ,se = TRUE)
  # Store the fit
  thicknessaq[nbparam1*(kk-1)+ov+1]<-as.vector(predw$fit)
  names(thicknessaq)[nbparam1*(kk-1)+ov+1]<-paste0("wl",names(wll2)[kk])
  # Store the standard error
  thicknessaq[nbparam1*(kk-1)+ov+2]<-predt$se.fit
  names(thicknessaq)[nbparam1*(kk-1)+ov+2]<-paste0("se.wl",names(tritiuml2)[kk])
  # Compute the thickness
  thickness<-as.vector(predw$fit)-inv5$TCCZfitb
  # Count neg values for diagnostics purposes
  nbnegthickvals[kk]<-sum(thickness<0)
  # Replace negative values with NA
  thickness[thickness<0]<-NA
  
  thicknessaq[nbparam1*(kk-1)+ov+3]<-thickness
  names(thicknessaq)[nbparam1*(kk-1)+ov+3]<-paste0("e",names(wll2)[kk])
  
  inv5[nbparam1*(kk-1)+12]<-height
  names(inv5)[nbparam1*(kk-1)+12]<-paste0("h",names(wll2)[kk])
  inv5[nbparam1*(kk-1)+13]<-inv5[nbparam1*(kk-1)+9]*inv5[nbparam1*(kk-1)+12]
  names(inv5)[nbparam1*(kk-1)+13]<-paste0("ch",names(wll2)[kk])
  inv5[nbparam1*(kk-1)+14]<-inv5[nbparam1*(kk-1)+10]*inv5[nbparam1*(kk-1)+12]
  names(inv5)[nbparam1*(kk-1)+14]<-paste0("chfl",names(wll2)[kk])
}



