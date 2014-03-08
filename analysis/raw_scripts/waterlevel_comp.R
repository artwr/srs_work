#Computation of the phreatic aquifer thickness
#Source the file at the beginning of each analysis for a contaminant.

#Load packages
require(plyr)

#Import the water level data
wl<-readRDS("./srs_data/processed/wl.rdata")
wlavg<-readRDS("./srs_data/processed/wlavg.rdata")

# Faster
# Select 1988 and after :
wlp1988 <- wl[wl$MYEAR > 1987,]
# Split into a list by year
wll2 <- split(wlp1988,wlp1988$MYEAR)

alphaloesswl <- seq(.25, .9, by = 0.05)

# Use helper function to compute a range of loess models
wlloessmd2 <- dlply(.data= wlp1988, .variables = .(MYEAR),.fun = function(wlsub) {loessAlphaVect(lformula = "mean~EASTING+NORTHING", ldata = wlsub, ldegree = 2, spanvector = alphaloesswl, lfamily = "gaussian", lcontrol = lcontrold)})
wlloessmd1 <- dlply(.data= wlp1988, .variables = .(MYEAR),.fun = function(wlsub) {loessAlphaVect(lformula = "mean~EASTING+NORTHING", ldata = wlsub, ldegree = 1, spanvector = alphaloesswl, lfamily = "gaussian", lcontrol = lcontrold)})



# Thickness Computation
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

thperyear[thperyear$TCCZ.fitb < 0, c("STATION_ID", "EASTING", "NORTHING", "mean", "TCCZ.fit", "TCCZ.fitb", "TCCZ.fitlm")]
thperyear[thperyear$TCCZ.se.fitb > 200, ]

#Compute the thickness in feet
thperyear$h<-thperyear$mean-thperyear$TCCZ.fit
thperyear$hb<-thperyear$mean-thperyear$TCCZ.fitb
thperyear$hlm<-thperyear$mean-thperyear$TCCZ.fitlm

#Compute the standard error of that estimate
thperyear$hse<-sqrt(pmax(1,thperyear$sd, na.rm =TRUE)^2+thperyear$TCCZ.se.fit^2)
thperyear$hbse<-sqrt(pmax(1,thperyear$sd, na.rm =TRUE)^2+thperyear$TCCZ.se.fitb^2)
thperyear$hlmse<-sqrt(pmax(1,thperyear$sd, na.rm =TRUE)^2+thperyear$TCCZ.se.fitlm^2)
summary(thperyear)
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
thperyear.cleanhb$hbserel<-thperyear.cleanhb$hbse/thperyear.cleanhb$hb
thperyear.cleanhlm$hlmserel<-thperyear.cleanhlm$hlmse/thperyear.cleanhlm$hlm

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
pre3<-predict(TCCZ.loess1,newdata = interpolation.grid,se = TRUE);
pre3b<-predict(TCCZ.loess1b,newdata = interpolation.grid,se = TRUE);
pre3lm<-predict(TCCZ.lm,newdata = interpolation.grid,se = TRUE, interval = "prediction",level = 0.95);

# wll.loess<-llply(wll2, function(zzl) {loess(mean~EASTING+NORTHING, data=zzl,degree=1, span=alphaloesswl)});
# wll.pred<-llply(wll.loess, function(m) {predict(m,newdata=interpolation.grid,se=TRUE)});

thicknessUAZ<-interpolation.grid;
dim(pre3$fit)
thicknessUAZ$TCCZfit<-as.vector(pre3$fit);
thicknessUAZ$TCCZfitb<-as.vector(pre3b$fit);
thicknessUAZ$TCCZfitlm<-as.vector(pre3lm$fit[,1]);
thicknessUAZ$TCCZsefit<-as.vector(pre3$se.fit);
thicknessUAZ$TCCZsefitb<-as.vector(pre3b$se.fit);
thicknessUAZ$TCCZsefitlm<-as.vector(pre3lm$se.fit);

ov <- dim(thicknessUAZ)[2];
nbparamUAZ<-4;

# dimpredgrid<-dim(interpolation.grid)

nbnegthickvals<-vector(mode = "integer", length = length(wll2));
nbNAthickvals<-vector(mode = "integer", length = length(wll2));



for (kk in 1:length(wll2)) {
  # Compute the locally weighted regression model
  w.loess<-loess(mean~EASTING+NORTHING, data=wll2[[kk]],degree=1, span=alphaloesswl);
  # Predict on the regular grid in the interpolation domain
  predw<-predict(w.loess,newdata = interpolation.grid ,se = TRUE);
  # Store the fit
  thicknessUAZ[nbparamUAZ*(kk-1)+ov+1]<-as.vector(predw$fit);
  names(thicknessUAZ)[nbparamUAZ*(kk-1)+ov+1]<-paste0("wl",names(wll2)[kk]);
  # Store the standard error
  thicknessUAZ[nbparamUAZ*(kk-1)+ov+2]<-as.vector(predw$se.fit);
  names(thicknessUAZ)[nbparamUAZ*(kk-1)+ov+2]<-paste0("se.wl",names(wll2)[kk]);
  # Compute the thickness
  thickness<-as.vector(predw$fit)-thicknessUAZ$TCCZfitb;
  # Count neg values for diagnostics purposes
  nbnegthickvals[kk]<-sum(thickness<0, na.rm=TRUE);
  # Replace negative values with NA
  thickness[thickness<0]<-NA
  nbNAthickvals[kk]<-sum(is.na(thickness));
  # Store the thickness
  thicknessUAZ[nbparamUAZ*(kk-1)+ov+3]<-thickness;
  names(thicknessUAZ)[nbparamUAZ*(kk-1)+ov+3]<-paste0("e",names(wll2)[kk]);
  #Compute the standard error
  thicknessUAZ[nbparamUAZ*(kk-1)+ov+4]<-sqrt(thicknessUAZ[nbparamUAZ*(kk-1)+ov+2]^2 + thicknessUAZ$TCCZsefitb^2);
  names(thicknessUAZ)[nbparamUAZ*(kk-1)+ov+4]<-paste0("se.e",names(wll2)[kk]);
}

interpolation.gridC<-interpolation.grid;
thicknessLAZ<-interpolation.gridC;
thicknessLAZ$e.in.m<-Cth;

rm(interpolation.gridC)

thickness.regression.diagnostics<-as.data.frame(cbind(nbnegthickvals,nbNAthickvals))


#Save the datasets
saveRDS(thicknessUAZ, file = "./analysis/processed_data/thicknessUAZ.rdata")
saveRDS(thicknessLAZ, file = "./analysis/processed_data/thicknessLAZ.rdata")
saveRDS(thickness.regression.diagnostics, file = "./analysis/processed_data/diagnostics/thickness.regression.diagnostics.rdata")



