#Aquifer cross validation

#################
# 0. Import the need data TCCZ and Water levels
#
#################

#Import picks for the TCCZ
TCCZe<-readRDS("./geo_data/processed/TCCZ_wtoppick.rdata")


#Import the water level data
wl<-readRDS("../SRS_data/wl.rdata")
wlavg<-readRDS("../SRS_data/wlavg.rdata")

# Select 1988 and after :
wlp1988 <- wl[wl$MYEAR > 1987,]
wll2 <- split(wl,wl$MYEAR)

# 
# #Split per measurement year
# wll<-split(wl,wl$MYEAR)
# #Select 1988 and after
# wll2<-wll[5:length(wll)]
#
XvalFitTCCZ <- data.frame( alphaspan = seq( .05, 1, .05))

XvalFitTCCZ$SSEd1 <- rep(NA, nrow(XvalFitTCCZ))
#Set up the predicted residual sum of squares
XvalFitTCCZ$PRESSd1 <- rep(NA, nrow(XvalFitTCCZ))
XvalFitTCCZ$SSEd2 <- rep(NA, nrow(XvalFitTCCZ))
XvalFitTCCZ$PRESSd2 <- rep(NA, nrow(XvalFitTCCZ))
# XvalFitTCCZ$SSEd3 <- rep(NA, nrow(XvalFitTCCZ))
# XvalFitTCCZ$PRESSd3 <- rep(NA, nrow(XvalFitTCCZ))
# XvalFitTCCZ$SSEd4 <- rep(NA, nrow(XvalFitTCCZ))
# XvalFitTCCZ$PRESSd4 <- rep(NA, nrow(XvalFitTCCZ))


for ( ii in 1:nrow(XvalFitTCCZ) ) {
  aspan <- XvalFitTCCZ$alphaspan[ii]
  fitd1 <- loess(TCCZ_top~EASTING+NORTHING,data=TCCZe,degree= 1, span= aspan)
  fitd2 <- loess(TCCZ_top~EASTING+NORTHING,data=TCCZe,degree= 2, span= aspan)
  # fitd3 <- loess(TCCZ_top~EASTING+NORTHING,data=TCCZe,degree= 3, span= aspan)
  # fitd4 <- loess(TCCZ_top~EASTING+NORTHING,data=TCCZe,degree= 4, span= aspan)
  SSEd1 <- sum( ((TCCZe$TCCZ_top - predict(fitd1))[-c(1,nrow(TCCZe))]) ^ 2)
  SSEd2 <- sum( ((TCCZe$TCCZ_top - predict(fitd2))[-c(1,nrow(TCCZe))]) ^ 2)
  pressd1 <- 0
  pressd2 <- 0
  for ( obs in 2: (nrow(TCCZe)-1)) {
    fitd1cv <- loess(TCCZ_top~EASTING+NORTHING,data=TCCZe,degree= 1, span= aspan)
    fitd2cv <- loess(TCCZ_top~EASTING+NORTHING,data=TCCZe,degree= 2, span= aspan)
    predd1cv <- predict(fitd1cv, newdata = TCCZe[obs,] )
    predd2cv <- predict(fitd2cv, newdata = TCCZe[obs,] )

    pressd1 <- pressd1 + (TCCZe[obs,'TCCZ_top'] - predd1cv)^2
    pressd2 <- pressd2 + (TCCZe[obs,'TCCZ_top'] - predd2cv)^2
  } 
  XvalFitTCCZ$SSEd1[ii] <- SSEd1
  XvalFitTCCZ$PRESSd1[ii] <- pressd1
  XvalFitTCCZ$SSEd2[ii] <- SSEd2
  XvalFitTCCZ$PRESSd2[ii] <- pressd2
}
XvalFitTCCZ
with( XvalFitTCCZ, matplot( alphaspan, cbind(PRESSd1,SSEd1), type = 'l') )

with( XvalFitTCCZ, matplot( alphaspan, cbind(PRESSd2,SSEd2), type = 'l') )

with( XvalFitTCCZ, matplot( alphaspan, cbind(PRESSd1,PRESSd2), type = 'l') )

# plot( y ~ x, dd)
# fit <- loess ( y ~ x, dd, span = .15 ,degree=1)   
# lines( predict(fit)  ~ x, dd)  


#Local polynomial fit (1st order) and linear model
# TCCZ.loess1<-loess(TCCZ_top~EASTING+NORTHING,data=TCCZe,degree= 1, span= alphaloess1)
# TCCZ.loess2<-loess(TCCZ_top~EASTING+NORTHING,data=TCCZe,degree= 2,span= alphaloess2)
# TCCZ.lm<-lm(TCCZ_top~EASTING+NORTHING,data=TCCZe)

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
# thperyear$TCCZ.fit<-pre2$fit
# thperyear$TCCZ.se.fit<-pre2$se.fit
# thperyear$TCCZ.fitb<-pre2b$fit
# thperyear$TCCZ.se.fitb<-pre2b$se.fit
# thperyear$TCCZ.fitlm<-pre2lm$fit[,1]
# thperyear$TCCZ.se.fitlm<-pre2lm$se.fit
# #Upper
# thperyear$TCCZ.fitlmupr<-pre2lm$fit[,2]
# #Lower Bound
# thperyear$TCCZ.fitlmlwr<-pre2lm$fit[,3]
# 
# #Compute the thickness in feet
# thperyear$h<-thperyear$mean-thperyear$TCCZ.fit
# thperyear$hb<-thperyear$mean-thperyear$TCCZ.fitb
# thperyear$hlm<-thperyear$mean-thperyear$TCCZ.fitlm
# 
# #Compute the standard error of that estimate
# thperyear$hse<-sqrt(max(1,thperyear$sd, na.rm =TRUE)^2+thperyear$TCCZ.se.fit^2)
# thperyear$hbse<-sqrt(max(1,thperyear$sd, na.rm =TRUE)^2+thperyear$TCCZ.se.fitb^2)
# thperyear$hlmse<-sqrt(max(1,thperyear$sd, na.rm =TRUE)^2+thperyear$TCCZ.se.fitlm^2)
# 
# # #Diagnostics on the thickness
# # summary(thperyear$hlm)
# # #ggplot
# # ph <- ggplot(thperyear, aes(x=EASTING,y=NORTHING)) + geom_point(aes(colour=hlm), size = 10) + scale_colour_gradient2(low="red", high="blue") 
# # print(ph)
# # #Close up in the area + 100m margin to the interpolation domain.
# # ph2 <- ph + xlim(ea.min-100,ea.max+100) + ylim(no.min-100,no.max+100)
# # print(ph2)
# 
# # In this case, I am removing points that are attributed a negative aq thickness. 
# # They will not be taken into account for the inventory
# # Replace negative values with NA
# thperyear$h[thperyear$h<=0]<-NA
# thperyear$hb[thperyear$hb<=0]<-NA
# thperyear$hlm[thperyear$hlm<=0]<-NA
# #Remove NAs
# thperyear.cleanh<-thperyear[!is.na(thperyear$h),]
# thperyear.cleanhb<-thperyear[!is.na(thperyear$hb),]
# thperyear.cleanhlm<-thperyear[!is.na(thperyear$hlm),]
# 
# thperyear.cleanh$hserel<-thperyear.cleanh$hse/thperyear.cleanh$h
# thperyear.cleanhb$hbserel<-thperyear.cleanhb$hbse/thperyear.cleanhb$hb
# thperyear.cleanhlm$hlmserel<-thperyear.cleanhlm$hlmse/thperyear.cleanhlm$hlm
# 
# #Compute the avg per year Careful does not take into account
# #The previous computation of error. To keep on the side for other computation
# th.avg.peryearh<-ddply(thperyear.cleanh, c('MYEAR'), function(x) c(counth=nrow(x),h.mean=mean(x$h),h.median=median(x$h),h.sd=sd(x$h),h.mad=mad(x$h),h.min=min(x$h),h.max=max(x$h)))
# th.avg.peryearhb<-ddply(thperyear.cleanhb, c('MYEAR'), function(x) c(counthb=nrow(x),hb.mean=mean(x$hb),hb.median=median(x$hb),hb.sd=sd(x$hb),hb.mad=mad(x$hb),hb.min=min(x$hb),hb.max=max(x$hb)))
# th.avg.peryearhlm<-ddply(thperyear.cleanhlm, c('MYEAR'), function(x) c(counthlm=nrow(x),hlm.mean=mean(x$hlm),hlm.median=median(x$hlm),hlm.sd=sd(x$hlm),hlm.mad=mad(x$hlm),hlm.min=min(x$hlm),hlm.max=max(x$hlm)))

