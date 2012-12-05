#Simple Computation
require(geoR)
require(geoRglm)
require(fields)
require(akima)
require(splancs)
require(plyr)
require(ggplot2)
require(scales)

# setwd("D:/CodeProjects/R/R_srs/inventory")
# setwd("D:/work/Code/srs_work/inventory")

#Clear all, set na.exclude
rm(list=ls())
options(na.action="na.exclude")
#options(na.action="na.omit")
#options("na.action")

ptm1<-proc.time()
###################
#0. Some needed functions
#
#Selected matrix functions from Henrik Bengtsson
# https://stat.ethz.ch/pipermail/r-help/2003-October/040484.html
# http://www1.maths.lth.se/help/R/image/image.R
# # Mirror matrix (left-right)
# mirror.matrix <- function(x) {
#   xx <- as.data.frame(x);
#   xx <- rev(xx);
#   xx <- as.matrix(xx);
#   xx;
# }
# # Rotate matrix 90 clockworks
# rotate90.matrix <- function(x) {
#   t(mirror.matrix(x))
# }
# # Rotate matrix 180 clockworks
# rotate180.matrix <- function(x) { 
#   xx <- rev(x);
#   dim(xx) <- dim(x);
#   xx;
# }
# # Rotate matrix 270 clockworks
# rotate270.matrix <- function(x) {
#   mirror.matrix(t(x))
# }

#################################
#1.
#load data
#nitrate<-readRDS("../SRS_data/nitrate.rdata")
nitrate<-readRDS("../SRS_data/nitrate.rdata")
nitrateC<-readRDS("../SRS_data/nitrateC.rdata")
nitrateavg<-readRDS("../SRS_data/nitrateavg.rdata")
nitrateCavg<-readRDS("../SRS_data/nitrateCavg.rdata")
wl<-readRDS("../SRS_data/wl.rdata")
TCCZe_all<-readRDS("../TCCZ_krig/TCCZ/TCCZ_o.rdata")
TCCZe<-TCCZe_all[!is.na(TCCZe_all$TCCZ_top),]
rm(TCCZe_all)
wlavg<-readRDS("../SRS_data/wlavg.rdata")
#basin coords for plotting if needed
#f3basin<-readRDS("../basin_coords/f3basin.rdata")
#f3basin27<-readRDS("../basin_coords/f3basin27.rdata")

#Add log transform
nitrate$logmean<-log(nitrate$mean)
nitrate$log10mean<-log10(nitrate$mean)
nitrateC$logmean<-log(nitrateC$mean)
nitrateC$log10mean<-log10(nitrateC$mean)


#Split per measurement year
wll<-split(wl,wl$MYEAR)
nitratel<-split(nitrate,nitrate$MYEAR)
nitrateCl<-split(nitrateC,nitrateC$MYEAR)
#Select 1988 and after
wll2<-wll[5:length(wll)]
nitratel2<-nitratel[4:length(nitratel)]
nitrateCl2<-nitrateCl[4:length(nitrateCl)]
# names(nitrateCl2)
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

#Create polygon to compute area.
d.ea<-c(ea.min,ea.min,ea.max,ea.max,ea.min)
d.no<-c(no.min,no.max,no.max,no.min,no.min)
pp<-cbind(d.ea,d.no)
#plot(pp, type="b")
area.dom<-areapl(pp)

#Define porosity value
porosity.mean<-.3
porosity.sd<-.03

#C thickness in m
Cth<-10*.3048

#########################################################
#3. Do simple average calculations on the well positions only
# \int_D C h p dx dy = \int_D dx dy * \hat{C} * \hat{h} * porosity p
#Thickness is computed using a linear prediction
#
#Local polynomial fit (1st order) and linear model
TCCZ.loess1<-loess(TCCZ_top~EASTING+NORTHING,data=TCCZe,degree= 1, span= 0.25)
TCCZ.loess1b<-loess(TCCZ_top~EASTING+NORTHING,data=TCCZe,degree= 1,span= 0.4)
TCCZ.lm<-lm(TCCZ_top~EASTING+NORTHING,data=TCCZe)

#Create the aquifer thickness data frame
thperyear<-wl
#Add the TCCZ values predicted by the linear models
# with standard error estimates
pre2<-predict(TCCZ.loess1,newdata = wl[,c("EASTING","NORTHING")],se = TRUE)
pre2b<-predict(TCCZ.loess1b,newdata = wl[,c("EASTING","NORTHING")],se = TRUE)
pre2lm<-predict(TCCZ.lm,newdata = wl[,c("EASTING","NORTHING")],se = TRUE, interval = "prediction",level = 0.95)
#
thperyear$TCCZ.fit<-pre2$fit
thperyear$TCCZ.se.fit<-pre2$se.fit
thperyear$TCCZ.fitb<-pre2b$fit
thperyear$TCCZ.se.fitb<-pre2b$se.fit
thperyear$TCCZ.fitlm<-pre2lm$fit[,1]
thperyear$TCCZ.se.fitlm<-pre2lm$se.fit
thperyear$TCCZ.fitlmupr<-pre2lm$fit[,2]
thperyear$TCCZ.fitlmlwr<-pre2lm$fit[,3]

#Compute the thickness in feet
thperyear$h<-thperyear$mean-thperyear$TCCZ.fit
thperyear$hb<-thperyear$mean-thperyear$TCCZ.fitb
thperyear$hlm<-thperyear$mean-thperyear$TCCZ.fitlm

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

#Create inventory df
inventoryja<-merge(nitrateavg[nitrateavg$MYEAR>=1984,],th.avg.peryearh,by="MYEAR")
inventoryja<-merge(inventoryja,th.avg.peryearhb,by="MYEAR")
inventoryja<-merge(inventoryja,th.avg.peryearhlm,by="MYEAR")

inventoryCja<-data.frame(MYEAR=nitrateCavg[,c("MYEAR")])
inventoryCja$inventoryC<-area.dom*porosity.mean*Cth*nitrateCavg$mean*1e-6

#Compute the inventory
inventoryja$inventory1<-area.dom*porosity.mean*inventoryja$h.mean*inventoryja$mean*.3048*1e-6
inventoryja$inventory1b<-area.dom*porosity.mean*inventoryja$hb.mean*inventoryja$mean*.3048*1e-6
inventoryja$inventory1lm<-area.dom*porosity.mean*inventoryja$hlm.mean*inventoryja$mean*.3048*1e-6
inventoryja<-merge(inventoryja,inventoryCja,by="MYEAR")

rm(thperyear)
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
ja.plot<- ja.plot +geom_line(aes(y=inventoryC), colour='violet')
ja.plot<-ja.plot+labs(title="Nitrate Inventory")+xlab("Year")+ylab("nitrate (kg)")
print(ja.plot)

# 
# # log2 scaling of the y axis (with visually-equal spacing)
# ja.plotlog1<- ja.plot + scale_y_continuous(trans=log10_trans())
# print(ja.plotlog1)
# # log2 coordinate transformation (with visually-diminishing spacing)
# ja.plotlog2<- ja.plot + coord_trans(y="log2")
# print(ja.plotlog2)

saveRDS(inventoryja, file = "inventoryja.rdata")

inventoryjaN.csv<-inventoryja[,c("MYEAR","inventory1","inventory1b","inventory1lm","inventoryC")]
write.csv(inventoryjaN.csv, file="inventoryja.csv")

########################################
#4. Do the calculation again, this time with interpolation on a regular grid.
# We should compute both 
# \int_D C * h * p dx dy = \int_D C dx dy/area.dom * \int_D h dx dy/area.dom * p
# = (\Sigma C)/N points on the regular grid * (\Sigma h)/N * p
# and
# \int_D C h dx dy = \int_D C*h dx dy * p
#The later will allow for the matching of concentration and aquifer thickness.
###########################################


######################################
#5. Using loess and prediction.

# Test Use of the loess model and linear models for TCCZ prediction
pre3<-predict(TCCZ.loess1,newdata = testgrid1,se = TRUE)
pre3b<-predict(TCCZ.loess1b,newdata = testgrid1,se = TRUE)
pre3lm<-predict(TCCZ.lm,newdata = testgrid1,se = TRUE)

wll.loess<-llply(wll2, function(zzl) {loess(mean~EASTING+NORTHING, data=zzl,degree=1, span=0.75)})
wl.pred<-llply(wll.loess, function(m) {predict(m,newdata=testgrid1,se=TRUE)})

nitratel.loess<-llply(nitratel2, function(zzl) {loess(mean~EASTING+NORTHING, data=zzl,degree=1,span=0.75)})
nitrate.pred<-llply(nitratel.loess, function(m) {predict(m,newdata=testgrid1,se =TRUE)})

inv5<-testgrid1
inv5$TCCZfit<-as.vector(pre3$fit)
inv5$TCCZfitb<-as.vector(pre3b$fit)
inv5$TCCZfitlm<-as.vector(pre3lm$fit)
inv5$TCCZsefit<-as.vector(pre3$se.fit)
inv5$TCCZsefitb<-as.vector(pre3b$se.fit)
inv5$TCCZsefitlm<-as.vector(pre3lm$se.fit)

nbparam1<-6

for (kk in 1:length(nitratel2)) {
  N.loess<-loess(mean~EASTING+NORTHING, data=nitratel2[[kk]],degree=1,span=0.5)
  logN.loess<-loess(logmean~EASTING+NORTHING, data=nitratel2[[kk]],degree=1,span=0.5)
  predN<-predict(N.loess,newdata = testgrid1 ,se = TRUE)
  predlogN<-predict(logN.loess,newdata = testgrid1 ,se = TRUE)
  fullfit<-as.vector(predN$fit)
  fullfit[fullfit<0]<-NA
  #fullfit[fullfit<0]<-1
  inv5[nbparam1*(kk-1)+9]<-fullfit
  names(inv5)[nbparam1*(kk-1)+9]<-paste0("N",names(nitratel2)[kk])
  logfullfit<-as.vector(predlogN$fit)
  Nfl<-exp(logfullfit)
  inv5[nbparam1*(kk-1)+10]<-Nfl
  names(inv5)[nbparam1*(kk-1)+10]<-paste0("Nfl",names(nitratel2)[kk])
  #inv5[nbparam1*(kk-1)+10]<-pdret$se.fit
  #names(inv5)[nbparam1*(kk-1)+10]<-paste0("seT",names(nitratel2)[kk])
  w.loess<-loess(mean~EASTING+NORTHING, data=wll2[[kk]],degree=1, span=0.5)
  predw<-predict(w.loess,newdata = testgrid1 ,se = TRUE)
  inv5[nbparam1*(kk-1)+11]<-as.vector(predw$fit)
  names(inv5)[nbparam1*(kk-1)+11]<-paste0("w",names(wll2)[kk])
  height<-as.vector(predw$fit)-inv5$TCCZfitb
  height[height<0]<-NA
  inv5[nbparam1*(kk-1)+12]<-height
  names(inv5)[nbparam1*(kk-1)+12]<-paste0("h",names(wll2)[kk])
  inv5[nbparam1*(kk-1)+13]<-inv5[nbparam1*(kk-1)+9]*inv5[nbparam1*(kk-1)+12]
  names(inv5)[nbparam1*(kk-1)+13]<-paste0("ch",names(wll2)[kk])
  inv5[nbparam1*(kk-1)+14]<-inv5[nbparam1*(kk-1)+10]*inv5[nbparam1*(kk-1)+12]
  names(inv5)[nbparam1*(kk-1)+14]<-paste0("chfl",names(wll2)[kk])
}

 nbparam1C<-4
inv5C<-testgrid1
for (kk2 in 1:length(nitrateCl2)) {
  N.loess<-loess(mean~EASTING+NORTHING, data=nitrateCl2[[kk2]],degree=1,span=0.5)
  logN.loess<-loess(logmean~EASTING+NORTHING, data=nitrateCl2[[kk2]],degree=1,span=0.5)
  predN<-predict(N.loess,newdata = testgrid1 ,se = TRUE)
  predlogN<-predict(logN.loess,newdata = testgrid1 ,se = TRUE)
  fullfit<-as.vector(predN$fit)
  #fullfit[fullfit<0]<-NA
  #fullfit[fullfit<0]<-1
  fullfit[fullfit<0]<-0
  inv5C[nbparam1C*(kk2-1)+3]<-fullfit
  names(inv5C)[nbparam1C*(kk2-1)+3]<-paste0("N",names(nitrateCl2)[kk2])
  logfullfit<-as.vector(predlogN$fit)
  Tfl<-exp(logfullfit)
  inv5C[nbparam1C*(kk2-1)+4]<-Tfl
  names(inv5C)[nbparam1C*(kk2-1)+4]<-paste0("Nfl",names(nitrateCl2)[kk2])
  #inv5C[nbparam1*(kk-1)+10]<-pdret$se.fit
  #names(inv5C)[nbparam1*(kk-1)+10]<-paste0("seT",names(nitratel2)[kk])
  inv5C[nbparam1C*(kk2-1)+5]<-inv5C[nbparam1C*(kk2-1)+3]*Cth
  names(inv5C)[nbparam1C*(kk2-1)+5]<-paste0("ch",names(nitrateCl2)[kk2])
  inv5C[nbparam1C*(kk2-1)+6]<-inv5C[nbparam1C*(kk2-1)+4]*Cth
  names(inv5C)[nbparam1C*(kk2-1)+6]<-paste0("chfl",names(nitrateCl2)[kk2])
}



# ggtest2<-ggplot(inv5, aes(x=EASTING,y=NORTHING)) + geom_tile(aes(fill=TCCZfitb))
# ggtest2<-ggtest2+scale_fill_gradient(limits=range(inv5$TCCZfitb, na.rm = TRUE), low="red", high="white")
# print(ggtest2)
# 
# ggtest2a<-ggplot(inv5, aes(x=EASTING,y=NORTHING)) + geom_tile(aes(fill=T1996))
# ggtest2a<-ggtest2a+scale_fill_gradient2()
# print(ggtest2)
# 
# ggtestC2a<-ggplot(inv5C, aes(x=EASTING,y=NORTHING)) + geom_tile(aes(fill=T1996))
# ggtestC2a<-ggtestC2a+scale_fill_gradient2()
# print(ggtestC2a)
# 
# ggtest2b<-ggplot(inv5, aes(x=EASTING,y=NORTHING)) + geom_tile(aes(fill=h2003))
# ggtest2b<-ggtest2b+scale_fill_gradient2()
# print(ggtest2b)
# 
# ggtest3<-ggplot(inv5, aes(x=EASTING,y=NORTHING)) + geom_tile(aes(fill=TCCZfitb), colour = "white",linetype = 0) + scale_fill_gradient(low = "white",high = "red")
# print(ggtest3)

# ggtest4<-ggplot(inv5, aes(x=EASTING,y=NORTHING, z=T1996)) + stat_contour(aes(colour = ..level..),breaks=c(0,100000,1000000))
# #ggtest4<-ggtest4+scale_colour_gradient(limits=range(inv5$TCCZfitb, na.rm = TRUE), low="red", high="white")
# print(ggtest4)
# 
# ggtest5<-ggplot(inv5, aes(x=EASTING,y=NORTHING, z=Tfl1996)) + stat_contour(aes(colour = ..level..),breaks=c(.1,1,1000,100000,1000000))
# #ggtest4<-ggtest4+scale_colour_gradient(limits=range(inv5$TCCZfitb, na.rm = TRUE), low="red", high="white")
# print(ggtest5)

#qplot(x=h1994,y=T1994,data=inv5)

inventory5<-data.frame(MYEAR=seq(1988,2011,length=24))
for (jj2 in 1:length(inventory5$MYEAR)) {
  inventory5$meanch[jj2]<-mean(inv5[[nbparam1*(jj2-1)+13]], na.rm=TRUE)
  inventory5$medianch[jj2]<-median(inv5[[nbparam1*(jj2-1)+13]], na.rm=TRUE)
  inventory5$sdch[jj2]<-sd(inv5[[nbparam1*(jj2-1)+13]], na.rm=TRUE)
#   print("invOK")
  inventory5$meanchfl[jj2]<-mean(inv5[[nbparam1*(jj2-1)+14]], na.rm=TRUE)
  inventory5$medianchfl[jj2]<-median(inv5[[nbparam1*(jj2-1)+14]], na.rm=TRUE)
  inventory5$sdchfl[jj2]<-sd(inv5[[nbparam1*(jj2-1)+14]], na.rm=TRUE)
  inventory5$meanchC[jj2]<-mean(inv5C[[nbparam1C*(jj2-1)+5]], na.rm=TRUE)
#   print("Cok")
#   print(jj2)
}
inventory5$N<-area.dom*porosity.mean*inventory5$meanch*1e-6*.3048
inventory5$Nmed<-area.dom*porosity.mean*inventory5$medianch*1e-6*.3048
inventory5$Nfl<-area.dom*porosity.mean*inventory5$meanchfl*1e-6*.3048
inventory5$Nmedfl<-area.dom*porosity.mean*inventory5$medianchfl*1e-6*.3048
inventory5$NC<-area.dom*porosity.mean*inventory5$meanchC*1e-12
inventory5$NCD<-inventory5$N+inventory5$NC

inventoryN.final<-merge(inventoryjaN.csv, inventory5, by="MYEAR")

# pdf(file="%GDRIVE%\test\testinv.pdf",paper="letter")
# final.plot<-ggplot(data=inventory.final, aes(x=MYEAR))
# final.plot<- final.plot +geom_line(aes(y=inventory1), colour='blue')
# final.plot<- final.plot +geom_line(aes(y=inventory1b), colour='red')
# final.plot<- final.plot +geom_line(aes(y=inventory1lm), colour='green')
# final.plot<- final.plot +geom_line(aes(y=t), colour='orange')
# #final.plot<- final.plot +geom_line(aes(y=tmed), colour='black')
# final.plot<- final.plot + scale_y_log10()
# #scale_y_continuous(trans=log2_trans())
# final.plot<-final.plot+labs(title="nitrate Inventory")+xlab("Year")+ylab("nitrate (Ci)")
# print(final.plot)
# dev.off()

# final.plot2<-ggplot(data=inventory.final, aes(x=MYEAR))
# final.plot2<- final.plot2 +geom_point(aes(y=inventory1), colour='blue')
# final.plot2<- final.plot2 +geom_point(aes(y=inventory1b), colour='red')
# final.plot2<- final.plot2 +geom_point(aes(y=inventory1lm), colour='green')
# final.plot2<- final.plot2 +geom_point(aes(y=t), colour='orange')
# final.plot2<- final.plot2 + scale_y_log10()
# final.plot2<-final.plot2+labs(title="nitrate Inventory")+xlab("Year")+ylab("nitrate (Ci)")
# print(final.plot2)

# final.plot3<-ggplot(data=inventory.final, aes(x=MYEAR))
# final.plot3<- final.plot3 +geom_point(aes(y=inventory1), colour='blue')
# final.plot3<- final.plot3 +geom_point(aes(y=inventory1b), colour='red')
# final.plot3<- final.plot3 +geom_point(aes(y=inventory1lm), colour='green')
# final.plot3<- final.plot3 +geom_point(aes(y=t), colour='orange')
# #final.plot3<- final.plot3 +geom_point(aes(y=tfl), colour='black')
# final.plot3<- final.plot3 +geom_point(aes(y=tC), colour='violet')
# final.plot3<- final.plot3 +geom_point(aes(y=tCD), colour='yellow')
# # final.plot3<- final.plot3 + scale_y_log10()
# final.plot3<-final.plot3+labs(title="nitrate Inventory")+xlab("Year")+ylab("nitrate (Ci)")
# print(final.plot3)
# 
# 
# final.plot4<-ggplot(data=inventory.final, aes(x=MYEAR))
# final.plot4<- final.plot4 +geom_point(aes(y=inventory1), colour='blue')
# final.plot4<- final.plot4 +geom_point(aes(y=inventory1b), colour='red')
# final.plot4<- final.plot4 +geom_point(aes(y=inventory1lm), colour='green')
# final.plot4<- final.plot4 +geom_point(aes(y=t), colour='orange')
# # final.plot4<- final.plot4 +geom_point(aes(y=tfl), colour='black')
# final.plot4<- final.plot4 + scale_y_log10()
# final.plot4<-final.plot4+labs(title="nitrate Inventory")+xlab("Year")+ylab("nitrate (Ci)")
# print(final.plot4)


saveRDS(inventoryN.final,"inventoryfinalN.rdata")

print(proc.time() - ptm1)









################################################
################################################
################################################
#Leftovers
# 
#
#Call loess
#second order polynomial
#TCCZ.loess2 = loess(TCCZ_top~EASTING+NORTHING, data = TCCZe, degree = 2, span = 0.25)
#predict(TCCZ.loess1,newdata = wlavg[,c("EASTING","NORTHING")], na.action = na.omit)

############################################
#3. a)Computation with the average water level
# thavg<-wlavg
# pre1<-predict(TCCZ.loess1,newdata = wlavg[,c("EASTING","NORTHING")],se=TRUE)
# pre1b<-predict(TCCZ.loess1b,newdata = wlavg[,c("EASTING","NORTHING")],se = TRUE)
# pre1lm<-predict(TCCZ.lm,newdata = wlavg[,c("EASTING","NORTHING")],se = TRUE)
#
# thavg$TCCZ.fit<-pre1$fit
# thavg$TCCZ.se.fit<-pre1$se.fit
# thavg$TCCZ.fitb<-pre1b$fit
# thavg$TCCZ.se.fitb<-pre1b$se.fit
# thavg$TCCZ.fitlm<-pre1lm$fit
# thavg$TCCZ.se.fitlm<-pre1lm$se.fit
#Compute the thickness in feet
# thavg$h<-thavg$mean-thavg$TCCZ.fit
# Replace negative values with NA
# thavg$h[thavg$h<=0]<-NA
#Remove NAs
# thavg.clean<-thavg[!is.na(thavg$h),]
#####################################





#Early inventory calculations
#inventoryjahb<-merge(nitrateavg[nitrateavg$MYEAR>=1984,],th.avg.peryearhb,by="MYEAR")
#inventoryjahlm<-merge(nitrateavg[nitrateavg$MYEAR>=1984,],th.avg.peryearhlm,by="MYEAR")
#inventoryja[,c("count","h.mean","h.median","h.sd","h.mad","h.min","h.max")]<-th.avg.peryear[, c("count","h.mean","h.median","h.sd","h.mad","h.min","h.max")]
#inventoryjah$inventory<-area.dom*porosity.mean*inventoryjah$h.mean*inventoryjah$mean*.3048*1e-9
#inventoryjahb$inventory<-area.dom*porosity.mean*inventoryjahb$hb.mean*inventoryjahb$mean*.3048*1e-9
#inventoryjahlm$inventory<-area.dom*porosity.mean*inventoryjahlm$hlm.mean*inventoryjahlm$mean*.3048*1e-9



#
#selectyear<-function (x,y) {subset(x, x$MYEAR == y)}
#nitrate.1984<-selectyear(nitrate, 1984)
#TCCZ.lm<-lm(TCCZ_top~UTM_E+UTM_N,data=TCCZe,na.action=na.omit)
#nitrate.interp<-interp(nitrate$EASTING, nitrate$NORTHING, nitrate$mean, xo=seq(ea.min, ea.max, length = ea.b), yo=seq(no.min, no.max, length = no.b), linear = TRUE, extrap=FALSE, duplicate = "mean")
#wlavg.lm<-lm(mean~EASTING+NORTHING, data =wlavg)
#prewllm<-predict(wlavg.lm,newdata = testgrid1,se = TRUE ,na.action = na.omit)





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
#TCCZ.loess1.interp<-list(x=matrix(testgrid1$EASTING,nrow=60, ncol=50, byrow=TRUE),y=matrix(testgrid1$NORTHING,nrow=60, ncol=50, byrow=TRUE),z=matrix(pre3$fit,nrow=60, ncol=50, byrow=TRUE))
#TCCZ.loess1b.interp<-list(x=matrix(testgrid1$EASTING,nrow=60, ncol=50, byrow=TRUE),y=matrix(testgrid1$NORTHING,nrow=60, ncol=50, byrow=TRUE),z=matrix(pre3$fit,nrow=60, ncol=50, byrow=TRUE))



###################################
#AKIMA version
# 
# #Define short hand for AKIMA interpolation functions without and with extrapolation
# interp.peryear<- function(x) AKIMA::interp(x$EASTING, x$NORTHING, x$mean, xo=ea.v, yo=no.v, linear = TRUE, extrap=FALSE, duplicate = "mean");
# interpext.peryear<- function(x) AKIMA::interp(x$EASTING, x$NORTHING, x$mean, xo=ea.v, yo=no.v, linear = FALSE, extrap=TRUE, duplicate = "mean");
# 
# #Interpolation for TCCZ
# TCCZ.interp<-AKIMA::interp(TCCZe$EASTING, TCCZe$NORTHING, TCCZe$TCCZ_top, xo=ea.v, yo=no.v, linear = TRUE, duplicate = "error")
# 
# #Interpolation
# wll.interp<-llply(wll, interp.peryear)
# nitratel.interp<-llply(nitratel, interp.peryear)
# #Extrapolation
# wll.interpext<-llply(wll, interpext.peryear, .progress = "text")
# nitratel.interpext<-llply(nitratel, interpext.peryear, .progress = "text")
# 
# #create aquifer thickness
#  thickness<-llply(wll.interp,function(ll){list(x=ll$x, y=ll$y, z=as.matrix(0.3048*(ll$z-TCCZ.interp$z)))}, .progress = "text")

# image.plot(TCCZ.interp)
# image.plot(wll.interp['1996'][[1]],asp = 1)
# image.plot(nitratel.interp['1992'][[1]],asp = 1)
# image.plot(wll.interpext['1988'][[1]],asp = 1)
# image.plot(nitratel.interpext['1992'][[1]],asp = 1)
# contour(wll.interpext['1988'][[1]][[1]],wll.interpext['1988'][[1]][[2]],wll.interpext['1988'][[1]][[3]])
# contour(wll.interp['1988'][[1]][[1]],wll.interp['1988'][[1]][[2]],wll.interp['1988'][[1]][[3]])
#image.plot(x=matrix(testgrid1$EASTING,nrow=60, ncol=50, byrow=TRUE),y=matrix(testgrid1$NORTHING,nrow=60, ncol=50, byrow=TRUE),z=thickness['1996'][[1]][[3]])

#Make into a dataframe for ggplot
# thicknessdf<-testgrid1
# for (jj in 1:length(thickness)) {
#   thicknessdf[2+jj]<-as.vector(thickness[jj][[1]][[3]])
#   names(thicknessdf)[2+jj]<-paste0("th",names(thickness)[jj])
# }
# gg96<-ggplot(data=thicknessdf,aes(x=EASTING,y=NORTHING)) + geom_tile(aes(fill=th1996))
# gg96
#contour(testthickness['1988'][[1]][[1]],testthickness['1988'][[1]][[2]],testthickness['1988'][[1]][[3]])




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

# wlavg.interp<-interp(wlavg$EASTING, wlavg$NORTHING, wlavg$mean, xo=ea.v, yo=no.v, linear = TRUE, duplicate = "mean")

####################################
#Loop test
#
# 
# t.loess<-loess(mean~EASTING+NORTHING, data=nitratel2[['1996']],degree=1,span=0.5)
# logt.loess<-loess(logmean~EASTING+NORTHING, data=nitratel2[['1996']],degree=1,span=0.5)
# predt<-predict(t.loess,newdata = testgrid1 ,se = TRUE)
# predlogt<-predict(logt.loess,newdata = testgrid1 ,se = TRUE)
# T1996<-as.vector(predt$fit)
# Tcl1996<-T1996
# Tcl1996[Tcl1996<0]<-NA
# LogT1996<-as.vector(predlogt$fit)
# Tprime1996<-exp(LogT1996)
# w.loess<-loess(mean~EASTING+NORTHING, data=wll2[['1996']],degree=1, span=0.5)
# predw<-predict(w.loess,newdata = testgrid1 ,se = TRUE)
# w1996<-as.vector(predw$fit)
# h1996<-as.vector(predw$fit)-inv5$TCCZfitb
# h1996[h1996<1]<-NA
# ch1996<-T1996*h1996
# chcl1996<-Tcl1996*h1996
# chprime1996<-Tprime1996*h1996
# 
# t1<-area.dom*porosity.mean*mean(ch1996,na.rm=TRUE)*1e-9*.3048
# tcl1<-area.dom*porosity.mean*mean(chcl1996,na.rm=TRUE)*1e-9*.3048
# tlog1<-area.dom*porosity.mean*mean(chprime1996,na.rm=TRUE)*1e-9*.3048

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