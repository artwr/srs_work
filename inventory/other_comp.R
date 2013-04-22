#Simple Computation for Tritium

#set na.exclude
options(na.action="na.exclude")
#options(na.action="na.omit")
#options("na.action")

# Alpha.loess
#
alphaloessconch<-0.75
alphaloessconc<-0.5
alphaloessconcl<-0.3

#################################
#1.
#load data
#Just the F wells
#tritium<-readRDS("../SRS_data/tritium.rdata")
#Added HSB 143 D which provides a BC on the eastern site of the interpolation domain
# tritium<-readRDS("../SRS_data/tritiumf.rdata")
# tritiumC<-readRDS("../SRS_data/tritiumC.rdata")
# tritiumavg<-readRDS("../SRS_data/tritiumavg.rdata")
# tritiumCavg<-readRDS("../SRS_data/tritiumCavg.rdata")

iodine<-readRDS("../SRS_data/iodine.rdata")
strontium<-readRDS("../SRS_data/strontium.rdata")
cesium137<-readRDS("../SRS_data/cesium137.rdata")
technetium<-readRDS("../SRS_data/technetium.rdata")

# #Add log transform
# tritium$logmean<-log(tritium$mean)
# tritium$log10mean<-log10(tritium$mean)
# tritiumC$logmean<-log(tritiumC$mean)
# tritiumC$log10mean<-log10(tritiumC$mean)

#Split per measurement year
# tritiuml<-split(tritium,tritium$MYEAR)
# tritiumCl<-split(tritiumC,tritiumC$MYEAR)

iodinel<-split(iodine,iodine$MYEAR)
names(technetiuml)
strontiuml<-split(strontium,strontium$MYEAR)
cesium137l<-split(cesium137,cesium137$MYEAR)
technetiuml<-split(technetium,technetium$MYEAR)
#Select 1993 and after no data in 1991 and 1992
iodinel2<-iodinel[3:length(iodinel)]
#Select after 1993
strontiuml2<-strontiuml[4:length(strontiuml)]
#After 1989
cesium137l2<-cesium137l[5:length(cesium137l)]
#Data after 1993
technetiuml2<-technetiuml[3:length(technetiuml)]
# tritiumCl2<-tritiumCl[3:length(tritiumCl)]
# names(tritiuml2)
names(iodinel2)
names(strontiuml2)
names(cesium137l2)
names(technetiuml2)
names(thicknessUAZ)


#########################################################
#2. Do simple average calculations on the well positions only
# \int_D C h p dx dy = \int_D dx dy * \hat{C} * \hat{h} * porosity p
#Thickness is computed using a linear prediction
#Works ok because the sampling wells do "importance sampling"
#on the aquifer

#Create inventory df
# inventoryja<-merge(tritiumavg[tritiumavg$MYEAR>=1984,],th.avg.peryearh,by="MYEAR")
# inventoryja<-merge(inventoryja,th.avg.peryearhb,by="MYEAR")
# inventoryja<-merge(inventoryja,th.avg.peryearhlm,by="MYEAR")
# 
# inventoryCja<-data.frame(MYEAR=tritiumCavg[,c("MYEAR")])
# inventoryCja$inventoryC<-area.dom*porosity.mean*Cth*tritiumCavg$mean*1e-9
# inventoryCja$inventoryCmedian<-area.dom*porosity.mean*Cth*tritiumCavg$median*1e-9
# inventoryCja$inventoryCsd<-area.dom*porosity.mean*Cth*tritiumCavg$sd*1e-9
# 
# #Compute the inventory and convert thickness from feet to meters, and pCi/mL into Ci/m^3
# inventoryja$inventory1<-area.dom*porosity.mean*inventoryja$h.mean*inventoryja$mean*.3048*1e-9
# inventoryja$inventory1b<-area.dom*porosity.mean*inventoryja$hb.mean*inventoryja$mean*.3048*1e-9
# inventoryja$inventory1lm<-area.dom*porosity.mean*inventoryja$hlm.mean*inventoryja$mean*.3048*1e-9
# inventoryja<-merge(inventoryja,inventoryCja,by="MYEAR")
# 
# # An error analysis on this one would be great.
# # Can just do a bootstrap to sample the joint distribution
# 
# #Remove unused data.frames
# # rm(thperyear)
# # rm(thperyear.cleanh)
# # rm(thperyear.cleanhb)
# # rm(thperyear.cleanhlm)
# # rm(th.avg.peryearh)
# # rm(th.avg.peryearhb)
# # rm(th.avg.peryearhlm)
# 
# # Save R object
# saveRDS(inventoryja, file = "inventoryja.rdata")
# 
# # Save as csv
# inventoryja.csv<-inventoryja[,c("MYEAR","inventory1","inventory1b","inventory1lm","inventoryC")]
# write.csv(inventoryja.csv, file="inventoryja.csv")

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

# tritiuml.loess<-llply(tritiuml2, function(zzl) {loess(mean~EASTING+NORTHING, data=zzl,degree=1,span=alphaloessconc)})
# tritium.pred<-llply(tritiuml.loess, function(m) {predict(m,newdata=testgrid1,se =TRUE)})

# inv5<-testgrid1
# inv5$TCCZfit<-as.vector(pre3$fit)
# inv5$TCCZfitb<-as.vector(pre3b$fit)
# inv5$TCCZfitlm<-as.vector(pre3lm$fit)
# inv5$TCCZsefit<-as.vector(pre3$se.fit)
# inv5$TCCZsefitb<-as.vector(pre3b$se.fit)
# inv5$TCCZsefitlm<-as.vector(pre3lm$se.fit)

# #Select 1993 and after no data in 1991 and 1992
# iodinel2<-iodinel[3:length(iodinel)]
# #Select after 1993
# strontiuml2<-strontiuml[4:length(strontiuml)]
# #After 1989
# cesium137l2<-cesium137l[5:length(cesium137l)]


Iinventory<-thicknessUAZ
Srinventory<-thicknessUAZ
thvn<-dim(thicknessUAZ)[2]
dimpredgrid<-dim(testgrid1)
nbnegIvals<-vector(mode = "integer", length = length(iodinel2));
nbNAIvals<-vector(mode = "integer", length = length(iodinel2));
nbnegSrvals<-vector(mode = "integer", length = length(strontiuml2));
nbNASrvals<-vector(mode = "integer", length = length(strontiuml2));


nbparamT1<-4

#lag to match the years, the aquifer thickness data.frame starts in 1988 
# when the usable iodine record starts in 1993
# the min year
minyearI<-as.numeric(names(iodinel2)[1])
indexlagI<-ov+(minyearI-1988)*4
#names(thicknessUAZ)[11]

for (kkI in 1:length(iodinel2)) {
  I.loess<-loess(mean~EASTING+NORTHING, data=iodinel2[[kkI]],degree=1,span=alphaloessconc)
  predI<-predict(I.loess,newdata = testgrid1 ,se = TRUE)
  #The prediction is given as a matrix, convert to vector
  dimpredictions<-dim(predI$fit)
  fullfit<-as.vector(predI$fit)
  #Remove negative values as we will just consider them NULL
  nbnegIvals[kkI]<-sum(fullfit<0, na.rm=TRUE)
  fullfit[fullfit<0]<-NA
  nbNAIvals[kkI]<-sum(is.na(fullfit))
  #fullfit[fullfit<0]<-0
  #Store in Tyear
  Iinventory[nbparamT1*(kkI-1)+thvn+1]<-fullfit
  names(Iinventory)[nbparamT1*(kkI-1)+thvn+1]<-paste0("I",names(iodinel2)[kkI])
  Iinventory[nbparamT1*(kkI-1)+thvn+2]<-as.vector(predI$se.fit)
  names(Iinventory)[nbparamT1*(kkI-1)+thvn+2]<-paste0("se.I",names(iodinel2)[kkI])
  # Compute C*e
  Iinventory[nbparamT1*(kkI-1)+thvn+3]<-Iinventory[nbparamT1*(kkI-1)+thvn+1]*Iinventory[nbparamUAZ*(kkI-1)+indexlagI+3]
  names(Iinventory)[nbparamT1*(kkI-1)+thvn+3]<-paste0("chI",names(iodinel2)[kkI])
  # And the standard error
  Iinventory[nbparamT1*(kkI-1)+thvn+4]<-Iinventory[nbparamT1*(kkI-1)+thvn+3] * sqrt((Iinventory[nbparamT1*(kkI-1)+thvn+2]/Iinventory[nbparamT1*(kkI-1)+thvn+1])^2+(Iinventory[nbparamUAZ*(kkI-1)+indexlagI+4]/Iinventory[nbparamUAZ*(kkI-1)+indexlagI+3])^2)
  names(Iinventory)[nbparamT1*(kkI-1)+thvn+4]<-paste0("se.chI",names(iodinel2)[kkI])
}

# Year alignment lag
minyearSr<-as.numeric(names(strontiuml2)[1])
indexlagSr<-ov+(minyearSr-1988)*4

## Strontium

for (kkSr in 1:length(strontiuml2)) {
  Sr.loess<-loess(mean~EASTING+NORTHING, data=strontiuml2[[kkSr]],degree=1,span=alphaloessconc)
  predSr<-predict(Sr.loess,newdata = testgrid1 ,se = TRUE)
  dimpredictions<-dim(predSr$fit)
  fullfit<-as.vector(predSr$fit)
  #Remove negative values as we will just consider them NULL
  nbnegSrvals[kkSr]<-sum(fullfit<0, na.rm=TRUE)
  fullfit[fullfit<0]<-NA
  nbNASrvals[kkSr]<-sum(is.na(fullfit))
  #fullfit[fullfit<0]<-0
  #Store in Tyear
  Srinventory[nbparamT1*(kkSr-1)+thvn+1]<-fullfit
  names(Srinventory)[nbparamT1*(kkSr-1)+thvn+1]<-paste0("Sr",names(strontiuml2)[kkSr])
  Srinventory[nbparamT1*(kkSr-1)+thvn+2]<-as.vector(predSr$se.fit)
  names(Srinventory)[nbparamT1*(kkSr-1)+thvn+2]<-paste0("se.Sr",names(strontiuml2)[kkSr])
  # Compute C*e
  Srinventory[nbparamT1*(kkSr-1)+thvn+3]<-Srinventory[nbparamT1*(kkSr-1)+thvn+1]*Srinventory[nbparamUAZ*(kkSr-1)+indexlagSr+3]
  names(Srinventory)[nbparamT1*(kkSr-1)+thvn+3]<-paste0("chSr",names(strontiuml2)[kkSr])
  # And the standard error
  Srinventory[nbparamT1*(kkSr-1)+thvn+4]<-Srinventory[nbparamT1*(kkSr-1)+thvn+3] * sqrt((Srinventory[nbparamT1*(kkSr-1)+thvn+2]/Srinventory[nbparamT1*(kkSr-1)+thvn+1])^2+(Srinventory[nbparamUAZ*(kkSr-1)+indexlagSr+4]/Srinventory[nbparamUAZ*(kkSr-1)+indexlagSr+3])^2)
  names(Srinventory)[nbparamT1*(kkSr-1)+thvn+4]<-paste0("se.chSr",names(strontiuml2)[kkSr])
}

## Cesium 137 

Csinventory<-thicknessUAZ
nbnegCsvals<-vector(mode = "integer", length = length(cesium137l2));
nbNACsvals<-vector(mode = "integer", length = length(cesium137l2));

# Year alignment lag
minyearCs<-as.numeric(names(cesium137l2)[1])
indexlagCs<-ov+(minyearSr-1988)*4


for (kkCs in 1:length(cesium137l2)) {
  Cs.loess<-loess(mean~EASTING+NORTHING, data=cesium137l2[[kkCs]],degree=1,span=alphaloessconc)
  predCs<-predict(Cs.loess,newdata = testgrid1 ,se = TRUE)
  #The prediction is given as a matrix, convert to vector
  dimpredictions<-dim(predCs$fit)
  fullfit<-as.vector(predCs$fit)
  #Remove negative values as we will just consider them NULL
  nbnegCsvals[kkCs]<-sum(fullfit<0, na.rm=TRUE)
  fullfit[fullfit<0]<-NA
  nbNACsvals[kkCs]<-sum(is.na(fullfit))
  #fullfit[fullfit<0]<-0
  #Store in Tyear
  Csinventory[nbparamT1*(kkCs-1)+thvn+1]<-fullfit
  names(Csinventory)[nbparamT1*(kkCs-1)+thvn+1]<-paste0("Cs",names(cesium137l2)[kkCs])
  Csinventory[nbparamT1*(kkCs-1)+thvn+2]<-as.vector(predCs$se.fit)
  names(Csinventory)[nbparamT1*(kkCs-1)+thvn+2]<-paste0("se.Cs",names(cesium137l2)[kkCs])
  # Compute C*e
  Csinventory[nbparamT1*(kkCs-1)+thvn+3]<-Csinventory[nbparamT1*(kkCs-1)+thvn+1]*Csinventory[nbparamUAZ*(kkCs-1)+indexlagCs+3]
  names(Csinventory)[nbparamT1*(kkCs-1)+thvn+3]<-paste0("chCs",names(cesium137l2)[kkCs])
  # And the standard error
  Csinventory[nbparamT1*(kkCs-1)+thvn+4]<-Csinventory[nbparamT1*(kkCs-1)+thvn+3] * sqrt((Csinventory[nbparamT1*(kkCs-1)+thvn+2]/Csinventory[nbparamT1*(kkCs-1)+thvn+1])^2+(Csinventory[nbparamUAZ*(kkCs-1)+indexlagCs+4]/Csinventory[nbparamUAZ*(kkCs-1)+indexlagCs+3])^2)
  names(Csinventory)[nbparamT1*(kkCs-1)+thvn+4]<-paste0("se.chCs",names(cesium137l2)[kkCs])
  print(names(cesium137l2)[kkCs])
}

# rstudio::viewData(Csinventory[,names(Csinventory)[grep("Cs",names(Csinventory))]])

#image.plot(ea.v,no.v,Csinventory$Cs1990)
testCsplot<-Csinventory$Cs1991
dim(testCsplot) <- c(50,60)
testCsplot2<-t(testCsplot)
image(ea.v,no.v,testCsplot2)
image(no.v,ea.v,testCsplot2)


# nbnegtritiumCvals<-vector(mode = "integer", length = length(tritiumCl2))
# nbNAtritiumCvals<-vector(mode = "integer", length = length(tritiumCl2))
# thCvn<-dim(thicknessLAZ)[2]
#  nbparamT1C<-6
# 
# TinventoryC<-thicknessLAZ
# for (kk2 in 1:length(tritiumCl2)) {
#   tC.loess<-loess(mean~EASTING+NORTHING, data=tritiumCl2[[kk2]],degree=1,span=alphaloessconc)
#   logtC.loess<-loess(logmean~EASTING+NORTHING, data=tritiumCl2[[kk2]],degree=1,span=alphaloessconc)
#   predtC<-predict(tC.loess,newdata = testgrid1 ,se = TRUE)
#   predlogtC<-predict(logtC.loess,newdata = testgrid1 ,se = TRUE)
#   fullfit<-as.vector(predtC$fit)
#   nbnegtritiumCvals[kk2]<-sum(fullfit<0, na.rm=TRUE)
#   fullfit[fullfit<0]<-NA
#   nbNAtritiumCvals[kk2]<-sum(is.na(fullfit));
#   #fullfit[fullfit<0]<-1
#   #fullfit[fullfit<0]<-0
#   TinventoryC[nbparamT1C*(kk2-1)+thCvn+1]<-fullfit
#   names(TinventoryC)[nbparamT1C*(kk2-1)+thCvn+1]<-paste0("TC",names(tritiumCl2)[kk2])
#   logfullfit<-as.vector(predlogtC$fit)
#   TCfl<-exp(logfullfit)
#   TinventoryC[nbparamT1C*(kk2-1)+thCvn+2]<-TCfl
#   names(TinventoryC)[nbparamT1C*(kk2-1)+thCvn+2]<-paste0("TCfl",names(tritiumCl2)[kk2])
#   TinventoryC[nbparamT1*(kk2-1)+thCvn+3]<-as.vector(predtC$se.fit)
#   names(TinventoryC)[nbparamT1*(kk2-1)+thCvn+3]<-paste0("se.TC",names(tritiumCl2)[kk2])
#   TinventoryC[nbparamT1C*(kk2-1)+thCvn+4]<-TinventoryC[nbparamT1C*(kk2-1)+thCvn+1]*TinventoryC[thCvn]
#   names(TinventoryC)[nbparamT1C*(kk2-1)+thCvn+4]<-paste0("ch",names(tritiumCl2)[kk2])
#   TinventoryC[nbparamT1C*(kk2-1)+thCvn+5]<-TinventoryC[nbparamT1*(kk2-1)+thCvn+3]*TinventoryC[thCvn]
#   names(TinventoryC)[nbparamT1C*(kk2-1)+thCvn+5]<-paste0("se.ch",names(tritiumCl2)[kk2])
#   TinventoryC[nbparamT1C*(kk2-1)+thCvn+6]<-TinventoryC[nbparamT1C*(kk2-1)+thCvn+2]*TinventoryC[thCvn]
#   names(TinventoryC)[nbparamT1C*(kk2-1)+thCvn+6]<-paste0("chfl",names(tritiumCl2)[kk2])
# }


# tritium.regression.diagnostics<-as.data.frame(cbind(nbnegtritiumvals,nbNAtritiumvals))
# tritiumC.regression.diagnostics<-as.data.frame(cbind(nbnegtritiumCvals,nbNAtritiumCvals))
I.regression.diagnostics<-as.data.frame(cbind(nbnegIvals,nbNAIvals))
Sr.regression.diagnostics<-as.data.frame(cbind(nbnegSrvals,nbNASrvals))
Cs.regression.diagnostics<-as.data.frame(cbind(nbnegCsvals,nbNACsvals))

# saveRDS(Tinventory,"Tinventory.rdata")
saveRDS(Iinventory,"Iinventory.rdata")
saveRDS(Srinventory,"Srinventory.rdata")
saveRDS(Csinventory,"Csinventory.rdata")

saveRDS(I.regression.diagnostics, file = "I.regression.diagnostics.rdata")
saveRDS(Sr.regression.diagnostics, file = "Sr.regression.diagnostics.rdata")
saveRDS(Cs.regression.diagnostics, file = "Cs.regression.diagnostics.rdata")


#Now for the averaging

Iinventory5<-data.frame(MYEAR=seq(1993,2011,length=19))
for (jj2 in 1:length(Iinventory5$MYEAR)) {
  # This computes the mean of the values, removing the NA
  Iinventory5$mean.ch[jj2]<-mean(Iinventory[[nbparamT1*(jj2-1)+thvn+3]], na.rm=TRUE)
  Iinventory5$median.ch[jj2]<-median(Iinventory[[nbparamT1*(jj2-1)+thvn+3]], na.rm=TRUE)
  Iinventory5$sd.ch[jj2]<-sd(Iinventory[[nbparamT1*(jj2-1)+thvn+3]], na.rm=TRUE)
  # This compute the discrete integral for the amount of 
  Iinventory5$dint.ch[jj2]<-sum(weightarea*Iinventory[[nbparamT1*(jj2-1)+thvn+3]], na.rm=TRUE)
  Iinventory5$se.ch[jj2]<-weightarea*sqrt(sum(Iinventory[nbparamT1*(jj2-1)+thvn+4]^2, na.rm=TRUE))
}
Iinventory5$I.mean<-area.dom*porosity.mean*Iinventory5$mean.ch*1e-9*.3048
Iinventory5$I.med<-area.dom*porosity.mean*Iinventory5$median.ch*1e-9*.3048
Iinventory5$I.sd<-area.dom*porosity.mean*Iinventory5$sd.ch*1e-9*.3048
Iinventory5$I.dint<-area.dom*porosity.mean*Iinventory5$dint.ch*1e-9*.3048
Iinventory5$I.se<-area.dom*porosity.mean*Iinventory5$se.ch*1e-9*.3048

Srinventory5<-data.frame(MYEAR=seq(1993,2011,length=19))
for (jj2 in 1:length(Srinventory5$MYEAR)) {
  # This computes the mean of the values, removing the NA
  Srinventory5$mean.ch[jj2]<-mean(Srinventory[[nbparamT1*(jj2-1)+thvn+3]], na.rm=TRUE)
  Srinventory5$median.ch[jj2]<-median(Srinventory[[nbparamT1*(jj2-1)+thvn+3]], na.rm=TRUE)
  Srinventory5$sd.ch[jj2]<-sd(Srinventory[[nbparamT1*(jj2-1)+thvn+3]], na.rm=TRUE)
  # This compute the discrete integral for the amount of 
  Srinventory5$dint.ch[jj2]<-sum(weightarea*Srinventory[[nbparamT1*(jj2-1)+thvn+3]], na.rm=TRUE)
  Srinventory5$se.ch[jj2]<-weightarea*sqrt(sum(Srinventory[[nbparamT1*(jj2-1)+thvn+4]]^2, na.rm=TRUE))
}
Srinventory5$Sr.mean<-area.dom*porosity.mean*Srinventory5$mean.ch*1e-9*.3048
Srinventory5$Sr.med<-area.dom*porosity.mean*Srinventory5$median.ch*1e-9*.3048
Srinventory5$Sr.sd<-area.dom*porosity.mean*Srinventory5$sd.ch*1e-9*.3048
Srinventory5$Sr.dint<-area.dom*porosity.mean*Srinventory5$dint.ch*1e-9*.3048
Srinventory5$Sr.se<-area.dom*porosity.mean*Srinventory5$se.ch*1e-9*.3048

# saveRDS(inventory5,"inventoryloesswithuncertaintyfinal.rdata")

saveRDS(Iinventory5,"Iinventoryloesswithuncertaintyfinal.rdata")

saveRDS(Srinventory5,"Srinventoryloesswithuncertaintyfinal.rdata")

saveRDS(Csinventory5,"Csinventoryloesswithuncertaintyfinal.rdata")

saveRDS(Iinventory5,"Iinventory.final.rdata")
saveRDS(Srinventory5,"Srinventory.final.rdata")

# Tinventory.final<-merge(inventoryja.csv, inventory5, by="MYEAR")

# saveRDS(Tinventory.final,"Tinventoryfinal.rdata")

# rm(TCfl)
# rm(Tfl)
# rm(fullfit)
# rm(logfullfit)
# rm(logt.loess)
# rm(logtC.loess)
# rm(t.loess)
# rm(tC.loess)
# rm(predlogt)
# rm(predlogtC)
# rm(predt)
# rm(predtC)



