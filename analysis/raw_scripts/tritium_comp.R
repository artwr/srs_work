#Simple Computation for Tritium
# require(geoR)
# require(geoRglm)
# require(fields)
# require(akima)
# require(splancs)
# require(plyr)
# require(ggplot2)
# require(scales)

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
#wells within the polygon defined by
# 'POLYGON((-81.69959850412471 33.25157883545753,
# -81.6695146228905 33.27042851958022, 
# -81.67053367567048 33.27832883035499,
# -81.69388086898707 33.28329777530127,
# -81.69959850412471 33.25157883545753))'
#
tritium<-readRDS("../../srs_data/processed/tritium.rdata")
tritiumC<-readRDS("../../srs_data/processed/tritiumC.rdata")
tritiumavg<-readRDS("../../srs_data/processed/tritiumavg.rdata")
tritiumCavg<-readRDS("../../srs_data/processed/tritiumCavg.rdata")

#Add log transform
tritium$logmean<-log(tritium$mean)
tritium$log10mean<-log10(tritium$mean)
tritiumC$logmean<-log(tritiumC$mean)
tritiumC$log10mean<-log10(tritiumC$mean)

#Split per measurement year
tritiuml<-split(tritium,tritium$MYEAR)
tritiumCl<-split(tritiumC,tritiumC$MYEAR)
#Select 1988 and after
tritiuml2<-tritiuml[10:length(tritiuml)]
tritiumCl2<-tritiumCl[3:length(tritiumCl)]
# names(tritiuml2)


#########################################################
#2. Do simple average calculations on the well positions only
# \int_D C h p dx dy = \int_D dx dy * \hat{C} * \hat{h} * porosity p
#Thickness is computed using a linear prediction
#Works ok because the sampling wells do "importance sampling"
#on the aquifer

#Create inventory df
inventoryja<-merge(tritiumavg[tritiumavg$MYEAR>=1984,],th.avg.peryearh,by="MYEAR")
inventoryja<-merge(inventoryja,th.avg.peryearhb,by="MYEAR")
inventoryja<-merge(inventoryja,th.avg.peryearhlm,by="MYEAR")

inventoryCja<-data.frame(MYEAR=tritiumCavg[,c("MYEAR")])
inventoryCja$inventoryC<-area.dom*porosity.mean*Cth*tritiumCavg$mean*1e-9
inventoryCja$inventoryCmedian<-area.dom*porosity.mean*Cth*tritiumCavg$median*1e-9
inventoryCja$inventoryCsd<-area.dom*porosity.mean*Cth*tritiumCavg$sd*1e-9

#Compute the inventory and convert thickness from feet to meters, and pCi/mL into Ci/m^3
inventoryja$inventory1<-area.dom*porosity.mean*inventoryja$h.mean*inventoryja$mean*.3048*1e-9
inventoryja$inventory1b<-area.dom*porosity.mean*inventoryja$hb.mean*inventoryja$mean*.3048*1e-9
inventoryja$inventory1lm<-area.dom*porosity.mean*inventoryja$hlm.mean*inventoryja$mean*.3048*1e-9
inventoryja<-merge(inventoryja,inventoryCja,by="MYEAR")

# An error analysis on this one would be great.
# Can just do a bootstrap to sample the joint distribution

#Remove unused data.frames
# rm(thperyear)
# rm(thperyear.cleanh)
# rm(thperyear.cleanhb)
# rm(thperyear.cleanhlm)
# rm(th.avg.peryearh)
# rm(th.avg.peryearhb)
# rm(th.avg.peryearhlm)

# Save R object
saveRDS(inventoryja, file = "./data/inventoryja.rdata")

# Save as csv
inventoryja.csv<-inventoryja[,c("MYEAR","inventory1","inventory1b","inventory1lm","inventoryC")]
write.csv(inventoryja.csv, file="./data/inventoryja.csv")

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
# tritium.pred<-llply(tritiuml.loess, function(m) {predict(m,newdata=interpolation.grid,se =TRUE)})

# inv5<-interpolation.grid
# inv5$TCCZfit<-as.vector(pre3$fit)
# inv5$TCCZfitb<-as.vector(pre3b$fit)
# inv5$TCCZfitlm<-as.vector(pre3lm$fit)
# inv5$TCCZsefit<-as.vector(pre3$se.fit)
# inv5$TCCZsefitb<-as.vector(pre3b$se.fit)
# inv5$TCCZsefitlm<-as.vector(pre3lm$se.fit)

Tinventory<-thicknessUAZ
thvn<-dim(thicknessUAZ)[2]
dimpredgrid<-dim(interpolation.grid)
nbnegtritiumvals<-vector(mode = "integer", length = length(tritiuml2));
nbNAtritiumvals<-vector(mode = "integer", length = length(tritiuml2));

nbparamT1<-6

for (kk in 1:length(tritiuml2)) {
  t.loess<-loess(mean~EASTING+NORTHING, data=tritiuml2[[kk]],degree=1,span=alphaloessconc)
  logt.loess<-loess(logmean~EASTING+NORTHING, data=tritiuml2[[kk]],degree=1,span=alphaloessconc)
  predt<-predict(t.loess,newdata = interpolation.grid ,se = TRUE)
  predlogt<-predict(logt.loess,newdata = interpolation.grid ,se = TRUE)
  #The prediction is given as a matrix, convert to vector
  dimpredictions<-dim(predt$fit)
  fullfit<-as.vector(predt$fit)
  #Remove negative values as we will just consider them NULL
  nbnegtritiumvals[kk]<-sum(fullfit<0, na.rm=TRUE)
  fullfit[fullfit<0]<-NA
  nbNAtritiumvals[kk]<-sum(is.na(fullfit))
  #fullfit[fullfit<0]<-0
  #Store in Tyear
  Tinventory[nbparamT1*(kk-1)+thvn+1]<-fullfit
  names(Tinventory)[nbparamT1*(kk-1)+thvn+1]<-paste0("T",names(tritiuml2)[kk])
  Tinventory[nbparamT1*(kk-1)+thvn+2]<-as.vector(predt$se.fit)
  names(Tinventory)[nbparamT1*(kk-1)+thvn+2]<-paste0("se.T",names(tritiuml2)[kk])
  #logreg
  logfullfit<-as.vector(predlogt$fit)
  Tfl<-exp(logfullfit)
  Tinventory[nbparamT1*(kk-1)+thvn+3]<-Tfl
  names(Tinventory)[nbparamT1*(kk-1)+thvn+3]<-paste0("Tfl",names(tritiuml2)[kk])
  # Compute C*e
  Tinventory[nbparamT1*(kk-1)+thvn+4]<-Tinventory[nbparamT1*(kk-1)+thvn+1]*Tinventory[nbparamUAZ*(kk-1)+ov+3]
  names(Tinventory)[nbparamT1*(kk-1)+thvn+4]<-paste0("ch",names(tritiuml2)[kk])
  # And the standard error
  Tinventory[nbparamT1*(kk-1)+thvn+5]<-Tinventory[nbparamT1*(kk-1)+thvn+4] * sqrt((Tinventory[nbparamT1*(kk-1)+thvn+2]/Tinventory[nbparamT1*(kk-1)+thvn+1])^2+(Tinventory[nbparamUAZ*(kk-1)+ov+3]/Tinventory[nbparamUAZ*(kk-1)+ov+4])^2)
  names(Tinventory)[nbparamT1*(kk-1)+thvn+5]<-paste0("se.ch",names(tritiuml2)[kk])
  # The the log
  Tinventory[nbparamT1*(kk-1)+thvn+6]<-Tinventory[nbparamT1*(kk-1)+thvn+3]*Tinventory[nbparamUAZ*(kk-1)+ov+3]
  names(Tinventory)[nbparamT1*(kk-1)+thvn+6]<-paste0("chfl",names(tritiuml2)[kk])
}

nbnegtritiumCvals<-vector(mode = "integer", length = length(tritiumCl2))
nbNAtritiumCvals<-vector(mode = "integer", length = length(tritiumCl2))
thCvn<-dim(thicknessLAZ)[2]
 nbparamT1C<-6

TinventoryC<-thicknessLAZ
for (kk2 in 1:length(tritiumCl2)) {
  tC.loess<-loess(mean~EASTING+NORTHING, data=tritiumCl2[[kk2]],degree=1,span=alphaloessconc)
  logtC.loess<-loess(logmean~EASTING+NORTHING, data=tritiumCl2[[kk2]],degree=1,span=alphaloessconc)
  predtC<-predict(tC.loess,newdata = interpolation.grid ,se = TRUE)
  predlogtC<-predict(logtC.loess,newdata = interpolation.grid ,se = TRUE)
  fullfit<-as.vector(predtC$fit)
  nbnegtritiumCvals[kk2]<-sum(fullfit<0, na.rm=TRUE)
  fullfit[fullfit<0]<-NA
  nbNAtritiumCvals[kk2]<-sum(is.na(fullfit));
  #fullfit[fullfit<0]<-1
  #fullfit[fullfit<0]<-0
  TinventoryC[nbparamT1C*(kk2-1)+thCvn+1]<-fullfit
  names(TinventoryC)[nbparamT1C*(kk2-1)+thCvn+1]<-paste0("TC",names(tritiumCl2)[kk2])
  logfullfit<-as.vector(predlogtC$fit)
  TCfl<-exp(logfullfit)
  TinventoryC[nbparamT1C*(kk2-1)+thCvn+2]<-TCfl
  names(TinventoryC)[nbparamT1C*(kk2-1)+thCvn+2]<-paste0("TCfl",names(tritiumCl2)[kk2])
  TinventoryC[nbparamT1*(kk2-1)+thCvn+3]<-as.vector(predtC$se.fit)
  names(TinventoryC)[nbparamT1*(kk2-1)+thCvn+3]<-paste0("se.TC",names(tritiumCl2)[kk2])
  TinventoryC[nbparamT1C*(kk2-1)+thCvn+4]<-TinventoryC[nbparamT1C*(kk2-1)+thCvn+1]*TinventoryC[thCvn]
  names(TinventoryC)[nbparamT1C*(kk2-1)+thCvn+4]<-paste0("ch",names(tritiumCl2)[kk2])
  TinventoryC[nbparamT1C*(kk2-1)+thCvn+5]<-TinventoryC[nbparamT1*(kk2-1)+thCvn+3]*TinventoryC[thCvn]
  names(TinventoryC)[nbparamT1C*(kk2-1)+thCvn+5]<-paste0("se.ch",names(tritiumCl2)[kk2])
  TinventoryC[nbparamT1C*(kk2-1)+thCvn+6]<-TinventoryC[nbparamT1C*(kk2-1)+thCvn+2]*TinventoryC[thCvn]
  names(TinventoryC)[nbparamT1C*(kk2-1)+thCvn+6]<-paste0("chfl",names(tritiumCl2)[kk2])
}


tritium.regression.diagnostics<-as.data.frame(cbind(nbnegtritiumvals,nbNAtritiumvals))
tritiumC.regression.diagnostics<-as.data.frame(cbind(nbnegtritiumCvals,nbNAtritiumCvals))

saveRDS(Tinventory,"./data/Tinventory.rdata")
saveRDS(TinventoryC,"./data/TinventoryC.rdata")
saveRDS(tritium.regression.diagnostics, file = "./diagnostics/tritium.regression.diagnostics.rdata")
saveRDS(tritiumC.regression.diagnostics, file = "./diagnostics/tritiumC.regression.diagnostics.rdata")



#Now for the averaging

inventory5<-data.frame(MYEAR=seq(1988,2011,length=24))
for (jj2 in 1:length(inventory5$MYEAR)) {
  # This computes the mean of the values, removing the NA
  inventory5$mean.ch[jj2]<-mean(Tinventory[[nbparamT1*(jj2-1)+thvn+4]], na.rm=TRUE)
  inventory5$median.ch[jj2]<-median(Tinventory[[nbparamT1*(jj2-1)+thvn+4]], na.rm=TRUE)
  inventory5$sd.ch[jj2]<-sd(Tinventory[[nbparamT1*(jj2-1)+thvn+4]], na.rm=TRUE)
  # This compute the discrete integral for the amount of tritium
  inventory5$dint.ch[jj2]<-sum(weightarea*Tinventory[[nbparamT1*(jj2-1)+thvn+4]], na.rm=TRUE)
  inventory5$se.ch[jj2]<-weightarea*sqrt(sum(Tinventory[[nbparamT1*(jj2-1)+thvn+5]]^2, na.rm=TRUE))
  inventory5$dint.chfl[jj2]<-sum(weightarea*Tinventory[[nbparamT1*(jj2-1)+thvn+6]], na.rm=TRUE)
  inventory5$mean.chfl[jj2]<-mean(Tinventory[[nbparamT1*(jj2-1)+thvn+6]], na.rm=TRUE)
  inventory5$median.chfl[jj2]<-median(Tinventory[[nbparamT1*(jj2-1)+thvn+6]], na.rm=TRUE)
  inventory5$sd.chfl[jj2]<-sd(Tinventory[[nbparamT1*(jj2-1)+thvn+6]], na.rm=TRUE)
  inventory5$mean.chC[jj2]<-mean(TinventoryC[[nbparamT1C*(jj2-1)+5]], na.rm=TRUE)
  inventory5$dint.chC[jj2]<-sum(weightarea*TinventoryC[[nbparamT1C*(jj2-1)+5]], na.rm=TRUE)
  inventory5$se.chC[jj2]<-sum(weightarea*TinventoryC[nbparamT1C*(kk2-1)+thCvn+5], na.rm=TRUE)
}
inventory5$t.mean<-area.dom*porosity.mean*inventory5$mean.ch*1e-9*.3048
inventory5$t.med<-area.dom*porosity.mean*inventory5$median.ch*1e-9*.3048
inventory5$t.sd<-area.dom*porosity.mean*inventory5$sd.ch*1e-9*.3048
inventory5$t.dint<-area.dom*porosity.mean*inventory5$dint.ch*1e-9*.3048
inventory5$t.se<-area.dom*porosity.mean*inventory5$se.ch*1e-9*.3048
inventory5$t.meanfl<-area.dom*porosity.mean*inventory5$mean.chfl*1e-9*.3048
inventory5$t.medfl<-area.dom*porosity.mean*inventory5$median.chfl*1e-9*.3048
inventory5$t.dintfl<-area.dom*porosity.mean*inventory5$dint.chfl*1e-9*.3048
inventory5$t.sd<-area.dom*porosity.mean*inventory5$sd.ch*1e-9*.3048
inventory5$tC.mean<-area.dom*porosity.mean*inventory5$mean.chC*1e-9
inventory5$tC.dint<-area.dom*porosity.mean*inventory5$dint.chC*1e-9
inventory5$tC.se<-area.dom*porosity.mean*inventory5$se.chC*1e-9
inventory5$tCD.mean<-inventory5$t.mean+inventory5$tC.mean
inventory5$tCD.sd<-inventory5$t.sd+inventory5$tC.se
inventory5$tCD.dint<-inventory5$t.dint+inventory5$tC.dint
inventory5$tCD.se<-inventory5$t.se+inventory5$tC.se

saveRDS(inventory5,"./data/inventoryloesswithuncertaintyfinal.rdata")

Tinventory.final<-merge(inventoryja.csv, inventory5, by="MYEAR")

saveRDS(Tinventory.final,"./data/Tinventoryfinal.rdata")

rm(TCfl)
rm(Tfl)
rm(fullfit)
rm(logfullfit)
rm(logt.loess)
rm(logtC.loess)
rm(t.loess)
rm(tC.loess)
rm(predlogt)
rm(predlogtC)
rm(predt)
rm(predtC)



