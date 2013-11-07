#Import source term data

sourceterms<-read.csv('../source_term/FareasourcetermforR.csv')
surfacewater<-read.csv('../source_term/surfacewatermonitoring.csv')

sourceterms2<-data.frame(Year=seq(1955,2011,length=2011-1955+1))
for (jjj in 1:length(sourceterms$Strontium90.Ci)) {
  sourceterms2$Strontium90.Ci[jjj]<-sourceterms$Strontium90.Ci[jjj]
}
sourceterms2$Strontium90.Ci[35:length(sourceterms2$Year)]<-0
sourceterms2$Srmig.Ci<-surfacewater$Sr.90.sw[2:length(surfacewater$Sr.90.sw)]
sourceterms2$Srmig.Ci[is.na(sourceterms2$Srmig.Ci)]<-0


sourceterms2$cdcSr90<-cumsumdecaycorrected(sourceterms2$Strontium90.Ci,28.6)
sourceterms2$cdcSrmig<-cumsumdecaycorrected(sourceterms2$Srmig.Ci,28.6)
sourceterms2$gwISr90<-sourceterms2$cdcSr90-sourceterms2$cdcSrmig

saveRDS(sourceterms,file='../source_term/sourceterms.rdata')
saveRDS(sourceterms2,file='../source_term/Srsourceterm.rdata')