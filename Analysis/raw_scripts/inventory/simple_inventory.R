#Simple Computation

#load data
tritium<-readRDS("tritium.rdata")
wl<-readRDS("wl.rdata")
TCCZe<-readRDS("TCCZ_o.rdata")
#



#summary(tritium[tritium$STATION_ID=='FSB115D',])
TCCZ.lm<-lm(TCCZ_top~UTM_E+UTM_N,data=TCCZe,na.action=na.omit)
TCCZ.loess = loess(TCCZ_top~UTM_E+UTM_N, data = TCCZe, degree = 2, span = 0.25)
plot(TCCZ.lm)

#Define n zones

#Compute trend planes in these zones for waterlevels and TCCZ

#Do the difference to get the thickness

