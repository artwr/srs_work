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

#Sample statistics

#qplot(log(mean), data=tritium)

pdf(file="histograms_logtritium.pdf",paper="letter")
for (ii in 1:length(tritiuml)) {
  ggp<-ggplot(tritiuml[[ii]],aes(x=log(mean))) + geom_histogram(binwidth=.2) + labs(title = as.character(1978+ii))
  print(ggp)
}
dev.off()
