#
require(ggplot2)
nitrateFAW5<-read.csv("./srs_data/raw/nitrate_FAW5.csv",header = TRUE)
names(nitrateFAW5)<-c("nitratemgl","delta.elev","depth.from.wt")
saveRDS(nitrateFAW5, file = "./srs_data/processed/nitrateFAW5.rdata")