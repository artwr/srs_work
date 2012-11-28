#Source term and Surface water data
tritiumsource.raw<-read.csv("tritiumsource.csv")
names(tritiumsource.raw)<-c("Year", "VolumeinL","H3input","CumulH3input", "NetH3afterevap", "CumulH3afterevap", "CumulH3afterevapdecayc", "HMigration", "HMigrationdecayc", "FMigration", "CumulH3frommigrationfromF", "CumulH3frommigrationfromFdecayc", "H3inventorydecayc", "Fmigrationdecayc","PossibleevolutionF", "Estimateinterp", "Lowerb","Upperb")

tritiumsource<-tritiumsource.raw[,c("Year", "VolumeinL","H3input", "NetH3afterevap", "CumulH3afterevapdecayc", "HMigration", "HMigrationdecayc", "FMigration", "CumulH3frommigrationfromF", "CumulH3frommigrationfromFdecayc", "H3inventorydecayc", "Fmigrationdecayc","PossibleevolutionF", "Estimateinterp")]
tritiumsource$cumulinputH3<-cumsum(tritiumsource$H3input))
tritiumsource$cumulinputH3afterevap<-cumsum(tritiumsource$NetH3afterevap))