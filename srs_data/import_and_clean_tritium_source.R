#Source term and Surface water data

require(ggplot2)
require(scales)
require(reshape2)

tritiumsource.raw<-read.csv("./srs_data/raw/tritiumsource.csv")
tritiumsource.raw<-tritiumsource.raw[!is.na(tritiumsource.raw$Year),]
# tritiumsource.raw2<-read.csv("tritiumsource.csv")
names(tritiumsource.raw)<-c("Year", "VolumeinL","H3input", "CumulH3input",
                            "NetH3afterevap", "CumulH3afterevap", "CumulH3afterevapdecayc", 
                            "HMigration", "HMigrationdecayc", "FMigration", 
                            "CumulH3frommigrationfromF", "CumulH3frommigrationfromFdecayc", 
                            "H3inventorydecayc", "Fmigrationdecayc",
                            "PossibleevolutionF", "Estimateinterp", "Lowerb","Upperb")

#bbb<-cumsumdecaycorrected(tritiumsource.raw$NetH3afterevap,12.3)

tritiumsource<-tritiumsource.raw[,c("Year", "CumulH3afterevapdecayc","CumulH3frommigrationfromFdecayc", "H3inventorydecayc")]
# tritiumsource$sourceterm2<-cumsumdecaycorrected(tritiumsource.raw$NetH3afterevap,12.3)
# names(tritiumsource)<-c("Year", "sourceterm","TritiuminFMB", "gwInventory","sourceterm2")
names(tritiumsource)<-c("Year", "sourceterm","TritiuminFMB", "gwInventory")

tritiumsource2<-tritiumsource
names(tritiumsource2)<-c("Year", "Input","Outflow", "Difference")


tritiumsource.csv<-tritiumsource
saveRDS(tritiumsource.csv,file="./srs_data/processed/tritiumsource.rdata")

# tritiumsource.long<-melt(tritiumsource, id.vars=c("Year"), 
#                          measure.vars=c("sourceterm","TritiuminFMB", "gwInventory","sourceterm2"),
#                          variable.name="Legend", value.name='value')
tritiumsource.long<-melt(tritiumsource, id.vars=c("Year"), 
                         measure.vars=c("sourceterm","TritiuminFMB", "gwInventory"),
                         variable.name="Legend", value.name='value')
tritiumsource2.long<-melt(tritiumsource2, id.vars=c("Year"), 
                         measure.vars=c("Input","Outflow", "Difference"),
                         variable.name="Legend", value.name='value')
tritiumsource.long$col1[tritiumsource.long$Legend=='sourceterm']<-"brown"
tritiumsource.long$col1[tritiumsource.long$Legend=='TritiuminFMB']<-"blue"
tritiumsource.long$col1[tritiumsource.long$Legend=='gwInventory']<-"orange"
# tritiumsource.long$col1[tritiumsource.long$Legend=='sourceterm2']<-"red"


tritiumsource2.long$col1[tritiumsource.long$Legend=='Input']<-"brown"
tritiumsource2.long$col1[tritiumsource.long$Legend=='Outflow']<-"blue"
tritiumsource2.long$col1[tritiumsource.long$Legend=='Difference']<-"orange"

# tritiumsource$cumulinputH3<-cumsum(tritiumsource$H3input))
# tritiumsource$cumulinputH3afterevap<-cumsum(tritiumsource$NetH3afterevap))
# rightcolours<-c("brown","blue","orange")
saveRDS(tritiumsource.long,file="./srs_data/processed/tritiumsourcelongformat.rdata")
saveRDS(tritiumsource2.long,file="./srs_data/processed/tritiumsource2longformat.rdata")





swgg3<-ggplot(data=tritiumsource.long,aes(x=Year, y=value, group=Legend, colour=col1)) + theme_bw()
swgg3<- swgg3 + geom_line(size=2) 
swgg3<- swgg3 + scale_colour_identity(guide = "legend")
print(swgg3)


swgg5 <- swgg3
swgg5 <- swgg5 +coord_cartesian(xlim = c(1954,1968), ylim = c(-1,80000))
print(swgg5)

swgg4<-ggplot(data=tritiumsource.long,aes(x=Year, y=value, group=Legend, colour=Legend)) + theme_bw()
swgg4<- swgg4 + geom_line(size=2) 
# swgg4<- swgg4 + scale_y_log10()
# swgg4<- swgg4 + scale_colour_identity(guide = "legend")
print(swgg4)
# 
# swgg3<- swgg3 + opts(panel.grid.major = none, panel.grid.minor = none)
# swgg3<- swgg3 + opts(panel.background = none, panel.border = none)
# swgg2<- swgg2 +geom_line(aes(y=CumulH3afterevapdecayc), size = 2, colour='brown')
# swgg2<- swgg2 +geom_line(aes(y=CumulH3frommigrationfromFdecayc), size = 2, colour='blue')
# swgg2<- swgg2 +geom_line(aes(y=H3inventorydecayc), size = 2, colour='orange')
# swgg2<- swgg2 +geom_point(aes(y=t), colour='orange')
# swgg2<- swgg2 +geom_point(aes(y=tfl), colour='black')
# swgg2<- swgg2 + scale_y_log10()
swgg2<-swgg2+labs(title="Tritium Inventory")+xlab("Year")+ylab("Tritium (Ci)")
print(swgg2)








swgg<-ggplot(data=tritiumsource.raw,aes(x=Year))
swgg<- swgg +geom_point(aes(y=CumulH3afterevapdecayc), colour='brown')
swgg<- swgg +geom_point(aes(y=CumulH3frommigrationfromFdecayc), colour='blue')
swgg<- swgg +geom_point(aes(y=H3inventorydecayc), colour='orange')
# swgg<- swgg +geom_point(aes(y=t), colour='orange')
# swgg<- swgg +geom_point(aes(y=tfl), colour='black')
# swgg<- swgg + scale_y_log10()
swgg<-swgg+labs(title="Tritium Inventory")+xlab("Year")+ylab("Tritium (Ci)")
print(swgg)

none<-theme_blank()

# png()
swgg2<-ggplot(data=tritiumsource.raw,aes(x=Year)) + theme_bw()
swgg2<- swgg2 + opts(panel.grid.major = none, panel.grid.minor = none)
swgg2<- swgg2 + opts(panel.background = none, panel.border = none)
swgg2<- swgg2 +geom_line(aes(y=CumulH3afterevapdecayc), size = 2, colour='brown')
swgg2<- swgg2 +geom_line(aes(y=CumulH3frommigrationfromFdecayc), size = 2, colour='blue')
swgg2<- swgg2 +geom_line(aes(y=H3inventorydecayc), size = 2, colour='orange')
# swgg2<- swgg2 +geom_point(aes(y=t), colour='orange')
# swgg2<- swgg2 +geom_point(aes(y=tfl), colour='black')
# swgg2<- swgg2 + scale_y_log10()
swgg2<-swgg2+labs(title="Tritium Inventory")+xlab("Year")+ylab("Tritium (Ci)")
print(swgg2)
