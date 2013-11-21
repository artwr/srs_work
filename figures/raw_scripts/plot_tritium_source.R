#plot Tritium Source
require(ggplot2)
require(scales)

sourceterm <- readRDS("../source_term/tritiumsource.rdata")
tritiumsource.long2 <- readRDS("../source_term/tritiumsource2longformat.rdata")


swgg3<-ggplot(data=tritiumsource.long,aes(x=Year, y=value, group=Legend, colour=col1)) + theme_bw()
swgg3<- swgg3 + geom_line(aes(linetype=Legend),size=2) 
swgg3<- swgg3 + scale_colour_identity(guide = guide_legend(title = "Terms of the mass balance"))
swgg3 <- swgg3 + scale_linetype_manual(breaks=c("Input","Outflow", "Difference"), values=c(1,2,4))
print(swgg3)


# swgg5 <- swgg3
# swgg5 <- swgg5 +coord_cartesian(xlim = c(1954,1968), ylim = c(-1,80000))
# print(swgg5)

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