require(ggplot2)
require(scales)


basins27<-readRDS("../basin_coords/basins27.rdata")
names(basins27)<-c('EASTING','NORTHING','id')

## Necessary trick to be able to identofy each of the basins
bnames <- data.frame(
  id = c("F3","F2","F1"),
  value = c(4,5,6)
)

basins27$id<-rep(bnames$id, each = 5)
basins27poly <- merge(basins27, bnames, by=c("id"))


# iodine<-readRDS("../SRS_data/iodine.rdata")
strontium<-readRDS("../SRS_data/strontium.rdata")
# cesium137<-readRDS("../SRS_data/cesium137.rdata")
# technetium<-readRDS("../SRS_data/technetium.rdata")

strontiumw<-unique(strontium[,c('MYEAR','EASTING','NORTHING')])
# strontiumw2<-unique(strontium[,c('STATION_ID','MYEAR','EASTING','NORTHING')])
strontiumw2<-unique(strontium[,c('STATION_ID','EASTING','NORTHING')])
strontiumw3<-strontiumw2[order(strontiumw2$STATION_ID),]


# iodinel<-split(iodine,iodine$MYEAR)
strontiumwl<-split(strontiumw,strontiumw$MYEAR)
# cesium137l<-split(cesium137,cesium137$MYEAR)
# technetiuml<-split(technetium,technetium$MYEAR)

chulldf<-ldply(strontiumwl, function(xx){xx[chull(xx[,c('EASTING','NORTHING')]),]})

head(chulldf)
chulldf$.id

Srchullmap<-ggplot(basins27poly, aes(x=EASTING, y=NORTHING)) + 
  geom_polygon(aes(group=id), fill="black", colour = "black") +
 # scale_fill_discrete("Key") +
  labs(list(title = "Convex hull of the wells with clean data for Strontium-90", x = "UTM Easting (m)", y = "UTM Northing (m)"))
print(Srchullmap)


Srchullmap2<-Srchullmap + theme_bw()
Srchullmap2 <- Srchullmap2 + geom_polygon(data=chulldf,aes(group=.id, colour=.id), size =1.5, alpha=0)
Srchullmap2 <- Srchullmap2 + guides(col = guide_legend(title = "Year"))
print(Srchullmap2)


Srchullmap3 <- Srchullmap2 + geom_point(data=strontiumw2, aes(x=EASTING,y=NORTHING))
print(Srchullmap3)

Srchullmap6 <- Srchullmap3 + geom_polygon(data=interp.dom, aes(x=UTM_E,y=UTM_N), colour="black", size =1.5, alpha=0)
print(Srchullmap6)







