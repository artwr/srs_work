#Create a plot including selected wells as points
#and the basins as polygons

require(geoR)
require(ggplot2)

rm(list=ls())

#Add basin 3 coordinates
f3basin<-readRDS("../basin_coords/f3basin.rdata")
f3basin27<-readRDS("../basin_coords/f3basin27.rdata")

f3basinmap <- ggplot(basins, aes(x=UTM_E, y=UTM_N)) +
  geom_polygon(fill="black", colour="black")

plot(f3basinmap)

#All the basins
basins<-readRDS("../basin_coords/basins.rdata")

bnames <- data.frame(
  id = c("F3","F2","F1"),
  value = c(4,5,6)
)

basins$id<-rep(bnames$id, each = 5)

datapoly <- merge(basins, bnames, by=c("id"))

#Extent of the interpolation domain
no.min<-3680930
no.max<-3682110
ea.min<-436175
ea.max<-437155


#Plot that identifies the basins with colours
basinsmap<-ggplot(datapoly, aes(x=UTM_E, y=UTM_N)) + 
  geom_polygon(aes(group=id, fill=factor(value))) +
  scale_fill_discrete("Key")

basinsmap<- basinsmap + expand_limits(x = c(ea.min+50,ea.max+50), y = c(no.min+50,no.max+50))

plot(basinsmap)

#Plot with black basins and axis labels

basinsmap2<-ggplot(datapoly, aes(x=UTM_E, y=UTM_N)) + 
  geom_polygon(aes(group=id), fill="black") +
  scale_fill_discrete("Key") +
  labs(list(title = "Plot of the basins", x = "UTM Easting (m)", y = "UTM Northing (m)"))

basinsmap2<- basinsmap2 + expand_limits(x = c(ea.min+50,ea.max+50), y = c(no.min+50,no.max+50))

plot(basinsmap2)