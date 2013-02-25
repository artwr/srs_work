#Create a plot including selected wells as points
#and the basins as polygons

require(geoR)
require(ggplot2)

rm(list=ls())

#Add basin 3 coordinates
f3basin<-readRDS("../basin_coords/f3basin.rdata")
f3basin27<-readRDS("../basin_coords/f3basin27.rdata")

f3basinmap <- ggplot(f3basin, aes(x=UTM_E, y=UTM_N)) +
  geom_polygon(fill="black", colour="black")

plot(f3basinmap)