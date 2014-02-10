# Load the basins data for plotting and create the appropriate polygons
require(ggplot2)
require(scales)
#Add coordinates of an outline of FMB
#NAD27
fmbpoly27 <- readRDS("geo_data/processed/fmbpoly27.rdata")

#All the basins in NAD83
# basins<-readRDS("../basin_coords/basins.rdata")

#All the basins in NAD27 which is the proj of my data
basins27poly <- readRDS("geo_data/processed/basins27poly.rdata")

# #Plot with black basins and axis labels
# 
basinsrivermap<-ggplot(basins27poly, aes(x=EASTING, y=NORTHING)) + 
  geom_polygon(aes(group=id), fill="black") +
  scale_fill_discrete("Key") +
  geom_polygon(data=fmbpoly27, fill="blue", colour="black") +
  labs(list(title = "Plot of the basins", x = "UTM Easting (m)", y = "UTM Northing (m)"))

plot(basinsrivermap)

# 
# ############################################################
