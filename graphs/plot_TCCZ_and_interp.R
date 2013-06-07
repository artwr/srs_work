# Plot of the TCCZ picks location + basins


# Import Data -------------------------------------------------------------

source('../basin_coords/basin_poly.R')

#Import picks for the TCCZ
TCCZe<-readRDS("../TCCZ_krig/TCCZ/TCCZ_o.rdata")


# Define baseplot ---------------------------------------------------------

TCCZbasinsmap<-ggplot(basins27poly, aes(x=EASTING, y=NORTHING)) + 
  geom_polygon(aes(group=id), fill="light grey", colour = "blue", alpha = 1/3) +
  scale_fill_discrete("Key") +
  labs(list(title = "Plot of the basins with wells", x = "UTM Easting (m)", y = "UTM Northing (m)"))

TCCZbasinsmap<- TCCZbasinsmap + geom_point(data=TCCZe, aes(x=EASTING, y=NORTHING))