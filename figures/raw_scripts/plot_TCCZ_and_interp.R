# Plot of the TCCZ picks location + basins
require(ggplot2)

# Import Data -------------------------------------------------------------

source('../../geo_data/create_basin_polygon_df.R')

#Import picks for the TCCZ
TCCZe<-readRDS("../../geo_data/processed/TCCZ_wtoppick.rdata")


# Define baseplot ---------------------------------------------------------

TCCZbasinsmap<-ggplot(basins27poly, aes(x=EASTING, y=NORTHING)) + 
  geom_polygon(aes(group=id), fill="light grey", colour = "blue", alpha = 1/3) +
  scale_fill_discrete("Key") +
  theme_bw() +
  labs(list(title = "Plot of the basins with boreholes with TCCZ picks", x = "UTM Easting (m)", y = "UTM Northing (m)"))

TCCZbasinsmap<- TCCZbasinsmap + geom_point(data=TCCZe, aes(x=EASTING, y=NORTHING))

TCCZbasinsmap2<- TCCZbasinsmap + geom_polygon(data=interp.dom2, fill="light grey", colour = "orange", alpha=0)

print(TCCZbasinsmap)

print(TCCZbasinsmap2)
