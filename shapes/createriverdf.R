#Create df for plotting FMB

# riverNAD83 = readOGR(dsn="../shapes", layer="just_fmb_nearF")
riverNAD83 = readOGR(dsn="../shapes", layer="just_fmb_nearF3withH")
# summary(riverNAD83)
riverUTM27<-spTransform(riverNAD83,CRS("+proj=utm +zone=17 +datum=NAD27"))
# summary(riverUTM27)
riverUTM27@data$id = rownames(riverUTM27@data)
river.points = fortify(riverUTM27, region="id")
river.df = join(river.points, riverUTM27@data, by="id")
# the transformation does not change the name of the variables. 
# In this case long->easting and lat->northing
names(river.df)[1:2]<-c("EASTING","NORTHING")
rm(riverNAD83)
rm(riverUTM27)