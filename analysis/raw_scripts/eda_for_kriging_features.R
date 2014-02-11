# 
require(sp)
require(rgdal)
require(rgeos)


# Load data
TCCZe<-readRDS("./geo_data/processed/TCCZ_wtoppick.rdata")
# picks<-readRDS("./geo_data/processed/picks_all.rdata")

pairs(TCCZe[,c("EASTING","NORTHING","g_elev_ft","TCCZ_top")])
pairs(TCCZe[,c("EASTING","NORTHING","TCCZ_top")])
TCCZe$logtccztop <- log(TCCZe$TCCZ_top)
pairs(TCCZe[,c("EASTING","NORTHING","logtccztop")])
# Not extremely convincing

# Finding the center of the F-3 basin
f3basin <- readRDS("geo_data/processed/f3basin27.rdata")
# 
ec <- mean(f3basin$EASTING[1:4])
# ec = 436626.4
nc <- mean(f3basin$NORTHING[1:4])
# nc = 3681720
TCCZe$d2basincenter <- sqrt((TCCZe$EASTING - ec)^2 + (TCCZe$NORTHING - nc)^2)

# convert to Spatial PoitnsdataFrane
TCCZsp <- TCCZe
coordinates(TCCZsp) <- c("EASTING","NORTHING")
checkCRSArgs("+proj=utm +zone=17 +datum=NAD27")
proj4string(TCCZsp) <- CRS("+proj=utm +zone=17 +datum=NAD27")
summary(TCCZsp)

# Distance to the river
fmbpoly27 <- readRDS("geo_data/processed/fmbpoly27.rdata")

l1 = cbind(c(min(fmbpoly27$EASTING),max(fmbpoly27$EASTING)),c(min(fmbpoly27$NORTHING),max(fmbpoly27$NORTHING)))
FMBl <- Line(l1)
S1 = Lines(list(FMBl), ID="FMB")
FMBldf <- SpatialLines(list(S1), proj4string = CRS("+proj=utm +zone=17 +datum=NAD27"))
TCCZsp$disttoriver <- as.vector(gDistance(TCCZsp, FMBldf, byid=TRUE))

TCCZe2 <- as.data.frame(TCCZsp)

#
pairs(TCCZe2[,c("EASTING","NORTHING","TCCZ_top", "disttoriver")])
pairs(TCCZe2[,c("EASTING","NORTHING","logtccztop", "disttoriver")])


spplot(TCCZsp, c("TCCZ_top"), aspect = "iso")



# All the picks
# pairs(picks[,c("EASTING", "NORTHING", "g_elev_ft", "Barnwell_top", "TCCZ_top", "Santee_top")])
# picks[is.na(picks$TCCZ_top), ]