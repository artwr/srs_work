#Create a plot including selected wells as points
#and the basins as polygons

require(geoR)
require(ggplot2)
require(scales)
#require(sqldf)

source('../inventory/interpolation_domain.R')

# rm(list=ls())

##########################################################

#Add basin 3 coordinates
f3basin<-readRDS("../basin_coords/f3basin.rdata")
f3basin27<-readRDS("../basin_coords/f3basin27.rdata")

f3basinmap <- ggplot(f3basin, aes(x=UTM_E, y=UTM_N)) +
  geom_polygon(fill="black", colour="black")

plot(f3basinmap)

#All the basins
basins<-readRDS("../basin_coords/basins.rdata")
basins27<-readRDS("../basin_coords/basins27.rdata")


## Necessary trick to be able to identofy each of the basins
bnames <- data.frame(
  id = c("F3","F2","F1"),
  value = c(4,5,6)
)

basins$id<-rep(bnames$id, each = 5)
basins27$id<-rep(bnames$id, each = 5)

basinspoly <- merge(basins, bnames, by=c("id"))
basins27poly <- merge(basins27, bnames, by=c("id"))

# #Extent of the interpolation domain
# no.min<-3680930
# no.max<-3682110
# ea.min<-436175
# ea.max<-437155


#Plot that identifies the basins with colours
basinsmap<-ggplot(basins27poly, aes(x=UTM_E, y=UTM_N)) + 
  geom_polygon(aes(group=id, fill=factor(value))) +
  scale_fill_discrete("Key")

basinsmap<- basinsmap + expand_limits(x = c(ea.min+50,ea.max+50), y = c(no.min+50,no.max+50))

plot(basinsmap)

#Plot with black basins and axis labels

basinsmap2<-ggplot(basins27poly, aes(x=UTM_E, y=UTM_N)) + 
  geom_polygon(aes(group=id), fill="black") +
  scale_fill_discrete("Key") +
  labs(list(title = "Plot of the basins", x = "UTM Easting (m)", y = "UTM Northing (m)"))

basinsmap2<- basinsmap2 + expand_limits(x = c(ea.min+50,ea.max+50), y = c(no.min+50,no.max+50))

basinsmap2 <- basinsmap2 + theme_bw() + theme(aspect.ratio =1)

plot(basinsmap2)

############################################################

#Add tritium data
tritium<-readRDS("../SRS_data/tritiumf.rdata")
names(tritium)
#create a dataset of all unique wells of interest
allwells<-unique(tritium[,c("STATION_ID","EASTING","NORTHING")])
names(allwells)<-c("STATION_ID","UTM_E","UTM_N")
names(allwells)

#Create Dataframe with selected years
yoi<-c(1979,1984,1988,1995,2000,2011)
tritiumwellsyoi<-subset(tritium[,c("STATION_ID","EASTING","NORTHING","MYEAR")], MYEAR %in% yoi)
names(tritiumwellsyoi)<-c("STATION_ID","UTM_E","UTM_N","Year")

###############################################

#Create a plot with the basins + the wells

basinsmap3<-ggplot(basins27poly, aes(x=UTM_E, y=UTM_N)) + 
  geom_polygon(aes(group=id), fill="black") +
  scale_fill_discrete("Key") +
  labs(list(title = "Plot of the basins with wells", x = "UTM Easting (m)", y = "UTM Northing (m)"))

basinsmap3<- basinsmap3 + geom_point(data=allwells, aes(x=UTM_E, y=UTM_N))

plot(basinsmap3)


# Add interpolation domain

basinsmap4 <- basinsmap3 + geom_polygon(data=interp.dom, fill = "orange", colour = "red", alpha = 1/3)

plot(basinsmap4)

#same but with less fill on basins
basinsmap5<-ggplot(basins27poly, aes(x=UTM_E, y=UTM_N)) + 
  geom_polygon(aes(group=id), fill="light grey", colour = "blue", alpha = 1/3) +
  scale_fill_discrete("Key") +
  labs(list(title = "Plot of the basins with wells", x = "UTM Easting (m)", y = "UTM Northing (m)"))

basinsmap5<- basinsmap5 + geom_point(data=allwells, aes(x=UTM_E, y=UTM_N))

basinsmap5<- basinsmap5 + geom_polygon(data=interp.dom, fill = "orange", colour = "red", alpha = 1/3)

basinsmap5<- basinsmap5 + geom_polygon(data=f3basin, fill = "light grey", colour = "green", alpha = 1/3)
  
basinsmap5<- basinsmap5 + geom_polygon(data=f3basin27, fill = "light grey", colour = "purple", alpha = 1/3)

plot(basinsmap5)





##

basins27poly2 <- basins27poly
basins27poly2$Year <- NULL

interp.dom2 <- interp.dom
interp.dom2$Year <- NULL

wellsmap1<-ggplot(tritiumwellsyoi, aes(x=UTM_E, y=UTM_N)) + geom_point(data = tritiumwellsyoi, aes(x=UTM_E, y=UTM_N)) + facet_wrap(~ Year, nrow = 2)
# + geom_polygon(data=interp.dom, fill = "orange", colour = "red", alpha = 1/3) 

wellsmap1<- wellsmap1 + theme_bw() + labs(title="Active wells")

wellsmap1<- wellsmap1 + labs(x = "UTM Easting (m)", y= "UTM Northing (m)")

plot(wellsmap1)


wellsmap2<-ggplot(tritiumwellsyoi, aes(x=UTM_E, y=UTM_N)) + geom_point(data = tritiumwellsyoi, aes(x=UTM_E, y=UTM_N)) 
# + geom_polygon(data=interp.dom, fill = "orange", colour = "red", alpha = 1/3) 

wellsmap2<- wellsmap2 + theme_bw() + labs(title="Active wells per year with respect to D")

wellsmap2<- wellsmap2 + labs(x = "UTM Easting (m)", y= "UTM Northing (m)")

wellsmap2<- wellsmap2 + geom_polygon(data=basins27poly2,aes(group=id), fill="black")
  
wellsmap2<- wellsmap2 + geom_polygon(data=interp.dom2, fill = "orange", colour = "red", alpha = 1/3)

wellsmap2<- wellsmap2 + facet_wrap(~ Year, nrow = 2)

wellsmap2<- wellsmap2  + scale_x_continuous(breaks=c(435000,436000,437000))

plot(wellsmap2)

##


pdf(file="tritiumwellsperyear_test.pdf", width=7, height=10,family="Courier")

wellsmap3<-ggplot(tritiumwellsyoi, aes(x=UTM_E, y=UTM_N)) + geom_point(data = tritiumwellsyoi, aes(x=UTM_E, y=UTM_N)) 
# + geom_polygon(data=interp.dom, fill = "orange", colour = "red", alpha = 1/3) 

wellsmap3<- wellsmap3 + theme_bw() + labs(title="Active wells per year with respect to D")

wellsmap3<- wellsmap3 + labs(x = "UTM Easting (m)", y= "UTM Northing (m)")

wellsmap3<- wellsmap3 + geom_polygon(data=basins27poly2,aes(group=id), fill="black")

wellsmap3<- wellsmap3 + geom_polygon(data=interp.dom2, fill = "orange", colour = "red", alpha = 1/3)

wellsmap3<- wellsmap3 + facet_wrap(~ Year, ncol = 2)

wellsmap3<- wellsmap3  + scale_x_continuous(breaks=c(435000,436000,437000))

print(wellsmap3)

dev.off()










## playing with geoR as.geodata etc...
#Split per measurement year
tritiuml<-split(tritium,tritium$MYEAR)

tritiumlgeo<-llply(tritiuml,function(twells) {as.geodata(twells, coords.col = 4:5, data.col = 6:14, data.names = NULL, na.action = "none")})

for (ii in seq(1,length(tritiuml))) {
  print(names(tritiuml[ii]))
  as.geodata(tritiuml[[ii]], coords.col = 4:5, data.col = 6:14, data.names = NULL, na.action = "none")
  }
    
head(tritiuml[22])
t2000df<-tritiuml[['2000']]
options(gsubfn.engine = "R")
sqldf("select a1.STATION_ID,a2.STATION_ID from t2000df a1, t2000df a2 where a1.EASTING=a2.EASTING and a1.NORTHING=a2.NORTHING and a1.STATION_ID<a2.STATION_ID",drv = "SQLite")

sqldf("select * from sqlite_master")



##Add wells with black markers


