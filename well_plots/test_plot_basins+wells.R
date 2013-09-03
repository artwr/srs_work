#Create a plot including selected wells as points
#and the basins as polygons

require("geoR")
require("ggplot2")
require("scales")
require("rgdal") # requires sp, will use proj.4 if installed
require("maptools")
require("plyr")

source('../interpolation_domains/interpolation_domain.R')
source('../basin_coords/basin_poly.R')
source('../shapes/createriverdf.R')

# rm(list=ls())

##########################################################

# #Extent of the interpolation domain
# no.min<-3680930
# no.max<-3682110
# ea.min<-436175
# ea.max<-437155


#Plot that identifies the basins with colours
basinsmap<-ggplot(basins27poly, aes(x=EASTING, y=NORTHING)) + 
  geom_polygon(aes(group=id, fill=factor(value))) +
  scale_fill_discrete("Key")

basinsmap<- basinsmap + expand_limits(x = c(ea.min+50,ea.max+50), y = c(no.min+50,no.max+50))

plot(basinsmap)

#Plot with black basins and axis labels

basinsmap2<-ggplot(basins27poly, aes(x=EASTING, y=NORTHING)) + 
  geom_polygon(aes(group=id), fill="black") +
  scale_fill_discrete("Key") +
  labs(list(title = "Plot of the basins", x = "UTM Easting (m)", y = "UTM Northing (m)"))

basinsmap2<- basinsmap2 + expand_limits(x = c(ea.min+50,ea.max+50), y = c(no.min+50,no.max+50))

basinsmap2 <- basinsmap2 + theme_bw() + theme(aspect.ratio =1)

plot(basinsmap2)

#################
#Add river line
##################################

basinsmap2r<-ggplot(basins27poly, aes(x=EASTING, y=NORTHING)) + 
  geom_polygon(aes(group=id), fill="black") +
  scale_fill_discrete("Key") +
  labs(list(title = "Plot of the basins", x = "UTM Easting (m)", y = "UTM Northing (m)"))

# basinsmap2r<- basinsmap2r + geom_polygon(data=river.df, aes(group=id), fill="blue", colour="blue")

basinsmap2r<- basinsmap2r + geom_line(data=river.df, aes(x=EASTING, y=NORTHING), colour="blue", size=1)

basinsmap2r<- basinsmap2r + coord_cartesian(xlim = c(ea.min+50,ea.max+50), ylim = c(no.min+50,no.max+50))

basinsmap2r <- basinsmap2r + theme_bw() + theme(aspect.ratio =1)

plot(basinsmap2r)


############################################################

#Add tritium data
tritium<-readRDS("../SRS_data/tritiumf.rdata")
names(tritium)
#create a dataset of all unique wells of interest
allwells<-unique(tritium[,c("STATION_ID","EASTING","NORTHING")])
#names(allwells)<-c("STATION_ID","UTM_E","UTM_N")
names(allwells)

#Create Dataframe with selected years
yoi<-c(1979,1984,1988,1995,2000,2011)
tritiumwellsyoi<-subset(tritium[,c("STATION_ID","EASTING","NORTHING","MYEAR")], MYEAR %in% yoi)
names(tritiumwellsyoi)<-c("STATION_ID","EASTING","NORTHING","Year")

###############################################

#Create a plot with the basins + the wells

basinsmap3<-ggplot(basins27poly, aes(x=EASTING, y=NORTHING)) + 
  geom_polygon(aes(group=id), fill="black") +
  scale_fill_discrete("Key") +
  labs(list(title = "Plot of the basins with wells", x = "UTM Easting (m)", y = "UTM Northing (m)"))

basinsmap3<- basinsmap3 + geom_point(data=allwells, aes(x=EASTING, y=NORTHING))

plot(basinsmap3)


# Add interpolation domain

basinsmap4 <- basinsmap3 + geom_polygon(data=interp.dom2, fill = "orange", colour = "red", alpha = 1/3)

plot(basinsmap4)

#same but with less fill on basins
basinsmap5<-ggplot(basins27poly, aes(x=EASTING, y=NORTHING)) + 
  geom_polygon(aes(group=id), fill="light grey", colour = "blue", alpha = 1/3) +
  scale_fill_discrete("Key") +
  labs(list(title = "Plot of the basins with wells", x = "UTM Easting (m)", y = "UTM Northing (m)"))

basinsmap5<- basinsmap5 + geom_point(data=allwells, aes(x=EASTING, y=NORTHING))

basinsmap5<- basinsmap5 + geom_polygon(data=interp.dom2, fill = "orange", colour = "red", alpha = 1/3)

#basinsmap5<- basinsmap5 + geom_polygon(data=f3basin, fill = "light grey", colour = "green", alpha = 1/3)
  
#basinsmap5<- basinsmap5 + geom_polygon(data=f3basin27, fill = "light grey", colour = "purple", alpha = 1/3)

plot(basinsmap5)


#With the river 
basinsmap6 <- basinsmap5 + geom_line(data=river.df, aes(x=EASTING, y=NORTHING), colour="blue", size=1)

basinsmap6<- basinsmap6 + coord_cartesian(xlim = c(min(allwells$EASTING)+50,max(allwells$EASTING)+50), ylim = c(min(allwells$NORTHING)+50,max(allwells$NORTHING)+50))

plot(basinsmap6)



##

basins27polyd <- basins27poly
basins27polyd$Year <- NULL

interp.domd <- interp.dom2
interp.domd$Year <- NULL

wellsmap1<-ggplot(tritiumwellsyoi, aes(x=EASTING, y=NORTHING)) + geom_point(data = tritiumwellsyoi, aes(x=EASTING, y=NORTHING)) + facet_wrap(~ Year, nrow = 2)
# + geom_polygon(data=interp.dom, fill = "orange", colour = "red", alpha = 1/3) 

wellsmap1<- wellsmap1 + theme_bw() + labs(title="Active wells")

wellsmap1<- wellsmap1 + labs(x = "UTM Easting (m)", y= "UTM Northing (m)")

plot(wellsmap1)


wellsmap2<-ggplot(tritiumwellsyoi, aes(x=EASTING, y=NORTHING)) + geom_point(data = tritiumwellsyoi, aes(x=EASTING, y=NORTHING)) 
# + geom_polygon(data=interp.dom, fill = "orange", colour = "red", alpha = 1/3) 

wellsmap2<- wellsmap2 + theme_bw() + labs(title="Active wells monitoring the UAZ (1979-2011)")

wellsmap2<- wellsmap2 + labs(x = "UTM Easting (m)", y= "UTM Northing (m)")

wellsmap2<- wellsmap2 + geom_polygon(data=basins27polyd,aes(group=id), fill="black")
  
wellsmap2<- wellsmap2 + geom_polygon(data=interp.domd, fill = "orange", colour = "red", alpha = 1/3)

wellsmap2<- wellsmap2 + facet_wrap(~ Year, nrow = 2)

wellsmap2<- wellsmap2  + scale_x_continuous(breaks=c(435000,436000,437000))

plot(wellsmap2)

##
#paste("../dgraphs/tritiumwellsperyear_",format(Sys.time(), "%Y-%m-%d %H-%M"), ".pdf", sep = "")

pdf(file=paste("../dgraphs/tritiumwellsperyear_",format(Sys.time(), "%Y-%m-%d %H-%M"), ".pdf", sep = ""), width=7, height=10,family="Courier")

wellsmap3<-ggplot(tritiumwellsyoi, aes(x=EASTING, y=NORTHING)) + geom_point(data = tritiumwellsyoi, aes(x=EASTING, y=NORTHING)) 
# + geom_polygon(data=interp.dom, fill = "orange", colour = "red", alpha = 1/3) 

wellsmap3<- wellsmap3 + theme_bw() + labs(title="Active wells monitoring the UAZ per year (1979-2011)")

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


