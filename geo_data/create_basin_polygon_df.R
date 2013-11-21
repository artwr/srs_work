# Load the basins data for plotting and create the appropriate polygons

#Add basin 3 coordinates
#UTM83
# f3basin<-readRDS("processed/f3basin.rdata")
#NAD27
f3basin27<-readRDS("processed/f3basin27.rdata")

#All the basins in NAD83
# basins<-readRDS("processed/basins.rdata")

#All the basins in NAD27 which is the proj of my data
basins27<-readRDS("processed/basins27.rdata")

# f3basinmap <- ggplot(f3basin, aes(x=EASTING, y=NORTHING)) +
#   geom_polygon(fill="black", colour="black")
# 
# plot(f3basinmap)


## Necessary trick to be able to identify each of the basins
bnames <- data.frame(
  id = c("F3","F2","F1"),
  value = c(4,5,6)
)

# #For the UTM83
# basins$id<-rep(bnames$id, each = 5)
# basinspoly <- merge(basins, bnames, by=c("id"))

#For the UTM27 the correct one
basins27$id<-rep(bnames$id, each = 5)
#Creates a polygon ready for ggpot2
basins27poly <- merge(basins27, bnames, by=c("id"))
saveRDS(basins27poly,"processed/basins27poly.rdata")

# ##############################
# #Plot that identifies the basins with colours
# basinsmap<-ggplot(basins27poly, aes(x=EASTING, y=NORTHING)) + 
#   geom_polygon(aes(group=id, fill=factor(value))) +
#   scale_fill_discrete("Key")
# 
# basinsmap<- basinsmap + expand_limits(x = c(ea.min+50,ea.max+50), y = c(no.min+50,no.max+50))
# 
# plot(basinsmap)
# 
# #Plot with black basins and axis labels
# 
# basinsmap2<-ggplot(basins27poly, aes(x=EASTING, y=NORTHING)) + 
#   geom_polygon(aes(group=id), fill="black") +
#   scale_fill_discrete("Key") +
#   labs(list(title = "Plot of the basins", x = "UTM Easting (m)", y = "UTM Northing (m)"))
# 
# basinsmap2<- basinsmap2 + expand_limits(x = c(ea.min+50,ea.max+50), y = c(no.min+50,no.max+50))
# 
# basinsmap2 <- basinsmap2 + theme_bw() + theme(aspect.ratio =1)
# 
# plot(basinsmap2)
# 
# ############################################################
