#Plot the error on calculating the aquifer

source('../inventory/aquifer_comp.R')

require(spatstat) 
require(maptools)
#require(ggplot2) 
#require(scales)
#

#ddd dataframe of interest
# W <- ripras(ddd, shape="rectangle") 
# W <- owin(c(0, 30), c(0, 30)) 
# X <- as.ppp(ddd, W=W) 
# Y <- dirichlet(X) 
# Z <- as(Y, "SpatialPolygons") 
# plot(Z, col=grey(ddd$z/max(ddd$z)))


# summary(pre2)
ploterrorTCCZ<-testgrid1
#
ploterrorTCCZ$TCCZ.fit<-as.vector(pre3$fit)
ploterrorTCCZ$TCCZ.se.fit<-as.vector(pre3$se.fit)
ploterrorTCCZ$TCCZ.fitb<-pre3b$fit
ploterrorTCCZ$TCCZ.se.fitb<-pre3b$se.fit
ploterrorTCCZ$TCCZ.fitlm<-pre3lm$fit[,1]
ploterrorTCCZ$TCCZ.se.fitlm<-pre3lm$se.fit
#Upper Bound 95% prediction interval
ploterrorTCCZ$TCCZ.fitlmupr<-pre3lm$fit[,2]
#Lower Bound 95% prediction interval
ploterrorTCCZ$TCCZ.fitlmlwr<-pre3lm$fit[,3]



# #image.plots in the mean time
# image.plot(ea.v,no.v,pre3$fit)
# image.plot(ea.v,no.v,pre3$se.fit)
# image.plot(ea.v,no.v,pre3b$fit)
# image.plot(ea.v,no.v,pre3b$se.fit)
# lmfit<-pre3lm$fit[,1]
# lmfitse<-pre3lm$se.fit
# #nb breaks easting : 50, northing : 60
# dim(lmfit) <- c(50,60)
# dim(lmfitse) <- c(50,60)
# image.plot(ea.v,no.v,lmfit)
# image.plot(ea.v,no.v,lmfitse)


# #ggplot to be fixed...
# plotTCCZerror <- ggplot(ploterrorTCCZ, aes(x=EASTING,y=NORTHING)) 
# + geom_tile(aes(fill=TCCZ.fit)) 
# + scale_fill_gradient(low="green", high="red")
# + geom_point(data=TCCZe, coulour= "black" , size = 4) 
# print(plotTCCZerror)
# + scale_colour_gradient2(low="red", high="blue") 
# print(ph)
# #Close up in the area + 100m margin to the interpolation domain.
# ph2 <- ph + xlim(ea.min-100,ea.max+100) + ylim(no.min-100,no.max+100)
# print(ph2)
# sum(abs(numbers - x) < 1e-6)