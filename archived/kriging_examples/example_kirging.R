##Example of kriging with R. To look at more closely

## load some libraries first:
library(gstat)
## load data
d <- read.csv('elev.csv')

## gstat does not like missing data, subset original data:
e <- na.omit(d)

## convert simple data frame into a spatial data frame object:
coordinates(e) <- ~ x+y

## test result with simple bubble plot:
bubble(e, zcol='elev', fill=FALSE, do.sqrt=FALSE, maxsize=2)

## create a grid onto which we will interpolate:
## first get the range in data
x.range <- as.integer(range(e@coords[,1]))
y.range <- as.integer(range(e@coords[,2]))

## now expand to a grid with 500 meter spacing:
grd <- expand.grid(x=seq(from=x.range[1], to=x.range[2], by=500), y=seq(from=y.range[1], to=y.range[2], by=500) )

## convert to SpatialPixel class
coordinates(grd) <- ~ x+y
gridded(grd) <- TRUE

## test it out:
plot(grd, cex=0.5)
points(e, pch=1, col='red', cex=0.7)
title("Interpolation Grid and Sample Points")

## make gstat object:
g <- gstat(id="elev", formula=elev ~ 1, data=e)

## the original data had a large north-south trend, check with a variogram map
plot(variogram(g, map=TRUE, cutoff=4000, width=200), threshold=10)

## another approach:
# create directional variograms at 0, 45, 90, 135 degrees from north (y-axis)
v <- variogram(g, alpha=c(0,45,90,135))

## 0 and 45 deg. look good. lets fit a linear variogram model:
## an un-bounded variogram suggests additional source of anisotropy... oh well.
v.fit <- fit.variogram(v, model=vgm(model='Lin' , anis=c(0, 0.5)))

## plot results:
plot(v, model=v.fit, as.table=TRUE)

## update the gstat object:
g <- gstat(g, id="elev", model=v.fit )

## perform ordinary kriging prediction:
p <- predict(g, model=v.fit, newdata=grd)

## visualize it:

## base graphics
par(mar=c(2,2,2,2))
image(p, col=terrain.colors(20))
contour(p, add=TRUE, drawlabels=FALSE, col='brown')
points(e, pch=4, cex=0.5)
title('OK Prediction')

## lattice graphics: thanks for R. Bivand's advice on this
##
## alternatively plot quantiles with
## ... col.regions=terrain.colors(6), cuts=quantile(p$elev.pred) ...
##
pts <- list("sp.points", e, pch = 4, col = "black", cex=0.5)
spplot(p, zcol="elev.pred", col.regions=terrain.colors(20), cuts=19, sp.layout=list(pts), contour=TRUE, labels=FALSE, pretty=TRUE, col='brown', main='OK Prediction')

## plot the kriging variance as well
spplot(p, zcol='elev.var', col.regions=heat.colors(100), cuts=99, main='OK Variance',sp.layout=list(pts) )
