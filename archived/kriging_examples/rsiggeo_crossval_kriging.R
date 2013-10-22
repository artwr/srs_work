library(gstat)
library(lattice)

data(meuse);
## trim dataset to useful columns
meuse=meuse[, c("x", "y", "zinc", "dist")]   
## log transform zinc
meuse$logZinc=log(meuse$zinc)              
## set positions for sampling transects
Transects=seq(from=330000, by=700, length=5); 
## extract sample data along transects
Sampled=meuse[which(x=(meuse$y>Transects[1]-100 & meuse$y< (Transects[1]+100) ) |
                      (meuse$y>Transects[2]-100 & meuse$y< (Transects[2]+100) ) |
                      (meuse$y>Transects[3]-100 & meuse$y< (Transects[3]+100) ) |
                      (meuse$y>Transects[4]-100 & meuse$y< (Transects[4]+100) ) |
                      (meuse$y>Transects[5]-100 & meuse$y< (Transects[5]+100) ) ), ]

coordinates(meuse)=c("x", "y");
coordinates(Sampled)=c("x", "y");
plot(meuse)
points(Sampled, pch=20, cex=1.5)

##W=W+10
## calculate variogram
Var=variogram(logZinc~sqrt(dist), Sampled, cutoff=1300, width=90);
plot(Var)
## fit variogram model
Var.Fit=fit.variogram(Var, vgm(model="Sph", nugget=0.1, psill=0.3,range=800)); 
plot(Var, Var.Fit)

## prepare prediction data set
data(meuse.grid) 
coordinates(meuse.grid) <- c("x", "y")
meuse.grid <- as(meuse.grid, "SpatialPixelsDataFrame")

Pred=krige(logZinc~sqrt(dist), Sampled, meuse.grid, Var.Fit)#,block=c(40,40)) ## predict by kriging
meuse.grid$Zinc.logPred=Pred$var1.pred; ## log-transformed predictions
meuse.grid$Zinc.Var=Pred$var1.var;  ## log-transformed variances
meuse.grid$Zinc.Pred.BT=exp(meuse.grid$Zinc.logPred+meuse.grid$Zinc.Var/2)
## bias-adjust and back-transform

trellis.par.set(sp.theme()) ## plot
spplot(meuse.grid["Zinc.logPred"], main="Kriging Prediction")
spplot(meuse.grid["Zinc.Var"], main="Kriging variance")

mean(meuse$zinc)
mean(meuse.grid$Zinc.Pred.BT)
