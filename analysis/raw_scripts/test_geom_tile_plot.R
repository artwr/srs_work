# 

set.seed(1789)
## Size of the mesh on the regular grid 
## (I used 20m to develop the algorithm)
spatial.step.size<-20;
# There is the possibility to define different step sizes
# in the EASTING and NORTHING dir
spatial.step.size.ea<-spatial.step.size
spatial.step.size.no<-spatial.step.size

#Boundaries of the rectangular domain in UTM27 (m)
no.min<-3680930
no.max<-3682110
ea.min<-436175
ea.max<-437155
#number of spatial.steps ~ 20 m apart
ea.b<-1+(ea.max-ea.min)/spatial.step.size.ea
no.b<-1+(no.max-no.min)/spatial.step.size.no
#Create the vectors
ea.v<-seq(ea.min, ea.max, length = ea.b)
ea.v.test <- ea.v - ea.min
no.v<-seq(no.min, no.max, length = no.b)
no.v.test <- no.v - no.min
#Create the expandgrid df for predictions
interpolation.grid.test<-expand.grid(EASTING=ea.v.test, NORTHING=no.v.test)


# d.ea<-c(ea.min,ea.min,ea.max,ea.max,ea.min)
# d.no<-c(no.min,no.max,no.max,no.min,no.min)
EASTING1<-c(ea.min,ea.min,ea.max,ea.max) - ea.min
NORTHING1<-c(no.min,no.max,no.max,no.min) - no.min
elev<-c(0,59,108,49)

elev1 <- function(ea,no) { z <- .05 * ea + .05 * no 
                           #+ rnorm(n = length(ea), mean = 0, sd = 1 )
                           return(z)
                          }

set.seed(1789)
data.test.elev <- as.data.frame(cbind(runif(n=60, min = 0, max = ea.max - ea.min), runif(n=60, min = 0, max = no.max - no.min)))
names(data.test.elev) <- c("EASTING","NORTHING")
data.test.elev$elev <- elev1(data.test.elev$EASTING,data.test.elev$NORTHING)

# test.elev <- as.data.frame(cbind(EASTING1,NORTHING1,elev))
# names(test.elev) <- c("EASTING","NORTHING","elev")
linmod1 <- lm(elev ~ EASTING+NORTHING, data = data.test.elev)
summary(linmod1)
loessmod1 <- loess(elev ~ EASTING+NORTHING, data = data.test.elev)
prediction <- predict(linmod1, newdata = interpolation.grid.test)
pred.loess <- predict(loessmod1, newdata = interpolation.grid.test)


pfp <- interpolation.grid.test
pfp$lmpred <- prediction
pfp$loesspred <- as.vector(pred.loess)

ggdiag <- ggplot(data = pfp, mapping = aes(x=EASTING,y=NORTHING))
ggres <- ggdiag + geom_tile(aes(fill = lmpred)) + scale_fill_gradient(low = "white", high = "green")
#   ggstderr <- ggdiag + geom_tile(aes(fill = se.fit)) + scale_fill_gradient(low = "white", high = "green")
print(ggres)

ggreslo <- ggdiag + geom_tile(aes(fill = loesspred)) + scale_fill_gradient(low = "white", high = "green")
#   ggstderr <- ggdiag + geom_tile(aes(fill = se.fit)) + scale_fill_gradient(low = "white", high = "green")
print(ggreslo)



