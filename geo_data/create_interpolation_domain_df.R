#Define interpolation domain and compute area, define other parameters
require(splancs)

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
no.v<-seq(no.min, no.max, length = no.b)
#Create the expandgrid df for predictions
interpolation.grid<-expand.grid(EASTING=ea.v, NORTHING=no.v)

# Create polygon to compute area. 
# Needs splancs lib for more complex polygons
d.ea<-c(ea.min,ea.min,ea.max,ea.max,ea.min)
d.no<-c(no.min,no.max,no.max,no.min,no.min)
pp<-cbind(d.ea,d.no)
#plot(pp, type="b")
area.dom<-areapl(pp)

#Area weights
weightarea<-(spatial.step.size.ea*spatial.step.size.no)/area.dom

interp.dom<-as.data.frame(pp)
# interp.dom2<-interp.dom
# names(interp.dom)<-c("UTM_E","UTM_N")
names(interp.dom)<-c("EASTING","NORTHING")
