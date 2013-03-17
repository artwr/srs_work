#Define interpolation domain and compute area, define other parameters
require(splancs)
#Boundaries
no.min<-3680930
no.max<-3682110
ea.min<-436175
ea.max<-437155
#number of breaks ~ 20 m apart
ea.b<-1+(ea.max-ea.min)/20
no.b<-1+(no.max-no.min)/20
#Create the vectors
ea.v<-seq(ea.min, ea.max, length = ea.b)
no.v<-seq(no.min, no.max, length = no.b)
#Create the expandgrid df for predictions
testgrid1<-expand.grid(EASTING=ea.v, NORTHING=no.v)

# Create polygon to compute area. 
# Needs splancs lib for more complex polygons
d.ea<-c(ea.min,ea.min,ea.max,ea.max,ea.min)
d.no<-c(no.min,no.max,no.max,no.min,no.min)
pp<-cbind(d.ea,d.no)
#plot(pp, type="b")
area.dom<-areapl(pp)

interp.dom<-as.data.frame(pp)
names(interp.dom)<-c("UTM_E","UTM_N")