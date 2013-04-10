#Define interpolation domain and compute area, define other parameters
require(splancs)

#
break.size<-20;
break.size.ea<-break.size
break.size.no<-break.size

#Boundaries of the rectangular domain
no.min<-3680930
no.max<-3682110
ea.min<-436175
ea.max<-437155
#number of breaks ~ 20 m apart
ea.b<-1+(ea.max-ea.min)/break.size.ea
no.b<-1+(no.max-no.min)/break.size.no
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

#Area weights
weightarea<-(break.size.ea*break.size.no)/area.dom

interp.dom<-as.data.frame(pp)
names(interp.dom)<-c("UTM_E","UTM_N")