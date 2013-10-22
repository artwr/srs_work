#Kalman Filter example

#Model
Mt<-exp(-.18)
#Init cond
x0<-5e5
#Initial variance
P0<-1e5
Qt<-100



Pf<-Mt*Pa*Mt + Qt;

#Innovation vector
dt<-0; 