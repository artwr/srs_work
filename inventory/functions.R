require(plyr)


# Function to compute the minimum bounding rectangle 
# for each year in the dataset
minboundrect<-function(interpdataset) {ddply(interpdataset, c('MYEAR'), function(x) c(datacount=nrow(x),x.min=min(x$EASTING),x.max=max(x$EASTING),y.min=min(x$NORTHING),y.max=max(x$NORTHING)))}


# Function to compute the cumulative input decay corrected
# Using a vector containing the original inputs
# and a constant containing the half-life in years

cumsumdecaycorrected<-function(yearlyvalues,halflife) {
  k<- log(2)/halflife; ek<-exp(-k);
  n<-length(yearlyvalues);v<-ek^(0:n-1);
  MM<-rep.int(0,n) %o% rep.int(0,n);
  for (jj in 1:(n-1)) {
    MM[jj:n,jj]<-v[1:(n-(jj-1))]
  }
  return (MM %*% yearlyvalues)
}


#basin coords for plotting if needed
#f3basin<-readRDS("../basin_coords/f3basin.rdata")
#f3basin27<-readRDS("../basin_coords/f3basin27.rdata")



# tritium.plyra<-ddply(tritium.clean, c('MYEAR'), function(x) c(datacount=nrow(x),x.min=min(x$RESULT),x.max=max(x$),min=min(x$RESULT),y.max=max(x$)))
# getexpandgridb<- function(xd.min,xd.max,yd.min,yd.max,nb.xbreaks,nb.ybreaks) {
#   xd.v<-seq(xd.min, xd.max, length = xd.b)
#   yd.v<-seq(yd.min, yd.max, length = yd.b)
#   expand.grid(EASTING=ea.v, NORTHING=no.v)
# }
# 
# getexpandgridd<- function(xd.min,xd.max,yd.min,yd.max,ybreaks.d,xbreaksd) {
#   xd.v<-seq(xd.min, xd.max, length = xd.b)
#   yd.v<-seq(yd.min, yd.max, length = yd.b)
#   expand.grid(EASTING=ea.v, NORTHING=no.v)
# }
