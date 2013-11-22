# getexpandgridb<- function(xd.min,xd.max,yd.min,yd.max,nb.xbreaks,nb.ybreaks) {
#   xd.v<-seq(xd.min, xd.max, length = xd.b)
#   yd.v<-seq(yd.min, yd.max, length = yd.b)
#   expand.grid(EASTING=ea.v, NORTHING=no.v)
# }
# 
# getexpandgridd<- function(xd.min,xd.max,yd.min,yd.max,y.step.size,x.step.size) {
#   xd.v<-seq(xd.min, xd.max, length = xd.b)
#   yd.v<-seq(yd.min, yd.max, length = yd.b)
#   expand.grid(EASTING=ea.v, NORTHING=no.v)
# }

createInterpGrid <- function(xd.min,xd.max,yd.min,yd.max,y.step.size,x.step.size) {
  # Compute the number of elements needed in the vector
  # to satisfy the step size. The ceiling function ensures that
  # the actual stepsize will be less than or equal to the one chosen 
  xd.n<-ceiling(1+(xd.max-xd.min)/x.step.size)
  yd.n<-ceiling(1+(yd.max-yd.min)/y.step.size)
  # Create the vectors
  xd.v<-seq(xd.min, xd.max, length.out = xd.n)
  yd.v<-seq(yd.min, yd.max, length.out = yd.n)
  # The last statement's result is the value returned by the function
  expand.grid(EASTING=ea.v, NORTHING=no.v)
}