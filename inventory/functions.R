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

# Multiple plot function
#
# ggplot objects can be passed in ..., or to plotlist (as a list of ggplot objects)
# - cols:   Number of columns in layout
# - layout: A matrix specifying the layout. If present, 'cols' is ignored.
#
# If the layout is something like matrix(c(1,2,3,3), nrow=2, byrow=TRUE),
# then plot 1 will go in the upper left, 2 will go in the upper right, and
# 3 will go all the way across the bottom.
#
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  require(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}
