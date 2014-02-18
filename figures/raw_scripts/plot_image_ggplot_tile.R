# Plot tiles

currentparamforplotting <- par(no.readonly = TRUE)
require(ggthemes)
# + scale_colour_gradient_tableau("Blue-Green Sequential", limits=c(3, 4))
# + scale_colour_gradient_tableau("Purple Sequential", limits=c(3, 4))
# + scale_colour_gradient_tableau("Grey Sequential", limits=c(3, 4))

require(ggmap)
# thicknessUAZ

basetileggplot <- function(df) {
  ggdiag <- ggplot(data = df, mapping = aes(x=EASTING,y=NORTHING)) + theme_bw(base_size = 14)
}


ggdiag <- ggplot(data = thicknessUAZ, mapping = aes(x=EASTING,y=NORTHING))
ggres <- ggdiag + geom_tile(aes(fill = TCCZfit)) + scale_fill_gradient(low = "white", high = "green")
#   ggstderr <- ggdiag + geom_tile(aes(fill = se.fit)) + scale_fill_gradient(low = "white", high = "green")
print(ggres)
#   print(ggstderr)

ggres2 <- ggdiag + geom_tile(aes(fill = TCCZfit)) + scale_fill_gradient(limits=c(160, 200), low="red")

print(ggres2)

range(thicknessUAZ$e1998, na.rm = TRUE)
ggthickness1998 <- ggdiag + geom_tile(aes(fill = e1998)) + scale_fill_gradient(limits=c(0, 70), low="red")

print(ggthickness1998)

ggthickness1998gb <- ggdiag + geom_tile(aes(fill = e1998)) + scale_fill_gradient(limits=c(0, 70), low="green", high="brown")

print(ggthickness1998gb)

ggimage(t(pre3$fit[,ncol(pre3$fit):1]))
        
