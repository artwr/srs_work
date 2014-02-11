# Plot tiles
require(ggmap)
# thicknessUAZ

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
        
