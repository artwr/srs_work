#Create a plot including selected wells as points
#and the basins as polygons

library(ggplot2)

ids <- letters[1:2]

# IDs and values to use for fill colour
values <- data.frame(
  id = ids,
  value = c(4,5)
)

# Polygon position
positions <- data.frame(
  id = rep(ids, each = 10),
  #     shape      hole         shape        hole
  x = c(1,4,4,1,1, 2,2,3,3,2,   5,10,10,5,5, 6,6,9,9,6),
  y = c(1,1,4,4,1, 2,3,3,2,2,   5,5,10,10,5, 6,9,9,6,6)
)

# Merge positions and values
datapoly <- merge(values, positions, by=c("id"))

ggplot(datapoly, aes(x=x, y=y)) + 
  geom_polygon(aes(group=id, fill=factor(value))) +
  scale_fill_discrete("Key")