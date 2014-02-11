## geostat model of the TCCZ
require(sp)
require(gstat)
require(nlme)
require(classInt)
require(fields)


TCCZ.pred <- readRDS("analysis/processed_data/TCCZloesspred.rdata")
TCCZpredsp <- TCCZ.pred
coordinates(TCCZpredsp) <- c("EASTING","NORTHING")
proj4string(TCCZpredsp) <- CRS("+proj=utm +zone=17 +datum=NAD27")

# Plots
# TCCZ.pred$ehat.loess1 
# TCCZ.pred$ehat.loess1b
# TCCZ.pred$ehat.lm
range(TCCZ.pred$ehat.l1)
range(TCCZ.pred$ehat.l1b)
range(TCCZ.pred$ehat.lm)
spplot(TCCZpredsp, c("ehat.l1"), aspect = "iso")
spplot(TCCZpredsp, c("ehat.l1b"), aspect = "iso")
spplot(TCCZpredsp, c("ehat.lm"), aspect = "iso")

# Colors
pal <- tim.colors(4)

# plot fitted/predicted values from lm regression
fj5 <- classIntervals(TCCZ.pred$ehat.l1, n = 5, style = "fisher")
fj5col <- findColours(fj5, pal)
plot(TCCZpredsp, col = fj5col, pch = 19)
points(TCCZpredsp, pch=1)
legend("topleft", fill = attr(fj5col, "palette"),
       legend = names(attr(fj5col, "table")), bty = "n")



plot(TCCZpredsp, asp = 1, pch = 1)
plot(TCCZpredsp, asp = 1, cex = 4 * (TCCZpredsp$TCCZ_top - min(TCCZpredsp$TCCZ_top))/(max(TCCZpredsp$TCCZ_top) - min(TCCZpredsp$TCCZ_top)), pch = 1)
