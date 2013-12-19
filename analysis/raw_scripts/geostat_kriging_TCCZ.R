## geostat model of the TCCZ
require(gstat)
require(geoR)
require(geoRglm)
#


# for (i in 1:6) {
#    fit <- lm(y ~ poly(x, degree = i))
#    r[i] <- (summary(fit))$r.squared
#    ar[i] <- (summary(fit))$adj.r.squared
#    }
lm3_3 <- lm(TCCZ_top ~ EASTING + I(EASTING^2) + I(EASTING^3) + NORTHING + I(NORTHING^2) + I(NORTHING^3), data = TCCZe)


#Import picks for the TCCZ
TCCZe<-readRDS("./geo_data/processed/TCCZ_wtoppick.rdata")

## gstat
## data(meuse)
hist(TCCZe$TCCZ_top, breaks = 25)
TCCZe$TCCZ_top.l <- log10(TCCZe$TCCZ_top)
TCCZe$g_elev_ft.l <- log10(TCCZe$g_elev_ft)
hist(TCCZe$TCCZ_top.l, breaks = 25)

#Bivariate
with(TCCZe, plot(TCCZ_top ~ g_elev_ft))
with(TCCZe, plot(TCCZ_top.l ~ g_elev_ft.l))

TCCZsp <- TCCZe
coordinates(TCCZsp) <- c("EASTING","NORTHING")

plot(TCCZsp, asp = 1, pch = 1)
plot(TCCZsp, asp = 1, cex = 4 * (TCCZsp$TCCZ_top - min(TCCZsp$TCCZ_top))/(max(TCCZsp$TCCZ_top) - min(TCCZsp$TCCZ_top)), pch = 1)

v.l <- variogram(TCCZ_top.l ~ 1, TCCZsp, cutoff = 600, width = 30)
print(plot(v.l,plot.numbers = T))


v <- variogram(TCCZ_top ~ 1, TCCZsp, cutoff = 750, width = 30)
print(plot(v,plot.numbers = T))

# variogram Models
print(show.vgms())

#Pick a variogram
vm.sph <- vgm(psill = 55, model = "Sph", range = 600, nugget = 25)
print(plot(v, pl = T, model = vm.sph))
vmsphf <- fit.variogram(v, vm.sph)
print(plot(v, pl = T, model = vmsphf))

vm.exp <- vgm(psill = 70, model = "Exp", range = 600, nugget = 25)
vm.exc <- vgm(psill = 55, model = "Exc", range = 600, nugget = 25)
print(plot(v, pl = T, model = vm.exp))
print(plot(v, pl = T, model = vm.exc))
vmexpf <- fit.variogram(v, vm.exp)
vmexcf <- fit.variogram(v, vm.exc)
print(plot(v, pl = T, model = vmexpf))
print(plot(v, pl = T, model = vmexcf))

#Interpolation grid as sp object
interpolation.grid.sp <- interpolation.grid
coordinates(interpolation.grid.sp) <- c("EASTING","NORTHING")
gridded(interpolation.grid.sp) <- TRUE

#kriging
TCCZk <- krige(TCCZ_top ~ 1, locations = TCCZsp, newdata = interpolation.grid.sp, model = vmexpf)
# Plot the predicted values
print(spplot(TCCZk, "var1.pred", asp=1, col.regions=bpy.colors(64),  main="OK prediction, TCCZ elevation (ft)"))
# Plot the variance
print(spplot(TCCZk, "var1.var", asp=1, col.regions=bpy.colors(64),  main="OK prediction, TCCZ elevation variance(ft^2)"))
print(spplot(TCCZk, "var1.var", asp=1,  main="OK prediction, TCCZ elevation variance(ft^2)"))

#readRDS("./geo_data/processed/basins27poly.rdata")

v1 <- variogram(TCCZ_top ~ EASTING + I(EASTING^2) + I(EASTING^3) + NORTHING + I(NORTHING^2) + I(NORTHING^3), TCCZsp, cutoff = 750, width = 30)
print(plot(v1,plot.numbers = T))

vm1.sph <- vgm(psill = 30, model = "Sph", range = 50, nugget = 9)
print(plot(v1, pl = T, model = vm1.sph))
vm1sphf <- fit.variogram(v1, vm1.sph)
summary(vm1sphf)
print(plot(v1, pl = T, model = vm1sphf))

vm1.exp <- vgm(psill = 30, model = "Exp", range = 25, nugget = 5)
vm1.exc <- vgm(psill = 36, model = "Exc", range = 50, nugget = 5)
print(plot(v1, pl = T, model = vm1.exp))
print(plot(v1, pl = T, model = vm1.exc))
vm1expf <- fit.variogram(v1, vm1.exp)
vm1excf <- fit.variogram(v1, vm1.exc)
print(plot(v1, pl = T, model = vm1expf))
print(plot(v1, pl = T, model = vm1excf))
summary(vm1expf)
summary(vm1excf)

TCCZk1 <- krige(TCCZ_top ~ EASTING + NORTHING + EASTING*NORTHING, 
                locations = TCCZsp, newdata = interpolation.grid.sp, model = vm1expf)
TCCZk2 <- krige(TCCZ_top ~ EASTING + I(EASTING^2) + NORTHING + I(NORTHING^2), 
               locations = TCCZsp, newdata = interpolation.grid.sp, model = vm1expf)

# Plot the predicted values
print(spplot(TCCZk1, "var1.pred", asp=1, col.regions=bpy.colors(64),  main="OK prediction, TCCZ elevation (ft)"))
# Plot the variance
# print(spplot(TCCZk1, "var1.var", asp=1, col.regions=bpy.colors(64),  main="OK prediction, TCCZ elevation variance(ft^2)"))
print(spplot(TCCZk1, "var1.var", asp=1,  main="OK prediction, TCCZ elevation variance(ft^2)"))



## geoR
