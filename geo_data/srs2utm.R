#Coords Conversion SRS to UTM 27

# utm1 <- c(437504.53, 3681779.12)
# srs1 <- c(52773.20, 73738.20)
# utm2 <- c(436555.12, 3681710.48)
# srs2 <- c(50121.60, 75389.80)
# utm3 <- c(436871.42, 3681290.47)
# srs3 <- c(50149.60, 73664.50)
#   
# srsc <- rbind(srs1,srs2,srs3)
# utmc <- rbind(utm1, utm2, utm3)
  
  
findTransformationMatrix <- function(srscoords, utm27coords) {
  x <- srscoords[,1]
  y <- srscoords[,2]
  xu <- utm27coords[,1]
  yu <- utm27coords[,2]
  AA <- matrix(data = c(x[1],y[1],1,x[2],y[2],1,x[3],y[3],1), nrow = 3, ncol = 3, byrow =TRUE)
  cx <- solve(AA, xu)
  cy <- solve(AA, yu)
  # list(cx, cy)
  as.matrix(cbind(cx,cy))
}

# Mtransform <- findTransformationMatrix(srsc, utmc)

srs2utm27 <- function(srscoords, Mtransform) {
  n<-dim(srscoords)[1]
  X <- cbind(srscoords, rep(1,n))
  utm27coords <- X %*% Mtransform
}