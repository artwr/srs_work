x <- seq(-10, 10, length.out = 50)  
y <- x  
rotsinc <- function(x,y) {
  sinc <- function(x) { y <- sin(x)/x ; y[is.na(y)] <- 1; y }  
  10 * sinc( sqrt(x^2+y^2) )  
}

z <- outer(x, y, rotsinc)  
persp(x, y, z)

require(rgl)  
surface3d(x, y, z)