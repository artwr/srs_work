install.packages(c("sp", "spgrass6", "maps", "mapdata", "MCMCpack", "zoo", "xts", "spatstat", "spatial"))
install.packages("bootstrap")
library(xts)
install.packages(c("ggplot", "reshape"))
install.packages("RPyGeo")
install.packages("MCMCpack")
install.packages("scatterplot3d")
install.packages("mvtnorm")
install.packages("fields")
setwd("G:/Laptop_data_20110217/Cours/courses/STAT200B/code")
ls()
ls
ls(cwd)
aquifer <- read.table("aquifer.dat", skip = 1, header = FALSE)
names(aquifer) <- c("x", "y", "z")
library(scatterplot3d) # Add-on package with 3d plotting functions
scatterplot3d(aquifer, type = "h",
xlab = "Easting (mi)", ylab = "Northing (mi)",
zlab = "Piezometric head (1000s ft)",
main = "Wolfcamp Aquifer Data")
dev.print(pdf, file = "aquiferpoints.pdf", height = 8, width = 8)
library(mvtnorm) # Add-on package with multivariate normal and t dists
nll <- function(param, Xmat, d, z, verbose = FALSE){
beta <- param[1:3]; sigma2 <- param[4]; rho <- param[5]
mu <- Xmat %*% beta
Sigma <- sigma2 * exp(-d/rho)
ll <- dmvnorm(z, mean = mu, sigma = Sigma, log = TRUE)
if (verbose) print(c(param, -ll))
return(-ll)
}
Xmat <- cbind(1, aquifer$x, aquifer$y)
View(Xmat)
d <- as.matrix(dist(aquifer[,1:2]))
beta.ols <- lm(aquifer$z~Xmat-1)$coef # OLS regression; assuming iid errors
names(beta.ols) <- NULL
start <- c(beta0 = beta.ols[1], beta1 = beta.ols[2], beta2 = beta.ols[3],
sigma2 = 1, rho = 1)
eps <- 1e-10
op <- optim(start, fn = nll, method = "L-BFGS-B",
lower = c(-Inf, -Inf, -Inf, eps, eps),
hessian = TRUE, # Approximate matrix of 2nd derivs
Xmat = Xmat, d = d, z = aquifer$z, verbose = TRUE)
op # This is a list; access elements using $
mle <- op$par
J <- solve(op$hessian)
se.hat <- sqrt(diag(J))
par(mfrow = c(3, 2))
sapply(1:length(mle), function(i){
values <- matrix(rep(mle, 100), nrow = 100, byrow = TRUE)
values[,i] <- seq(mle[i]/2, mle[i]*2, length = 100)
nll.seq <- apply(values, 1, nll, Xmat = Xmat, d = d, z = aquifer$z)
plot(seq(mle[i]/2, mle[i]*2, length = 100), nll.seq, type = "l", xlab = names(mle)[i])
})
lower <- mle - 2 * se.hat
upper <- mle + 2 * se.hat
gridn <- 50
xseq <- seq(-150, 150, length = gridn)
yseq <- seq(0, 200, length = gridn)
krige.grid <- expand.grid(x = xseq, y = yseq)
dpred <- as.matrix(dist(rbind(aquifer[,1:2], krige.grid)))[1:85,-(1:85)]
Xpred <- cbind(1, krige.grid$x, krige.grid$y)
g <- exp(-dpred/mle["rho"])
G <- exp(-d/mle["rho"])
zpred <- Xpred %*% mle[1:3] + t(g) %*% solve(G, aquifer$z - Xmat %*% mle[1:3])
library(fields) # Add-on package for spatial stats
zmat <- matrix(zpred, nrow = 50)
ok <- in.poly(krige.grid, aquifer[,1:2], convex.hull = TRUE)
zmat[!ok] <- NA
drape.plot(x = xseq, y = yseq, z = zmat,
theta = 120, phi = 30, # Viewing angles
col = terrain.colors(60), horiz = FALSE)
plot(x = xseq, y = yseq, z = zmat,
theta = 120, phi = 30, # Viewing angles
col = terrain.colors(60), horiz = FALSE)
scatterplot3d(x = xseq, y = yseq, z = zmat,
theta = 120, phi = 30, # Viewing angles
col = terrain.colors(60), horiz = FALSE)
drape.plot(x = xseq, y = yseq, z = zmat,
theta = 120, phi = 30, # Viewing angles
col = terrain.colors(60), horiz = FALSE)
zmat[!ok] <- NA
drape.plot(x = xseq, y = yseq, z = zmat,
theta = 120, phi = 30, # Viewing angles
drape.plot(x = xseq, y = yseq, z = zmat, theta = 120, phi = 30, col = terrain.colors(60), horiz = FALSE)
)
n <- 100; alpha <- 3; beta <- 4
x <- rbeta(100, alpha, beta)
hist(x)
nll <- function(par, x, verbose = FALSE){
alpha <- par[1]; beta <- par[2] # unpack
ll <- sum(dbeta(x, alpha, beta, log = TRUE))
if (verbose) print(c(par, -ll))
return(-ll)
}
start <- c(alpha = 1, beta = 1) # starting values
eps <- 1e-10 # small value for lower bounds
op <- optim(par = start, fn = nll,
lower = rep(eps, 2),
x = x, verbose = TRUE)
op # This is a list; extract elements using $
mle <- op$par
alpha.test <- cbind(seq(mle[1]/2, mle[1]*2, length = 100), mle[2])
nll.alpha <- apply(alpha.test, 1, nll, x = x, verbose = FALSE)
plot(alpha.test[,1], nll.alpha, type = "l")
beta.test <- cbind(mle[1], seq(mle[2]/2, mle[2]*2, length = 100))
nll.beta <- apply(beta.test, 1, nll, x = x, verbose = FALSE)
plot(beta.test[,2], nll.beta, type = "l")
setwd("D:/data1")
te<-read.table("tritium_error.txt", header=TRUE ,sep="\t")
View(te)
require(ggplot2)
install.packages("MASS")
install.packages("ggplot2")
te<-read.table("tritium_error.txt", header=TRUE ,sep="\t")
require(ggplot2)
qplot(log(resval),errval, data = te)
qplot(resval,errval, data = te)
rm(list=ls())
te<-read.table("tritium_error.txt", header=TRUE ,sep="\t", colClasses = c(NULL,numeric,NULL,numeric,date,numeric))
te<-read.table("tritium_error.txt", header=TRUE ,sep="\t", colClasses = c(NULL,"numeric",NULL,"numeric","date","numeric"))
te<-read.table("tritium_error.txt", header=TRUE ,sep="\t", colClasses = c(NULL,"numeric",NULL,"numeric",NULL,"numeric"))
te<-read.table("tritium_error.txt", header=TRUE ,sep="\t", colClasses = c("NULL","numeric","NULL","numeric","NULL","numeric"))
rm(list=ls())
te<-read.table("tritium_error.txt", header=TRUE ,sep="\t", colClasses = c("NULL","numeric","NULL","numeric","NULL","numeric"))
te<-read.table("tritium_error.txt", header=TRUE, row.names=NULL ,sep="\t", colClasses = c("NULL","numeric","NULL","numeric","NULL","numeric"))
te<-read.table("tritium_error.txt", header=TRUE, row.names=NULL ,sep="\t", colClasses = c(NA,"numeric",NA,"numeric",NA,"numeric"))
te<-read.table("tritium_error.txt", header=TRUE, row.names=NULL ,sep="\t", colClasses = c("numeric",NA,"numeric",NA,"numeric"))
te<-read.table("tritium_error.txt", header=TRUE, row.names=NULL ,sep="\t", colClasses = c(NA,NA,"numeric",NA,"numeric",NA,"numeric"))
te<-read.table("tritium_error.txt", header=TRUE, row.names=NULL ,sep="\t", colClasses = c("NULL","NULL","numeric","NULL","numeric","NULL","numeric"))
te<-read.table("tritium_error.txt", header=TRUE, row.names=NULL ,sep="\t", colClasses = c("NULL","numeric","NULL","numeric","NULL","numeric"))
te<-read.table("tritium_error.txt", header=TRUE, sep="")
View(te)
is.numeric(te$resval)
as.numeric(te$resval)
te$tri<-as.numeric(te$resval);
te$err<-as.numeric(te$errval);
te&yearm<-as.numeric(te$yearm);
te$yearm<-as.numeric(te$yearm);
qplot(tri,err, data = te)
qplot(tri,err, data = te, color=yearm)
qplot(log(tri),log(err), data = te, color=yearm)
qplot(tri,err, data = te, color=yearm)
qplot(tri(yearm==1990),err(yearm==1990), data = te)
qplot(tri[yearm==1990],err[yearm==1990], data = te)
qplot(tri[yearm==1990],err[yearm==1990])
qplot(te$tri[yearm==1990],te$err[yearm==1990])
qplot(te$tri[te$yearm==1990],te$err[te$yearm==1990])
qplot(log(te$tri[te$yearm==1990]),log(te$err[te$yearm==1990]))
ylabel("Tritium Error")
xlabel("Tritium Concentration (pCi/L)")
qplot(log(te$tri[te$yearm==1990]),log(te$err[te$yearm==1990]),ylab="Tritium Error",xlab="Tritium Concentration (pCi/L)")
qplot(log(te$tri[te$yearm==1990]),log(te$err[te$yearm==1990]),ylab="log(Tritium Error)",xlab="log(Tritium Concentration (pCi/L))")
length(te$tri[te$yearm==1990])
