# outliers_detection_using_loess
require(plyr)
require(lubridate)
require(ggplot2)
graphpath = "figures/exploratory/"

# Import data
tritiumall <- readRDS("srs_data/processed/byanalyte/results_tritium.rdata")

# Select the data post 1988 and drop unused levels
tritiumallp1988 <- droplevels(tritiumall[tritiumall$ZYEAR>1988,])

# Compute a decimal date for the regression
tritiumallp1988$DDATE <- decimal_date(tritiumallp1988$ZDATE)
tritiumallp1988$DDATEp1988 <- tritiumallp1988$DDATE - 1988
tritiumallp1988$logT <- log(tritiumallp1988$RESULT)

# Define the parameters for the call to loess
lcontrold<-loess.control(surface = c("direct"),
                         statistics = c("exact"),
                         trace.hat = c("exact"),
                         cell = 0.2, iterations = 5)

# Define a function to call loess with a set number of points used in the smoothing, instead of a set proportion of points
tsloess <- function(lformula, ldata, ldegree = 1, kpoints = 5, lfamily = c("gaussian") ,lcontrol = loess.control(surface = c("direct"), statistics = c("exact"), trace.hat = c("exact"), cell = 0.2, iterations = 5)) {
  ndata <- nrow(ldata)
  lspan <- kpoints / ndata
  tsloessmod <- loess(as.formula(lformula), data = ldata, degree = ldegree, span = lspan, family = lfamily, normalize = TRUE, method = c("loess"), control = lcontrol)
  return(tsloessmod)
}


tritiumwellsp1988 <- split(tritiumallp1988, tritiumallp1988$STATION_ID)
testsample <- tritiumwellsp1988[c("FBI  5D","FBI  6D", "FPZ  1A", "FPZ  2A", "FPZ  4A", "FSB 78","FSB 79", "FSB 91D", "FSB 93D", "FSB 95DR", "FSB109D", "FSB110D", "FSB114D")]

ggFBI5D <- ggplot(data = testsample[["FBI  5D"]], aes(x = DDATE, y = RESULT))
ggFBI5D <- ggFBI5D + geom_point()
ggFBI5D <- ggFBI5D + theme_bw()
print(ggFBI5D)

p <- ggplot(data = testsample[["FSB 79"]], aes(x = DDATE, y = RESULT))
p <- p + geom_point()
p <- p + theme_bw()
print(p)

p2 <- ggplot(data = testsample[["FSB109D"]], aes(x = DDATE, y = RESULT))
p2 <- p2 + geom_point()
p2 <- p2 + theme_bw()
print(p2)

p3 <- ggplot(data = testsample[["FSB114D"]], aes(x = DDATE, y = RESULT))
p3 <- p3 + geom_point()
p3 <- p3 + theme_bw()
p3 <- p3 + geom_line(data=cbind())
print(p3)

residuals(tsloessmodellist[["FSB114D"]])

tsloessmodellist <- llply(testsample, .fun = function(zz){tsloess("RESULT~DDATE", ldata = zz, ldegree = 1, lcontrol = lcontrold )})
tsloesspredict <- llply(tsloessmodellist, .fun = predict)


#pdf(file = paste0(graphpath,"tritium_loess_ts_p1988_for_D_wells.pdf"), onefile = TRUE, height = 4, title = "Tritium time series starting 1988 for all D wells")
for (i in seq_along(names(tsloessmodellist))) {
  plot(tsloessmodellist[[names(tsloessmodellist)[i]]]$, xlab = "Date", ylab = "Tritium (pCi/L)", main = names(tritiumzoop1988)[i], type = "p", pch = "+", xaxt = "n")
  tt <- time(tritiumzoop1988[[names(tritiumzoop1988)[i]]])
  axis.Date(1, x = tt, format= "%Y-%m")
}
#dev.off()
