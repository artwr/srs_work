# convert data to timeseries
require(Hmisc)
require(plyr)
require(zoo)
require(forecast)
graphpath = "figures/exploratory/"
#


# Import data
tritiumall <- readRDS("srs_data/processed/byanalyte/results_tritium.rdata")
tritiumallp1988 <- droplevels(tritiumall[tritiumall$ZYEAR>1988])

tritiumzoo <- dlply(tritiumall, .(STATION_ID), .fun = function(zz){zoo(x = zz$RESULT, order.by = zz$ZDATE)})
tritiumzoop1988 <- dlply(tritiumallp1988, .(STATION_ID), .fun = function(zz){zoo(x = zz$RESULT, order.by = zz$ZDATE)})
# Testing
# names(tritiumzoo)
# plot(tritiumzoo[["BRR  1D"]], xlab = "Year", type = "p", pch = "+")


# Creating plots for all ts
pdf(file = paste0(graphpath,"tritium_ts_for_D_wells.pdf"), onefile = TRUE, height = 4, title = "Tritium time series for all D wells")
for (i in seq_along(names(tritiumzoo))) {
  plot(tritiumzoo[[names(tritiumzoo)[i]]], xlab = "Date", ylab = "Tritium (pCi/L)", main = names(tritiumzoo)[i], type = "p", pch = "+", xaxt = "n")
  tt <- time(tritiumzoo[[names(tritiumzoo)[i]]])
  axis.Date(1, x = tt, format= "%Y-%m")
}
dev.off()
# The data post 1988
pdf(file = paste0(graphpath,"tritium_ts_p1988_for_D_wells.pdf"), onefile = TRUE, height = 4, title = "Tritium time series starting 1988 for all D wells")
for (i in seq_along(names(tritiumzoop1988))) {
  plot(tritiumzoop1988[[names(tritiumzoop1988)[i]]], xlab = "Date", ylab = "Tritium (pCi/L)", main = names(tritiumzoop1988)[i], type = "p", pch = "+", xaxt = "n")
  tt <- time(tritiumzoop1988[[names(tritiumzoop1988)[i]]])
  axis.Date(1, x = tt, format= "%Y-%m")
}
dev.off()




# Dataset analysis
tritiumzool <- ldply(tritiumzoo, .fun = length)
tritiumzoop1988l <- ldply(tritiumzoop1988, .fun = length)
names(tritiumzool)[2] <- "data.count"
names(tritiumzoop1988l)[2] <- "data.count"

welldatapoints <- hist(tritiumzool$data.count, breaks = c(1,5, seq(10,100, by = 10), 125, 150, 200, 250), freq = TRUE)
welldatapoints2 <- hist(tritiumzool$data.count, breaks = Hmisc::cut2(tritiumzool$data.count, g = 10, all.inside = TRUE, onlycuts = TRUE), freq = TRUE)
barplot(welldatapoints2$counts, names.arg = levels(Hmisc::cut2(tritiumzool$data.count, g = 10, all)), , cex.names=0.8 )

Fn <- ecdf(tritiumzool$data.count)
plot.ecdf(Fn)
plot(Fn, verticals = TRUE, col.points = "blue", col.hor = "red", col.vert = "bisque")


cbind(tritiumzool$data.count, findInterval(tritiumzool$data.count, Hmisc::cut2(tritiumzool$data.count, g = 20, onlycuts = TRUE)))
  
?ecdf

#

length(lapply(tritiumzoop1988, "[", c("FBI  5D","FBI  6D", "FPZ  1A", "FPZ  2A", "FPZ  4A", "FSB 79", "FSB 91D", "FSB 93D", "FSB 95DR")))

length(tritiumzoop1988[c("FBI  5D","FBI  6D", "FPZ  1A", "FPZ  2A", "FPZ  4A", "FSB 79", "FSB 91D", "FSB 93D", "FSB 95DR", "FSB109D", "FSB110D", "FSB114D")])

testtsl <- tritiumzoop1988[c("FBI  5D","FBI  6D", "FPZ  1A", "FPZ  2A", "FPZ  4A", "FSB 78","FSB 79", "FSB 91D", "FSB 93D", "FSB 95DR", "FSB109D", "FSB110D", "FSB114D")]

loess

# test.fit <- auto.arima(testtsl[["FSB 78"]], seasonal=FALSE)

# rm(i)
# rm(list = ls())
