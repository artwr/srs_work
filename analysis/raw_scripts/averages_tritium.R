# Zero-th order / 0.5-th order

# require(dplyr)
require(plyr)
# require(boot)

# Load the cleaned up version of the tables
results <- readRDS("./srs_data/processed/resultsnonegnona.rdata")
resultsC <- readRDS("./srs_data/processed/resultsCnonegnona.rdata")

# resultstbl <- as.tbl(results)
# resultsCtbl <- as.tbl(resultsC)
# 
# by.analyte.year <- group_by(resultstbl, MYEAR, ANALYTE_NAME)
# result.by.analyte.year <- summarise(by.analyte.year, datapoint = n(), mean.result = mean(RESULT))

# B <- 10000
mean.by.analyte.by.year<-ddply(results, .(MYEAR, ANALYTE_NAME) , summarise, count = length(RESULT), mean = mean(RESULT), median = median(RESULT), sd = sd(RESULT), var = var(RESULT))
# X<-daply(results, .(MYEAR, ANALYTE_NAME) ,)
# ?sample
# sample(x, , replace = FALSE, prob = NULL)

mean.fun2 <- function(data, ind) {meanres <- mean(data$RESULT[ind], na.rm = TRUE)}
# Bootstrap 95% CI Inventory

# function to obtain R-Squared from the data
list.bootstrapobj<-dlply(results, .(MYEAR, ANALYTE_NAME), function(ays) {boot(data=ays, statistic=mean.fun2, R = 1000)})
# bootstrapping with 1000 replications
# results <- boot(data=mtcars, statistic=rsq,
#                 R=1000, formula=mpg~wt+disp)



tritium1999<-filter(results, ANALYTE_NAME == "TRITIUM", MYEAR == "1999")



