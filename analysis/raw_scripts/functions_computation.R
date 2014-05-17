# Computation Functions
require(data.table)
require(plyr)
require(cvTools)
# require(multicore)
# require(doMC)
# registerDoMC(cores = 4)
# require(parallel)
# require(doParallel)
# registerDoParallel(cl, cores=4)
# require(foreach)

# getAnalyteResults <- function(analyte_name, results.df) {
#   require(data.table)
# #   if(!exists("results")) {results <- readRDS("srs_data/processed/resultsnonegnona.rdata")}
# #   if(!exists("resultsC")) {resultsC <- readRDS("srs_data/processed/resultsCnonegnona.rdata")}
#   resultsDT <- as.data.table(results.df)
#   setkey(resultsDT,ANALYTE_NAME)
#   <-resultsDT[paste()]
# }

aggregateAnalyteByYear <- function(analyte.df){
  analyte.aggreg<-ddply(analyte.df, c('STATION_SEQ','STATION_ID','ZYEAR','EASTING','NORTHING'), function(x) c(count=nrow(x),mean=mean(x$RESULT),median=median(x$RESULT),sd=sd(x$RESULT),mad=mad(x$RESULT),min=min(x$RESULT),max=max(x$RESULT)))
}

# function to compute loess models with a range of alphas
# It uses the parameters I need in most of my cases

loessAlphaVect <- function(lformula, ldata, ldegree, spanvector = c(.25,.5,.75), lfamily = c("gaussian") ,lcontrol) {
  loess.models <-list() 
  for (k in 1:length(spanvector)) { 
    loess.models[paste0("alpha",spanvector[k])] <- list(loess(as.formula(lformula), data = ldata, degree = ldegree, span = spanvector[k], family = lfamily, normalize = FALSE, method = c("loess"), control = lcontrol))
    #     loess.models <- c(loess.models, list(loess(as.formula(lformula), data = ldata, degree = ldegree, span = spanvector[k], normalize = FALSE, method = c("loess"), control = lcontrol)))
  }
  return(loess.models)
}


## loess alpha cross validation








