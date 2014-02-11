
require(data.table)

results1 <- readRDS("./srs_data/processed/results.rdata")
resultsC1 <- readRDS("./srs_data/processed/resultsC.rdata")

results2 <- readRDS("srs_data/processed/resultsnonegnona.rdata")
resultsC2 <- readRDS("srs_data/processed/resultsCnonegnona.rdata")

# Aggregation with data frame (slow)
# results.counts <- aggregate(RESULT~ANALYTE_NAME, data = results1, FUN = "length")
# names(results.counts)[2] <- "COUNT"

# Using data.table
results_dt <- data.table(results1)
results.counts <- results_dt[,list(COUNT = length(RESULT)),by=list(ANALYTE_NAME)]
results.counts <- results.counts[order(rank(ANALYTE_NAME))]

resultsC_dt <- data.table(resultsC1)
resultsC.counts <- resultsC_dt[,list(COUNT = length(RESULT)),by=list(ANALYTE_NAME)]
resultsC.counts <- resultsC.counts[order(rank(ANALYTE_NAME))]

# Without NA or Negative values
resultsDT2 <- as.data.table(results2)
resultsCDT2 <- as.data.table(resultsC2)
setkey(resultsDT2,ANALYTE_NAME)
setkey(resultsCDT2,ANALYTE_NAME)

results.counts2 <- resultsDT2[,list(COUNT = length(RESULT)),by=list(ANALYTE_NAME)]
results.counts2 <- results.counts2[order(rank(ANALYTE_NAME))]
resultsC.counts2 <- resultsCDT2[,list(COUNT = length(RESULT)),by=list(ANALYTE_NAME)]
resultsC.counts2 <- resultsC.counts2[order(rank(ANALYTE_NAME))]

beforeafter <- merge(results.counts, results.counts2, by = "ANALYTE_NAME")
beforeafter$COUNT.NA <- beforeafter$COUNT.x - beforeafter$COUNT.y
setnames(beforeafter, c("ANALYTE_NAME", "COUNT.RAW", "COUNT.CLEAN", "COUNT.DIFF"))
beforeafterC <- merge(resultsC.counts, resultsC.counts2, by = "ANALYTE_NAME")
beforeafterC$COUNT.NA <- beforeafterC$COUNT.x - beforeafterC$COUNT.y
setnames(beforeafterC, c("ANALYTE_NAME", "COUNT.RAW", "COUNT.CLEAN", "COUNT.DIFF"))

saveRDS(beforeafter,"srs_data/processed/results_counts.rdata")
saveRDS(beforeafterC,"srs_data/processed/resultsC_counts.rdata")

write.table(results.counts, file = "./srs_data/processed/analytecountsraw.txt", sep = "|", row.names = FALSE)
write.table(resultsC.counts, file = "./srs_data/processed/analytecountsCraw.txt", sep = "|", row.names = FALSE)

write.table(results.counts2, file = "./srs_data/processed/analytecountsclean.txt", sep = "|", row.names = FALSE)
write.table(resultsC.counts2, file = "./srs_data/processed/analytecountsCclean.txt", sep = "|", row.names = FALSE)

rm(list=ls())
