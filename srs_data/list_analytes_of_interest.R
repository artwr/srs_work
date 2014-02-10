
require(data.table)

results <- readRDS("./srs_data/processed/results.rdata")
resultsC <- readRDS("./srs_data/processed/resultsC.rdata")

# Aggregation with data frame (slow)
# results.counts <- aggregate(RESULT~ANALYTE_NAME, data = results, FUN = "length")
# names(results.counts)[2] <- "COUNT"

# Using data.table
results_dt <- data.table(results)
results.counts <- results_dt[,list(COUNT = length(RESULT)),by=list(ANALYTE_NAME)]
results.counts <- results.counts[order(rank(ANALYTE_NAME))]

resultsC_dt <- data.table(resultsC)
resultsC.counts <- resultsC_dt[,list(COUNT = length(RESULT)),by=list(ANALYTE_NAME)]
resultsC.counts <- resultsC.counts[order(rank(ANALYTE_NAME))]

write.table(results.counts, file = "./srs_data/processed/analytecounts.txt", sep = "|", row.names = FALSE)
write.table(resultsC.counts, file = "./srs_data/processed/analytecountsC.txt", sep = "|", row.names = FALSE)
