# create subsets by analyte

results <- readRDS("./srs_data/processed/results.rdata")
resultsC <- readRDS("./srs_data/processed/resultsC.rdata")

resultsDT <- as.data.table(results)
resultsCDT <- as.data.table(resultsC)

# Create the analytes subfiles for faster analysis
setkey(resultsDT,ANALYTE_NAME)
setkey(resultsCDT,ANALYTE_NAME)
# anames <- unique(resultsDT$ANALYTE_NAME)
# tables()

# List of analytes of interest first pass
varofinterest1 <- c("TRITIUM", "NITRATE-NITRITE AS NITROGEN", "IODINE-129", "CESIUM-137", "TECHNETIUM-99", "STRONTIUM-90")

# List of analytes of interest second pass
varofinterest2 <- c("TRITIUM", "NITRATE-NITRITE AS NITROGEN", "SPECIFIC CONDUCTANCE", "IODINE-129", "URANIUM-235", "URANIUM-238", "CESIUM-137", "TECHNETIUM-99", "STRONTIUM-90", "TOTAL ACTIVITY", "PH")

# List of analytes of interest third pass
varofinterest3 <- c("TRITIUM", "NONVOLATILE BETA", "GROSS ALPHA", "NITRATE-NITRITE AS NITROGEN", "SPECIFIC CONDUCTANCE", "IODINE-129", "URANIUM-235", "URANIUM-238", "CESIUM-137", "TECHNETIUM-99", "STRONTIUM-90", "TOTAL ACTIVITY", "PH")


filenamesdf <- data.frame(ANALYTE_NAME = c("TRITIUM", "NONVOLATILE BETA", "GROSS ALPHA", "NITRATE-NITRITE AS NITROGEN", "SPECIFIC CONDUCTANCE", "IODINE-129", "URANIUM-235", "URANIUM-238", "CESIUM-137", "TECHNETIUM-99", "STRONTIUM-90", "TOTAL ACTIVITY", "PH"), filetag = c("tritium", "nonvolbeta", "grossalpha", "nitrate", "conductivity", "iodine129", "uranium235", "uranium238", "cesium137", "technetium99", "strontium90", "totalactivity", "pH"))

resultsofinterest1 <- resultsDT[J(varofinterest1),]
resultsofinterest2 <- resultsDT[J(varofinterest2),]
resultsofinterest3 <- resultsDT[J(varofinterest3),]
saveRDS(resultsofinterest1,"srs_data/processed/resultsofinterest1.rdata")
saveRDS(resultsofinterest2,"srs_data/processed/resultsofinterest2.rdata")
saveRDS(resultsofinterest3,"srs_data/processed/resultsofinterest3.rdata")

resultsCofinterest1 <- resultsCDT[J(varofinterest1),]
resultsCofinterest2 <- resultsCDT[J(varofinterest2),]
resultsCofinterest3 <- resultsCDT[J(varofinterest3),]
saveRDS(resultsCofinterest1,"srs_data/processed/resultsCofinterest1.rdata")
saveRDS(resultsCofinterest2,"srs_data/processed/resultsCofinterest2.rdata")
saveRDS(resultsCofinterest3,"srs_data/processed/resultsCofinterest3.rdata")



for(i in 1:length(varofinterest3)) {
  resultsa <- resultsDT[J(varofinterest3[i])]
  resultsCa <- resultsCDT[J(varofinterest3[i])]
  saveRDS(resultsa,paste0("srs_data/processed/byanalyte/results_", filenamesdf$filetag[i],".rdata"))
  saveRDS(resultsCa,paste0("srs_data/processed/byanalyte/resultsC_", filenamesdf$filetag[i],".rdata"))
}

rm(list=ls())