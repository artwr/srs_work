#remove_neg_results

# Load the cleaned up version of the tables
results <- readRDS("./srs_data/processed/results.rdata")
resultsC <- readRDS("./srs_data/processed/resultsC.rdata")

# 

#Remove FEX8 because of the double screen
results.clean1<-results[!results$STATION_ID=='FEX  8',]
results.clean1<-results.clean1[!results.clean1$STATION_ID=='FEX  9',]

# resultsC.clean1<-resultsC[!resultsC$STATION_ID=='FEX  8',]
# resultsC.clean1<-resultsC.clean1[!resultsC.clean1$STATION_ID=='FEX  9',]
resultsC.clean1<-resultsC
#Store FEX8 separate;y for further inspection
# tritium.FEX8<-tritium.raw[tritium.raw$STATION_ID=='FEX  8',]
# tritiumC.FEX8<-tritiumC.raw[tritiumC.raw$STATION_ID=='FEX  8',]
#Visual inspection of the data
#tritium.fex8<-tritium.raw[tritium.raw$STATION_ID=='FEX  8',]


#Replace Negative values with NA
results.clean1[results.clean1$RESULT<0,]$RESULT<-NA
resultsC.clean1[resultsC.clean1$RESULT<0,]$RESULT<-NA
#tritium.see<-tritium.raw[tritium.raw$RESULT<0,]
#Remove NA values since I cannot use them
results.clean<-results.clean1[!is.na(results.clean1$RESULT),]
resultsC.clean<-resultsC.clean1[!is.na(resultsC.clean1$RESULT),]

#

saveRDS(results.clean, "./srs_data/processed/resultsnonegnona.rdata")
saveRDS(resultsC.clean, "./srs_data/processed/resultsCnonegnona.rdata")

rm(results)
rm(results.clean1)
rm(results.clean)
rm(resultsC)
rm(resultsC.clean1)
rm(resultsC.clean)

message("Negative and NA values removed")

