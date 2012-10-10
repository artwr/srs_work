#Clean_data
setwd("D:/SRS/data_SRS/TCCZ_krig/R_work - Copy")

TCCZdatac<-TCCZdata
##Convert non meaningful zeros to NAs
TCCZdatac[TCCZdatac==0]<-NA

##Convert to a spatial object
saveRDS(TCCZdatac, file = "TCCZ_o.rdata")