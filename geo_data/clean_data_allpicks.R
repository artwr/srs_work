#Clean_data
setwd("D:/SRS/data_SRS/TCCZ_krig/R_work")

TCCZdatac<-TCCZdata
##Convert non meaningful zeros to NAs
TCCZdatac[TCCZdatac==0]<-NA

##Save as flat object
saveRDS(TCCZdatac, file = "TCCZ.rdata")