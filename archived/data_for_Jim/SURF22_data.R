#Data for Jim

inventory.final<-readRDS("../inventory/inventoryfinal.rdata")
inventory.finalN<-readRDS("../inventory/inventoryfinalN.rdata")
names(inventory.finalN)[1]<-"Year"
# names(inventory.finalN)
sourceterm<-readRDS("../source_term/tritiumsource.rdata")

comparison<-merge(sourceterm,inventory.final,by.x = "Year", by.y = "MYEAR")
comparison$inventory1bCD<-comparison$inventory1b+comparison$inventoryC

plotdf1<-comparison[,c("Year","gwInventory","tCD","inventory1bCD")]

write.csv(plotdf1,file="data_for_Jim_SURF22.csv")