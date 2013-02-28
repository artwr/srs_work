require(ggplot2)

setwd("D:/work/Code/srs_work/exp_model")
# setwd("D:/CodeProjects/R_SRS/exp_model")

#params
ptsize<-12


inventory.final<-readRDS("../inventory/inventoryfinal.rdata")
#inventoryN.final<-readRDS("../inventory/inventoryfinalN.rdata")
inventoryN.final<-readRDS("../inventory/inventoryfinalN_alt.rdata")
names(inventoryN.final)[1]<-"Year"
names(inventoryN.final)
sourceterm<-readRDS("../source_term/tritiumsource.rdata")
comparison<-merge(sourceterm,inventory.final,by.x = "Year", by.y = "MYEAR")
comparison$inventory1bCD<-comparison$inventory1b+comparison$inventoryC

comparison2<-comparison[comparison$Year>2000,]
inventoryN.final2<-inventoryN.final[inventoryN.final$Year>2000,]

gwI.lm<-lm(log(gwInventory)~Year,data=comparison)
inv1all.lm<-lm(log(inventory1bCD)~Year,data=comparison)
inv1UAZ.lm<-lm(log(inventory1b)~Year,data=comparison)
inv1LAZ.lm<-lm(log(inventoryC)~Year,data=comparison)
inv1loessall.lm<-lm(log(tCD)~Year,data=comparison)
inv1loessUAZ.lm<-lm(log(t)~Year,data=comparison)
inv1loessLAZ.lm<-lm(log(tC)~Year,data=comparison)
inv1loessNall.lm<-lm(log(NCD)~Year,data=inventoryN.final)
inv1loessNUAZ.lm<-lm(log(N)~Year,data=inventoryN.final)
inv1loessNLAZ.lm<-lm(log(NC)~Year,data=inventoryN.final)

gwI.lm2<-lm(log(gwInventory)~Year,data=comparison2)
inv1all.lm2<-lm(log(inventory1bCD)~Year,data=comparison2)
inv1UAZ.lm2<-lm(log(inventory1b)~Year,data=comparison2)
inv1LAZ.lm2<-lm(log(inventoryC)~Year,data=comparison2)
inv1loessall.lm2<-lm(log(tCD)~Year,data=comparison2)
inv1loessUAZ.lm2<-lm(log(t)~Year,data=comparison2)
inv1loessLAZ.lm2<-lm(log(tC)~Year,data=comparison2)
inv1loessNall.lm2<-lm(log(NCD)~Year,data=inventoryN.final2)
inv1loessNUAZ.lm2<-lm(log(N)~Year,data=inventoryN.final2)
inv1loessNLAZ.lm2<-lm(log(NC)~Year,data=inventoryN.final2)




final.plot3<-ggplot(data=inventoryN.final, aes(x=Year, size=12))
final.plot3<- final.plot3 + theme_bw()
# final.plot3<- final.plot3 +geom_point(aes(y=inventory1b), colour='red')
final.plot3<- final.plot3 +geom_point(aes(y=N), colour='orange')
final.plot3<- final.plot3 +geom_point(aes(y=NC), colour='violet')
final.plot3<- final.plot3 +geom_point(aes(y=NCD), colour='yellow')
final.plot3<- final.plot3 + theme(plot.title=element_text(face="bold", colour="#000000", size=30))
final.plot3<- final.plot3 + theme(axis.title.x = element_text(face="bold", colour="#000000", size=26))
final.plot3<- final.plot3 + theme(axis.title.y = element_text(face="bold", colour="#000000", lineheight=1 , size=26))
final.plot3<- final.plot3 + theme(axis.text.x  = element_text(size=22))
final.plot3<- final.plot3 + theme(axis.text.y  = element_text(size=22))
final.plot3<- final.plot3 + scale_y_log10()
final.plot3<-final.plot3+labs(title="Nitrate Inventory")+xlab("Year")+ylab("Nitrate")
print(final.plot3)

final.plot4<-ggplot(data=comparison, aes(x=Year, size=3))
final.plot4<- final.plot4 + theme_bw()
# final.plot4<- final.plot4 +geom_point(aes(y=inventory1), colour='blue', size=2)
final.plot4<- final.plot4 +geom_point(aes(y=inventory1b), colour='red')
# final.plot4<- final.plot4 +geom_point(aes(y=inventory1lm), colour='green', size=2)
final.plot4<- final.plot4 +geom_point(aes(y=t), colour='orange')
final.plot4<- final.plot4 +geom_point(aes(y=gwInventory), colour='black')
#final.plot4<- final.plot4 +geom_point(aes(y=tfl), colour='black')
final.plot4<- final.plot4 +geom_point(aes(y=tC), colour='violet')
final.plot4<- final.plot4 +geom_point(aes(y=tCD), colour='yellow')
final.plot4<- final.plot4 + theme(plot.title=element_text(face="bold", colour="#000000", size=30))
final.plot4<- final.plot4 + theme(axis.title.x = element_text(face="bold", colour="#000000", size=26))
final.plot4<- final.plot4 + theme(axis.title.y = element_text(face="bold", colour="#000000", lineheight=1 , size=26))
final.plot4<- final.plot4 + theme(axis.text.x  = element_text(size=22))
final.plot4<- final.plot4 + theme(axis.text.y  = element_text(size=22))
final.plot4<- final.plot4 + scale_y_log10()
final.plot4<-final.plot4+labs(title="Tritium Inventory")+xlab("Year")+ylab("Tritium (Ci)")
print(final.plot4)



gwI.lm$coefficients[2]
inv1all.lm$coefficients[2]
inv1UAZ.lm$coefficients[2]
inv1LAZ.lm$coefficients[2]
inv1loessall.lm$coefficients[2]
inv1loessUAZ.lm$coefficients[2]
inv1loessLAZ.lm$coefficients[2]
inv1loessNall.lm$coefficients[2]
inv1loessNUAZ.lm$coefficients[2]
inv1loessNLAZ.lm$coefficients[2]


gwI.lm2$coefficients[2]
inv1all.lm2$coefficients[2]
inv1UAZ.lm2$coefficients[2]
inv1LAZ.lm2$coefficients[2]
inv1loessall.lm2$coefficients[2]
inv1loessUAZ.lm2$coefficients[2]
inv1loessLAZ.lm2$coefficients[2]
inv1loessNall.lm2$coefficients[2]
inv1loessNUAZ.lm2$coefficients[2]
inv1loessNLAZ.lm2$coefficients[2]

summary(inv1all.lm2)
summary(inv1UAZ.lm2)
summary(inv1LAZ.lm2)

summary(inv1all.lm)
summary(inv1UAZ.lm)
summary(inv1LAZ.lm)

years<-data.frame(Year=seq(from=1988,to=2061,by=1))
gwI.pred<-predict(gwI.lm, newdata=years, se.fit=TRUE, interval = "prediction")
inv1.pred<-predict(inv1.lm, newdata=years, se.fit=TRUE, interval = "prediction")
inv1loess.pred<-predict(inv1loess.lm, newdata=years, se.fit=TRUE, interval = "prediction")

# predictdf<-years
# predictdf$lip1<-gwI.pred$fit[,1]
# predictdf$lup1<-gwI.pred$fit[,2]
# predictdf$llo1<-gwI.pred$fit[,3]
# predictdf$ip1<-exp(gwI.pred$fit[,1])
# predictdf$up1<-exp(gwI.pred$fit[,2])
# predictdf$lo1<-exp(gwI.pred$fit[,3])
# # predictdf$tmcl<-20
# # + geom_line(aes(y=tmcl))
# qplot(Year, ip1, data = predictdf) + geom_smooth(aes(ymin=lo1,ymax=up1), stat="identity") + scale_y_log10()
# ggplot(data=predictf)


# png("tritium_comparison_w_clegend_log.png", width=860, height=720)
# fp5<-ggplot(data=comparison, aes(x=Year))
# fp5<- fp5 + theme_bw()
# fp5<- fp5 +geom_point(aes(y=gwInventory, colour="Mass Balance", shape="Mass Balance"),size=ptsize)
# fp5<- fp5 +geom_point(aes(y=inventory1bCD, colour="Mean Estimate", shape="Mean Estimate"),size=ptsize)
# fp5<- fp5 +geom_point(aes(y=tCD, colour="Loess Model", shape="Loess Model"),size=ptsize)
# fp5<- fp5 + theme(plot.title=element_text(face="bold", colour="#000000", size=30))
# fp5<- fp5 + theme(axis.title.x = element_text(face="bold", colour="#000000", size=26))
# fp5<- fp5 + theme(axis.title.y = element_text(face="bold", colour="#000000", lineheight=1 , size=26))
# fp5<- fp5 + theme(axis.text.x  = element_text(size=22))
# fp5<- fp5 + theme(axis.text.y  = element_text(size=22))
# fp5<- fp5 + theme(legend.text  = element_text(size=22))
# fp5<- fp5 + theme(legend.title  = element_text(size=26))
# fp5<- fp5 + opts(legend.position  = c(0.7,0.8))
# fp5<-fp5+labs(title="Tritium Inventory")+xlab("Year")+ylab("Tritium (Ci)")
# fp5<- fp5 + scale_colour_manual(name="Inventory Models", values=c("Mass Balance"="orange","Mean Estimate"="red","Loess Model"="green"))
# fp5<- fp5 + scale_shape_manual(name="Inventory Models", values=c("Mass Balance"=15,"Mean Estimate"=16,"Loess Model"=17))
# fp5<- fp5 + scale_y_log10(limits=c(1e3,1e5))
# print(fp5)
# dev.off()