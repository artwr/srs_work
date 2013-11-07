require(ggplot2)
require(lubridate)
require(plyr)

# setwd("D:/work/Code/srs_work/exp_model")
setwd("D:/CodeProjects/R_SRS/exp_model")

tritium.raw<-read.csv("../SRS_data/tritium4R.csv")
tritium.raw$RESULT[tritium.raw$RESULT<0]<-NA
#tritium.see<-tritium.raw[tritium.raw$RESULT<0,]
tritium.clean<-tritium.raw[!is.na(tritium.raw$RESULT),]
tritium.clean$Year<-decimal_date(strptime(tritium.clean$MDATE,"%Y-%m-%d %H:%M:%S"))

# tritiumforlm<-tritium.clean[,c("STATION_ID","RESULT","Year")]
# names(tritiumforlm)<-c("well","Tritium","Year")
# 
# tritiumlpw<-split(tritiumforlm,tritiumforlm$well)
# 
# 
# tritiuml.lm<-llply(tritiumlpw, function(zzl) {lm(log(Tritium)~Year, data=zzl)})

# summary(tritiuml.lm[[1]])
# names(tritiuml.lm)

# wells1<-names(tritiuml.lm)[substring(names(tritiuml.lm),1,3)=="FSB"]
# wells1.lm<-tritiuml.lm[wells1]
# # substring(names(tritiuml.lm),1,3)
# 
# # wells1.lm[[1]]$coefficients
# 
# decayc<-as.data.frame(wells1)
# 
# for (ii in 1:length(wells1.lm)) {
#   decayc$k<-wells1.lm[[1]]$coefficients[2]
# }


fsb78<-tritium.clean[tritium.clean$STATION_ID=='FSB 78',]
fsb79<-tritium.clean[tritium.clean$STATION_ID=='FSB 79',]
fsb87D<-tritium.clean[tritium.clean$STATION_ID=='FSB 87D',]
fsb98D<-tritium.clean[tritium.clean$STATION_ID=='FSB 98D',]
# fsb110<-tritium.clean[tritium.clean$STATION_ID=='FSB 110',]
# fsb78$Year<-decimal_date(strptime(fsb78$MDATE,"%Y-%m-%d %H:%M:%S"))

qplot(Year,RESULT,data=fsb79) + scale_y_log10()


fsb78a.lm<-lm(log(RESULT)~Year,data=fsb78)
fsb79a.lm<-lm(log(RESULT)~Year,data=fsb79)
fsb87Da.lm<-lm(log(RESULT)~Year,data=fsb87D)
fsb98Da.lm<-lm(log(RESULT)~Year,data=fsb98D)
# fsb110.lm<-lm(log(RESULT)~Year,data=fsb110)

fsb78.lm<-lm(log(RESULT)~Year,data=fsb78,subset= fsb78$Year>1999)
fsb79.lm<-lm(log(RESULT)~Year,data=fsb79,subset= fsb79$Year>1999)
fsb87D.lm<-lm(log(RESULT)~Year,data=fsb87D,subset= fsb87D$Year>1999)
fsb98D.lm<-lm(log(RESULT)~Year,data=fsb98D,subset= fsb98D$Year>1999)

summary(fsb78.lm)
summary(fsb79.lm)
summary(fsb87D.lm)
summary(fsb98D.lm)

fsb78.lm$coefficients[2]
fsb79.lm$coefficients[2]
fsb87D.lm$coefficients[2]
fsb98D.lm$coefficients[2]

fsb98Dp<-fsb98D[,c("Year","RESULT")]
names(fsb98Dp)<-c("Year","Tritium")

predf<-rbind(fsb98Dp,data.frame(Year=seq(from=1992,to=2061,by=1),Tritium=NA))
predf$logT<-log(predf$Tritium)

# years<-data.frame(Year=seq(from=1991,to=2061,by=1))
fsb98D.pred<-predict(fsb98D.lm,newdata=predf, se.fit=TRUE, interval = "prediction")

# predictdf<-years
predf$lip1<-fsb98D.pred$fit[,1]
predf$lup1<-fsb98D.pred$fit[,2]
predf$llo1<-fsb98D.pred$fit[,3]
predf$ip1<-exp(fsb98D.pred$fit[,1])
predf$up1<-exp(fsb98D.pred$fit[,2])
predf$lo1<-exp(fsb98D.pred$fit[,3])
predf$tmcl<-20


g1<-ggplot(data=predf,aes(x=Year,y=Tritium))
g1<-g1 + geom_point(size=4)
g1<-g1 + geom_line(aes(y=tmcl)) 
g1<-g1 + geom_line(aes(y=ip1))
g1<-g1 + geom_smooth(aes(ymin=lo1,ymax=up1), stat="identity")
g1<-g1 + scale_y_log10()
print(g1)


g2<-ggplot(data=predf,aes(x=Year,y=logT))
g2<-g2 + stat_smooth(method=lm, fullrange = TRUE) + geom_point()
g2<-g2 + geom_line(aes(y=tmcl))



# inventory.final<-readRDS("../inventory/inventoryfinal.rdata")
# sourceterm<-readRDS("../source_term/tritiumsource.rdata")
# comparison<-merge(sourceterm,inventory.final,by.x = "Year", by.y = "MYEAR")
# comparison$inventory1bCD<-comparison$inventory1b+comparison$inventoryC
# 
# gwI.lm<-lm(log(gwInventory)~Year,data=comparison)
# inv1.lm<-lm(log(inventory1bCD)~Year,data=comparison)
# inv1loess.lm<-lm(log(tCD)~Year,data=comparison)
# 
# years<-data.frame(Year=seq(from=1988,to=2061,by=1))
# gwI.pred<-predict(gwI.lm, newdata=years, se.fit=TRUE, interval = "prediction")
# inv1.pred<-predict(inv1.lm, newdata=years, se.fit=TRUE, interval = "prediction")
# inv1loess.pred<-predict(inv1loess.lm, newdata=years, se.fit=TRUE, interval = "prediction")

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


# + geom_line(aes(y=tmcl))
# qplot(Year, ip1, data = predf) 
# + geom_smooth(aes(ymin=lo1,ymax=up1), stat="identity")
# +geom_point(aes(),)
# + geom_line(aes(y=tmcl)) 
# + scale_y_log10()