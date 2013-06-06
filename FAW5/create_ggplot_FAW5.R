#create ggplot of density effect
require(ggplot2)

nitrateFAW5<-readRDS("../FAW5/nitrateFAW5.rdata")

#PNG format
#png("nitrateFAW5_test.png", width=720, height=600)
png("../FAW5/nitrateFAW5.png", width=720, height=960)
gg<-ggplot(data=nitrateFAW5,aes(x=nitratemgl))
gg<-gg+geom_line(aes(y=depth.from.wt), colour="#000099",size=3)
gg<-gg+scale_y_reverse()
gg<-gg+ theme_bw()
gg<-gg+ theme(plot.title=element_text(face="bold", colour="#000000", size=30))
gg<-gg+ theme(axis.title.x = element_text(face="bold", colour="#000000", size=26))
gg<-gg+ theme(axis.title.y = element_text(face="bold", colour="#000000", lineheight=1 , size=26))
gg<-gg+ theme(axis.text.x  = element_text(size=22))
gg<-gg+ theme(axis.text.y  = element_text(size=22))
gg<-gg+ labs(title="Nitrate profile at well FAW5 \n",x="Nitrate (mg/L)", y="Depth from water table (m) \n")
gg<-gg+ geom_rect(aes(xmin=1,xmax=799,ymin=9.8,ymax=11),fill="grey", alpha=0.2)
gg<-gg+ annotate("text", x = 400, y = 10.4, label = "Tan Clay Confining Zone", size=10)
print(gg)
dev.off()


png("../FAW5/nitrateFAW5_pts.png", width=720, height=960)
gg2<-ggplot(data=nitrateFAW5,aes(x=nitratemgl))
gg2<-gg2+geom_point(aes(y=depth.from.wt), colour="#000099", ,size=5)
gg2<-gg2+scale_y_reverse()
gg2<-gg2+ theme_bw()
gg2<-gg2+ theme(plot.title=element_text(face="bold", colour="#000000", size=30))
gg2<-gg2+ theme(axis.title.x = element_text(face="bold", colour="#000000", size=26))
gg2<-gg2+ theme(axis.title.y = element_text(face="bold", colour="#000000", lineheight=1 , size=26))
gg2<-gg2+ theme(axis.text.x  = element_text(size=22))
gg2<-gg2+ theme(axis.text.y  = element_text(size=22))
gg2<-gg2+ labs(title="Nitrate profile at well FAW5 \n",x="Nitrate (mg/L)", y="Depth from water table (m) \n")
gg2<-gg2+ geom_rect(aes(xmin=1,xmax=799,ymin=9.8,ymax=11),fill="grey", alpha=0.2)
gg2<-gg2+ annotate("text", x = 400, y = 10.4, label = "Tan Clay Confining Zone", size=10)
print(gg2)
dev.off()


# gg2<-ggplot(data=nitrateFAW5,aes(x=depth.from.wt))
# gg2<-gg2+geom_line(aes(y=nitratemgl))
# gg2<-gg2 + coord_flip()
# gg2

