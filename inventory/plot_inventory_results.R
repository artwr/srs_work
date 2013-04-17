#Plot the inventory results

#
# inventory.final.plot<-readRDS("../inventory/inventoryfinal.rdata")
names(Tinventory.final)
names(Tinventory.final)[1]<-"Year"
Tinventory.final.plot<-Tinventory.final
with(Tinventory.final.plot,Tinventory.final.plot$upr1<-tCD.mean+2*tCD.sd)
with(Tinventory.final.plot,Tinventory.final.plot$lwr1<-tCD.mean-2*tCD.sd)
with(Tinventory.final.plot,Tinventory.final.plot$upr2<-tCD.dint+2*tCD.se)
with(Tinventory.final.plot,Tinventory.final.plot$lwr2<-tCD.dint-2*tCD.se)
#Draft ggplot for the inventory
#qplot(MYEAR, inventory, data=inventoryja)

Tplot<-ggplot(data=Tinventory.final.plot, aes(x=Year))
Tplot<- Tplot +geom_line(aes(y=tCD.mean), colour='blue')
Tplot<- Tplot +geom_ribbon(data=Tinventory.final.plot,aes(ymin=lwr1, ymax=upr1), colour='blue', alpha = 0.3)
Tplot<- Tplot +geom_line(aes(y=inventory1b), colour='red')
Tplot<- Tplot +geom_line(aes(y=inventory1lm), colour='green')
Tplot<- Tplot +geom_line(aes(y=inventoryC), colour='violet')
Tplot<-Tplot+labs(title="Tritium Inventory")+xlab("Year")+ylab("Tritium (Ci)")
print(Tplot)


ja.plot<-ggplot(data=inventoryja, aes(x=MYEAR))
ja.plot<- ja.plot +geom_line(aes(y=inventory1), colour='blue')
ja.plot<- ja.plot +geom_line(aes(y=inventory1b), colour='red')
ja.plot<- ja.plot +geom_line(aes(y=inventory1lm), colour='green')
ja.plot<- ja.plot +geom_line(aes(y=inventoryC), colour='violet')
ja.plot<-ja.plot+labs(title="Tritium Inventory")+xlab("Year")+ylab("Tritium (Ci)")
print(ja.plot)

# 
# # log2 scaling of the y axis (with visually-equal spacing)
# ja.plotlog1<- ja.plot + scale_y_continuous(trans=log10_trans())
# print(ja.plotlog1)
# # log2 coordinate transformation (with visually-diminishing spacing)
# ja.plotlog2<- ja.plot + coord_trans(y="log2")
# print(ja.plotlog2)
