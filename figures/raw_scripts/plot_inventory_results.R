#Plot the inventory results

#
# inventory.final.plot<-readRDS("../inventory/inventoryfinal.rdata")
names(Tinventory.final)

Tinventory.final.plot<-Tinventory.final
names(Tinventory.final.plot)[1]<-"Year"
Tinventory.final.plot$upr1<-Tinventory.final.plot$tCD.mean+2*Tinventory.final.plot$tCD.sd
Tinventory.final.plot$lwr1<-Tinventory.final.plot$tCD.mean-2*Tinventory.final.plot$tCD.sd
Tinventory.final.plot$lwr1bb<-Tinventory.final.plot$lwr1
Tinventory.final.plot$lwr1bb[Tinventory.final.plot$lwr1bb<0]<-10
Tinventory.final.plot$upr2<-Tinventory.final.plot$tCD.dint+2*Tinventory.final.plot$tCD.se
Tinventory.final.plot$lwr2<-Tinventory.final.plot$tCD.dint-2*Tinventory.final.plot$tCD.se
Tinventory.final.plot$lwr2bb<-Tinventory.final.plot$lwr2
Tinventory.final.plot$lwr2bb[Tinventory.final.plot$lwr2bb<0]<-10
#Draft ggplot for the inventory
#qplot(MYEAR, inventory, data=inventoryja)

Iinventory.final.plot<-readRDS("Iinventory.final.rdata")
Srinventory.final.plot<-readRDS("Srinventory.final.rdata")

names(Iinventory.final.plot)[1]<-"Year"
names(Srinventory.final.plot)[1]<-"Year"
Iinventory.final.plot$upr1<-Iinventory.final.plot$I.mean+2*Iinventory.final.plot$I.sd
Iinventory.final.plot$lwr1<-Iinventory.final.plot$I.mean-2*Iinventory.final.plot$I.sd
Iinventory.final.plot$upr2<-Iinventory.final.plot$I.dint+2*Iinventory.final.plot$I.se
Iinventory.final.plot$lwr2<-Iinventory.final.plot$I.dint-2*Iinventory.final.plot$I.se
Srinventory.final.plot$upr1<-Srinventory.final.plot$Sr.mean+2*Srinventory.final.plot$Sr.sd
Srinventory.final.plot$lwr1<-Srinventory.final.plot$Sr.mean-2*Srinventory.final.plot$Sr.sd
Srinventory.final.plot$upr2<-Srinventory.final.plot$Sr.dint+2*Srinventory.final.plot$Sr.se
Srinventory.final.plot$lwr2<-Srinventory.final.plot$Sr.dint-2*Srinventory.final.plot$Sr.se


Iplot <-ggplot(data=Iinventory.final.plot, aes(x=Year))
Iplot<- Iplot +geom_line(aes(y=I.mean), colour='blue')
Iplot<- Iplot +geom_ribbon(data=Iinventory.final.plot,aes(ymin=lwr1, ymax=upr1), fill='blue', alpha = 0.3)
Iplot<- Iplot +geom_line(aes(y=I.dint), colour='red')
Iplot<- Iplot +geom_ribbon(data=Iinventory.final.plot,aes(ymin=lwr2, ymax=upr2), fill='red', alpha = 0.3)
# Iplot<- Iplot +geom_line(aes(y=inventory1b), colour='red')
# Iplot<- Iplot +geom_line(aes(y=inventory1lm), colour='green')
# Iplot<- Iplot +geom_line(aes(y=inventoryC), colour='violet')
Iplot<-Iplot+labs(title="Iodine 129 Inventory")+xlab("Year")+ylab("Iodine (Ci)")
# Iplot <- Iplot + scale_y_log10()
print(Iplot)





Srplot <-ggplot(data=Srinventory.final.plot, aes(x=Year))
Srplot<- Srplot +geom_line(aes(y=Sr.mean), colour='blue')
Srplot<- Srplot +geom_ribbon(data=Srinventory.final.plot,aes(ymin=lwr1, ymax=upr1), fill='blue', alpha = 0.3)
Srplot<- Srplot +geom_line(aes(y=Sr.dint), colour='red')
Srplot<- Srplot +geom_ribbon(data=Srinventory.final.plot,aes(ymin=lwr2, ymax=upr2), fill='red', alpha = 0.3)
Srplot <- Srplot + geom_point(data=sourceterms2, aes(y=gwISr90))
# Srplot<- Srplot +geom_line(aes(y=inventory1b), colour='red')
# Srplot<- Srplot +geom_line(aes(y=inventory1lm), colour='green')
# Srplot<- Srplot +geom_line(aes(y=inventoryC), colour='violet')
Srplot<-Srplot+labs(title="Strontium Inventory")+xlab("Year")+ylab("Strontium 90 (Ci)")
Srplot<-Srplot+coord_cartesian(xlim = c(1993,2011), ylim = c(-1,11))
# Srplot <- Srplot + scale_y_log10()
print(Srplot)




Tplot<-ggplot(data=Tinventory.final.plot, aes(x=Year))
Tplot<- Tplot +geom_line(aes(y=tCD.mean), colour='blue')
Tplot<- Tplot +geom_ribbon(data=Tinventory.final.plot,aes(ymin=lwr1, ymax=upr1), fill='blue', alpha = 0.3)
Tplot<- Tplot +geom_line(aes(y=tCD.dint), colour='red')
Tplot<- Tplot +geom_ribbon(data=Tinventory.final.plot,aes(ymin=lwr2, ymax=upr2), fill='red', alpha = 0.3)
Tplot<- Tplot + geom_point(data=tritiumsource, aes(y=gwInventory))
# Tplot<- Tplot +geom_line(aes(y=inventory1b), colour='red')
# Tplot<- Tplot +geom_line(aes(y=inventory1lm), colour='green')
# Tplot<- Tplot +geom_line(aes(y=inventoryC), colour='violet')
Tplot<-Tplot+labs(title="Tritium Inventory")+xlab("Year")+ylab("Tritium (Ci)")
# Tplot <- Tplot + scale_y_log10()
print(Tplot)

Tplot<-ggplot(data=Tinventory.final.plot, aes(x=Year))
Tplot<- Tplot +geom_line(aes(y=tCD.mean), colour='blue')
Tplot<- Tplot +geom_ribbon(data=Tinventory.final.plot,aes(ymin=lwr1, ymax=upr1), fill='blue', alpha = 0.3)
Tplot<- Tplot +geom_line(aes(y=tCD.dint), colour='red')
Tplot<- Tplot +geom_ribbon(data=Tinventory.final.plot,aes(ymin=lwr2, ymax=upr2), fill='red', alpha = 0.3)
Tplot<- Tplot + geom_point(data=tritiumsource, aes(y=gwInventory))
# Tplot<- Tplot +geom_line(aes(y=inventory1b), colour='red')
# Tplot<- Tplot +geom_line(aes(y=inventory1lm), colour='green')
# Tplot<- Tplot +geom_line(aes(y=inventoryC), colour='violet')
Tplot<-Tplot+labs(title="Tritium Inventory")+xlab("Year")+ylab("Tritium (Ci)")
Tplot <- Tplot +coord_cartesian(xlim = c(1988,2011))
print(Tplot)

Tplot<-ggplot(data=Tinventory.final.plot, aes(x=Year))
Tplot<- Tplot +geom_line(aes(y=tCD.mean), colour='blue')
Tplot<- Tplot +geom_ribbon(data=Tinventory.final.plot,aes(ymin=lwr1bb, ymax=upr1), fill='blue', alpha = 0.3)
Tplot<- Tplot +geom_line(aes(y=tCD.dint), colour='red')
Tplot<- Tplot +geom_ribbon(data=Tinventory.final.plot,aes(ymin=lwr2bb, ymax=upr2), fill='red', alpha = 0.3)
Tplot<- Tplot + geom_point(data=tritiumsource, aes(y=gwInventory))
# Tplot<- Tplot +geom_line(aes(y=inventory1b), colour='red')
# Tplot<- Tplot +geom_line(aes(y=inventory1lm), colour='green')
# Tplot<- Tplot +geom_line(aes(y=inventoryC), colour='violet')
Tplot<-Tplot+labs(title="Tritium Inventory")+xlab("Year")+ylab("Tritium (Ci)")
Tplot <- Tplot +coord_cartesian(xlim = c(1988,2011))
Tplot <- Tplot + scale_y_log10()
print(Tplot)

Tplot<-ggplot(data=Tinventory.final.plot, aes(x=Year))
Tplot<- Tplot +geom_line(aes(y=tCD.mean), colour='blue')
Tplot<- Tplot +geom_ribbon(data=Tinventory.final.plot,aes(ymin=lwr1, ymax=upr1), fill='blue', alpha = 0.3)
Tplot<- Tplot +geom_line(aes(y=tCD.dint), colour='red')
Tplot<- Tplot +geom_ribbon(data=Tinventory.final.plot,aes(ymin=lwr2, ymax=upr2), fill='red', alpha = 0.3)
# Tplot<- Tplot +geom_line(aes(y=inventory1b), colour='red')
# Tplot<- Tplot +geom_line(aes(y=inventory1lm), colour='green')
# Tplot<- Tplot +geom_line(aes(y=inventoryC), colour='violet')
Tplot<-Tplot+labs(title="Tritium Inventory")+xlab("Year")+ylab("Tritium (Ci)")
# Tplot <- Tplot + scale_y_log10()
Tplot <- Tplot + coord_cartesian(xlim = c(2000,2011), ylim = c(-1,7000))
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
