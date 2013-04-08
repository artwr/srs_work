#Plot the inventory results

#Draft ggplot for the inventory
#qplot(MYEAR, inventory, data=inventoryja)
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
