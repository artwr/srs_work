setwd("D:/data1")
rm(list=ls())

require(ggplot2)

te<-read.table("tritium_error.txt", header=TRUE, sep="")
##te1<-

te$tri<-as.numeric(te$resval);
te$err<-as.numeric(te$errval);
te$yearm<-as.numeric(te$yearm);
  
qplot(tri,err, data = te, color=yearm)
qplot(log(tri),log(err), data = te, color=yearm)
qplot(te$tri[te$yearm==1990],te$err[te$yearm==1990])
qplot(log(te$tri[te$yearm==1990]),log(te$err[te$yearm==1990]),ylab="log(Tritium Error)",xlab="log(Tritium Concentration (pCi/L))")