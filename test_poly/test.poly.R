require(splancs)

x <- c(1,0,0,1,1,1,1,3,3,1)
y <- c(0,0,1,1,0,0,-1,-1,0,0)
m <- cbind(x, y)
plot(m, type="b")
areapl(m)
areapl(m[1:5,])
areapl(m[6:10,])

ea<-c(436175,436175,437158,438158,436175)
no<-c(3680929,3682109,3682109,3680929,3680929)
pp<-cbind(ea,no)
plot(pp, type="b")
areapl(pp)
(2109-929)*(7158-6175)

#rm(list=ls())