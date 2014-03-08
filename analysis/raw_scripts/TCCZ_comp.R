# TCCZ Interpolation

#Import picks for the TCCZ
TCCZe <- readRDS("./geo_data/processed/TCCZ_nearF_final.rdata")

#
#Alpha loess
#alphaloessTCCZ <- c(.15,.2,.25,.3,.4,.5)
#alphaloessTCCZ <- c(.2,.3,.5,.6,.7,.8)
alphaloessTCCZ <- seq(.15, .9, by = 0.05)

# Use helper function to compute a range of loess models
lloessmd2 <- loessAlphaVect(lformula = "TCCZ_top~EASTING+NORTHING", ldata = TCCZe, ldegree = 2, spanvector = alphaloessTCCZ, lcontrol = lcontrold)
lloessmd1 <- loessAlphaVect(lformula = "TCCZ_top~EASTING+NORTHING", ldata = TCCZe, ldegree = 1, spanvector = alphaloessTCCZ, lcontrol = lcontrold)

loessmd1RMSD <- laply(lloessmd1, function(loessm){sqrt(sum(loessm$residuals^2))})
loessmd2RMSD <- laply(lloessmd2, function(loessm){sqrt(sum(loessm$residuals^2))})
# <- cbind(alphaloessTCCZ, loessmd1RMSD, loessd2RMSD)

matplot(x = alphaloessTCCZ, y = cbind(loessmd1RMSD, loessmd2RMSD))

#Local polynomial fit (2nd order) and linear model
# TCCZe.loess1 <- loess(TCCZ_top~EASTING+NORTHING, data = TCCZe, degree = 2, span = alphaloessTCCZ[2], normalize = FALSE, method = c("loess"), control = lcontrold)
# TCCZe.loess1b <- loess(TCCZ_top~EASTING+NORTHING, data = TCCZe, degree = 2, span = alphaloessTCCZ[4], normalize = FALSE, method = c("loess"), control = lcontrold)
TCCZe.lm <- lm(TCCZ_top~EASTING+NORTHING , data=TCCZe)

# Look at the error
# pre1<-predict(TCCZe.loess1, se = TRUE)
# pre1b<-predict(TCCZe.loess1b, se = TRUE)
pre1lm<-predict(TCCZe.lm, se = TRUE, interval = "confidence",level = 0.95)

TCCZe.pred <- TCCZe
#
# TCCZe.pred$TCCZ.fit<-pre1$fit
# TCCZe.pred$TCCZ.se.fit<-pre1$se.fit
# TCCZe.pred$TCCZ.fitb<-pre1b$fit
# TCCZe.pred$TCCZ.se.fitb<-pre1b$se.fit
TCCZe.pred$TCCZ.fitlm<-pre1lm$fit[,1]
TCCZe.pred$TCCZ.se.fitlm<-pre1lm$se.fit
#Upper Bound
TCCZe.pred$TCCZ.fitlmupr<-pre1lm$fit[,2]
#Lower Bound
TCCZe.pred$TCCZ.fitlmlwr<-pre1lm$fit[,3]

#Error
# TCCZe.pred$ehat.l1 <- TCCZe.pred$TCCZ_top - TCCZe.pred$TCCZ.fit
# TCCZe.pred$ehat.l1b <- TCCZe.pred$TCCZ_top - TCCZe.pred$TCCZ.fitb
TCCZe.pred$ehat.lm <- TCCZe.pred$TCCZ_top - TCCZe.pred$TCCZ.fitlm

#Save the linmod predictions
saveRDS(TCCZe.pred,"./analysis/processed_data/TCCZlmpred.rdata")

predloessmd1 <- llply(lloessmd1, function(loessm){as.vector(predict(loessm, newdata = interpolation.grid))})

predloessmd1se <- llply(lloessmd1, function(loessm){pred <- predict(loessm, newdata = interpolation.grid, se = TRUE); return(as.vector(pred$se.fit))})

predloessmd2 <- llply(lloessmd2, function(loessm){as.vector(predict(loessm, newdata = interpolation.grid))})

predloessmd2se <- llply(lloessmd2, function(loessm){pred <- predict(loessm, newdata = interpolation.grid, se = TRUE); return(as.vector(pred$se.fit))})

# names(predloessm)
# length(predloessm[["alpha0.15"]])
# as.data.frame(predloessm)
TCCZ.interpolatedd1 <- cbind(interpolation.grid, as.data.frame(predloessmd1))
TCCZ.interpolatedd1se <- cbind(interpolation.grid, as.data.frame(predloessmd1se))
saveRDS(TCCZ.interpolatedd1,"./analysis/processed_data/TCCZloessinterpolationd1.rdata")
saveRDS(TCCZ.interpolatedd1se,"./analysis/processed_data/TCCZloessinterpolationd1se.rdata")

saveRDS(TCCZ.interpolatedd1,file = paste0("./analysis/processed_data/TCCZloessinterpolationd1_",format(Sys.time(), "%Y%m%d-%H%M"),".rdata")
saveRDS(TCCZ.interpolatedd1se,file = paste0("./analysis/processed_data/TCCZloessinterpolationd1se_",format(Sys.time(), "%Y%m%d-%H%M"),".rdata")

TCCZ.interpolatedd2 <- cbind(interpolation.grid, as.data.frame(predloessmd2))
TCCZ.interpolatedd2se <- cbind(interpolation.grid, as.data.frame(predloessmd2se))
saveRDS(TCCZ.interpolatedd2,"./analysis/processed_data/TCCZloessinterpolationd2.rdata")
saveRDS(TCCZ.interpolatedd2se,"./analysis/processed_data/TCCZloessinterpolationd2se.rdata")
saveRDS(TCCZ.interpolatedd2,file = paste0("./analysis/processed_data/TCCZloessinterpolationd2_",format(Sys.time(), "%Y%m%d-%H%M"),".rdata")
saveRDS(TCCZ.interpolatedd2se,file = paste0("./analysis/processed_data/TCCZloessinterpolationd2se_",format(Sys.time(), "%Y%m%d-%H%M"),".rdata")

plot.dfd1 <- TCCZ.interpolatedd1
plot.dfd1.se <- TCCZ.interpolatedd1se
plot.dfd2 <- TCCZ.interpolatedd2
plot.dfd2.se <- TCCZ.interpolatedd2se
#names(plot.df)

plot.dfd1.long <- melt(plot.dfd1, id.vars = c("EASTING","NORTHING"),
     variable.name = "alpha", na.rm = FALSE,
     value.name = "TCCZ_top.fit")

plot.dfd1.se.long <- melt(plot.dfd1.se, id.vars = c("EASTING","NORTHING"),
                     variable.name = "alpha", na.rm = FALSE,
                     value.name = "TCCZ_top.sefit")

plot.dfd2.long <- melt(plot.dfd2, id.vars = c("EASTING","NORTHING"),
                       variable.name = "alpha", na.rm = FALSE,
                       value.name = "TCCZ_top.fit")

plot.dfd2.se.long <- melt(plot.dfd2.se, id.vars = c("EASTING","NORTHING"),
                          variable.name = "alpha", na.rm = FALSE,
                          value.name = "TCCZ_top.sefit")

# Added facet_wrap plot with alpha for comparison between the alphas
# require(ggthemes)
# currentparamforplotting <- par(no.readonly = TRUE)

# Base gg objects
ggdiagd1 <- ggplot(data = plot.dfd1.long, mapping = aes(x=EASTING,y=NORTHING)) + theme_bw()
ggdiagd1se <- ggplot(data = plot.dfd1.se.long, mapping = aes(x=EASTING,y=NORTHING)) + theme_bw()

ggdiagd2 <- ggplot(data = plot.dfd2.long, mapping = aes(x=EASTING,y=NORTHING)) + theme_bw()
ggdiagd2se <- ggplot(data = plot.dfd2.se.long, mapping = aes(x=EASTING,y=NORTHING)) + theme_bw()


#
ggres <- ggdiag + geom_tile(aes(fill = TCCZ_top.fit)) + scale_fill_gradient_tableau("Blue-Green Sequential") + facet_wrap(~ alpha)
# ggres <- ggres +coord_cartesian(xlim = c(), ylim = , wise = NULL)
#   ggstderr <- ggdiag + geom_tile(aes(fill = se.fit)) + scale_fill_gradient(low = "white", high = "green")
print(ggres)

ggres2 <- ggdiag + geom_tile(aes(fill = TCCZ_top.fit)) + scale_fill_gradient(low="green", high = "brown") + facet_wrap(~ alpha)
# ggres <- ggres +coord_cartesian(xlim = c(), ylim = , wise = NULL)
#   ggstderr <- ggdiag + geom_tile(aes(fill = se.fit)) + scale_fill_gradient(low = "white", high = "green")
print(ggres2)

ggres3 <- ggdiag + geom_tile(aes(fill = TCCZ_top.fit)) + scale_fill_gradientn(colours = rainbow(7)) + facet_wrap(~ alpha)
# ggres <- ggres +coord_cartesian(xlim = c(), ylim = , wise = NULL)
#   ggstderr <- ggdiag + geom_tile(aes(fill = se.fit)) + scale_fill_gradient(low = "white", high = "green")
print(ggres3)

ggres4 <- ggdiagse + geom_tile(aes(fill = TCCZ_top.sefit)) + scale_fill_gradientn(colours = rainbow(7)) + facet_wrap(~ alpha)
# ggres <- ggres +coord_cartesian(xlim = c(), ylim = , wise = NULL)
#   ggstderr <- ggdiag + geom_tile(aes(fill = se.fit)) + scale_fill_gradient(low = "white", high = "green")
print(ggres4)

ggresd15 <- ggdiagd1se + geom_tile(aes(fill = TCCZ_top.sefit)) + scale_fill_gradientn(colours = rainbow(7), trans = "log") + facet_wrap(~ alpha)
# ggres <- ggres +coord_cartesian(xlim = c(), ylim = , wise = NULL)
#   ggstderr <- ggdiag + geom_tile(aes(fill = se.fit)) + scale_fill_gradient(low = "white", high = "green")
print(ggresd15)

ggresd25 <- ggdiagd2se + geom_tile(aes(fill = TCCZ_top.sefit)) + scale_fill_gradientn(colours = rainbow(7), trans = "log") + facet_wrap(~ alpha)
# ggres <- ggres +coord_cartesian(xlim = c(), ylim = , wise = NULL)
#   ggstderr <- ggdiag + geom_tile(aes(fill = se.fit)) + scale_fill_gradient(low = "white", high = "green")
print(ggresd25)

# ggdiag2 <- ggplot(data = plot.long2, mapping = aes(x=EASTING,y=NORTHING)) + theme_bw()
# ggres2 <- ggdiag2 + geom_tile(aes(fill = TCCZ_top.fit)) + scale_fill_gradient_tableau("Blue-Green Sequential") + facet_wrap(~ alpha)
# # ggres <- ggres +coord_cartesian(xlim = c(), ylim = , wise = NULL)
# #   ggstderr <- ggdiag + geom_tile(aes(fill = se.fit)) + scale_fill_gradient(low = "white", high = "green")
# print(ggres2)
# 
# par(mfrow = c(1,2))

