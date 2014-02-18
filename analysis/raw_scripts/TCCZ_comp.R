# TCCZ Interpolation

#Import picks for the TCCZ
TCCZe <- readRDS("./geo_data/processed/TCCZ_nearF_final.rdata")

#
#Alpha loess
alphaloessTCCZ <- c(.15,.2,.25,.3,.4,.5)

# Use helper function to compute a range of loess models
lloessm <- loessAlphaVect(lformula = "TCCZ_top~EASTING+NORTHING", ldata = TCCZe, ldegree = 2, spanvector = alphaloessTCCZ, lcontrol = lcontrold)


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

predloessm <- llply(lloessm, function(loessm){as.vector(predict(loessm, newdata = interpolation.grid))})

predloessmse <- llply(lloessm, function(loessm){pred <- predict(loessm, newdata = interpolation.grid, se = TRUE); return(as.vector(pred$se.fit))})

# names(predloessm)
# length(predloessm[["alpha0.15"]])
# as.data.frame(predloessm)
TCCZ.interpolated <- cbind(interpolation.grid, as.data.frame(predloessm))
TCCZ.interpolatedse <- cbind(interpolation.grid, as.data.frame(predloessmse))
saveRDS(TCCZ.interpolated,"./analysis/processed_data/TCCZloessinterpolation.rdata")
saveRDS(TCCZ.interpolatedse,"./analysis/processed_data/TCCZloessinterpolationse.rdata")

plot.df <- TCCZ.interpolated
plot.df.se <- TCCZ.interpolatedse
names(plot.df)

plot.df.long <- melt(plot.df, id.vars = c("EASTING","NORTHING"),
     variable.name = "alpha", na.rm = FALSE,
     value.name = "TCCZ_top.fit")

plot.df.se.long <- melt(plot.df.se, id.vars = c("EASTING","NORTHING"),
                     variable.name = "alpha", na.rm = FALSE,
                     value.name = "TCCZ_top.sefit")

# Added facet_wrap plot with alpha for comparison between the alphas
# require(ggthemes)
# currentparamforplotting <- par(no.readonly = TRUE)

# Base gg objects
ggdiag <- ggplot(data = plot.df.long, mapping = aes(x=EASTING,y=NORTHING)) + theme_bw()
ggdiagse <- ggplot(data = plot.df.se.long, mapping = aes(x=EASTING,y=NORTHING)) + theme_bw()

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

ggres3 <- ggdiagse + geom_tile(aes(fill = TCCZ_top.sefit)) + scale_fill_gradientn(colours = rainbow(7)) + facet_wrap(~ alpha)
# ggres <- ggres +coord_cartesian(xlim = c(), ylim = , wise = NULL)
#   ggstderr <- ggdiag + geom_tile(aes(fill = se.fit)) + scale_fill_gradient(low = "white", high = "green")
print(ggres3)

# ggdiag2 <- ggplot(data = plot.long2, mapping = aes(x=EASTING,y=NORTHING)) + theme_bw()
# ggres2 <- ggdiag2 + geom_tile(aes(fill = TCCZ_top.fit)) + scale_fill_gradient_tableau("Blue-Green Sequential") + facet_wrap(~ alpha)
# # ggres <- ggres +coord_cartesian(xlim = c(), ylim = , wise = NULL)
# #   ggstderr <- ggdiag + geom_tile(aes(fill = se.fit)) + scale_fill_gradient(low = "white", high = "green")
# print(ggres2)
# 
# par(mfrow = c(1,2))

