#correlations_analysis

# required libraries
require(Hmisc)
require(spatstat)
require(gclus)
require(plyr)
#require(dplyr)

# 
source("analysis/raw_scripts/functions_correl.R")

corr.clean <- readRDS(file = "srs_data/processed/corrclean.rdata")

datavals <- corr.clean[, 16:100]
# names(corr.clean[, 16:100])
# rcorr(as.matrix(corr.clean[, c("TRITIUM", "NONVOLATILE.BETA", "GROSS.ALPHA", "NITRATE.NITRITE.AS.NITROGEN", "SPECIFIC.CONDUCTANCE", "IODINE.129", "URANIUM.235", "URANIUM.238", "CESIUM.137", "TECHNETIUM.99", "STRONTIUM.90", "TOTAL.ACTIVITY")]))


cormatrixlist <- rcorr(as.matrix(datavals))
cormatrix <- cormatrixlist$r
numpairs <- cormatrixlist$n
pvals <- cormatrixlist$P
# Correlogram plot
plot(im(cormatrix[nrow(cormatrix):1,]))


numpairsmat1 <- numpairs
diag(numpairsmat1) <- NA
numpairsmat1[upper.tri(numpairsmat1)] <- NA
pairnumdf <- as.data.frame(as.table(numpairsmat1, useNA = "no"))
names(pairnumdf) <- c("First.Variable", "Second.Variable", "n")
head(pairnumdf[order(pairnumdf$n),], n = 20L)

hist(pairnumdf$n, breaks = 25)

# pairnumdf[order(abs(pairnumdf$n),decreasing=T),]

#try dissimilarity
dissimilarity <- 1 - cormatrix
dissim2 <- .5 * (1 - cormatrix)
dissim3 <- 1 - abs(cormatrix)

distance <- as.dist(dissimilarity)
distance2 <- as.dist(dissim2)
distance3 <- as.dist(dissim3)

varclusterpearson <- varclus(as.matrix(datavals) , similarity="pearson", na.action=na.retain)

# varclusterspearman <- varclus(datavals, similarity="spearman",  method="complete", na.action=na.retain)


plot(hclust(distance), main="Dissimilarity = 1 - Correlation", xlab="")

mosthighlycorrelated(datavals, 20)

correlationsdf(datavals)

cormatrix1 <- cormatrix
diag(cormatrix1) <- NA
cormatrix1[lower.tri(cormatrix1)] <- NA
cordf <-as.data.frame(as.table(cormatrix1, useNA = "always"))
names(cordf) <- c("First.Variable", "Second.Variable","Correlation")
head(cordf[!is.na(cordf$Correlation),])
head(cordf[order(abs(cordf$Correlation),decreasing=T),], n = 20)

head(as.vector(numpairs))

cordfclean <- cordf[!is.na(cordf$Correlation),]
is.numeric(cordfclean$Correlation)
abs(cordfclean$Correlation)
