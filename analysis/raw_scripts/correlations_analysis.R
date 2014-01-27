#correlations_analysis

# required libraries
library(Hmisc)
library(spatstat)

# rcorr(as.matrix(mtcars))

corr.clean <- readRDS(file = "srs_data/processed/corrclean.rdata")

datavals <- corr.clean[, 16:100]
# names(corr.clean[, 16:100])
# rcorr(as.matrix(corr.clean[, c("TRITIUM", "NONVOLATILE.BETA", "GROSS.ALPHA", "NITRATE.NITRITE.AS.NITROGEN", "SPECIFIC.CONDUCTANCE", "IODINE.129", "URANIUM.235", "URANIUM.238", "CESIUM.137", "TECHNETIUM.99", "STRONTIUM.90", "TOTAL.ACTIVITY")]))


cormatrixlist <- rcorr(as.matrix(datavals))
cormatrix <- cormatrixlist$r
numpairs <- cormatrixlist$n
pvals <- cormatrixlist$P

plot(im(cormatrix[nrow(cormatrix):1,]))

numpairsmat1 <- numpairs
numpairsmat1[upper.tri(numpairsmat1)] <- NA
pairnumdf <- as.data.frame(as.table(numpairsmat1))
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



mosthighlycorrelated <- function(mydataframe, numtoreport)
{
  require(Hmisc)
  # find the correlations
  cormatrixlist <- rcorr(mydataframe)
  cormatrix <- cormatrixlist$r
  numpairs <- cormatrixlist$n
  pvals <- cormatrixlist$P
  # set the correlations on the diagonal or lower triangle to zero,
  # so they will not be reported as the highest ones:
  diag(cormatrix) <- 0
  cormatrix[lower.tri(cormatrix)] <- 0
  # flatten the matrix into a dataframe for easy sorting
  fm <- as.data.frame(as.table(cormatrix))
  fm <-
  
  # assign human-friendly names
  names(fm) <- c("First.Variable", "Second.Variable","Correlation", "Number.of.pairs", "P.value")
  # sort and print the top n correlations
  head(fm[order(abs(fm$Correlation),decreasing=T),],n=numtoreport)
}
