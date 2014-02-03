#Correlations analysis useful functions


mosthighlycorrelated <- function(mydataframe, numtoreport)
{
  require(Hmisc)
  # find the correlations
  cormatrixlist <- rcorr(as.matrix(mydataframe))
  cormatrix <- cormatrixlist$r
  numpairs <- cormatrixlist$n
  pvals <- cormatrixlist$P
  # set the correlations on the diagonal or lower triangle to zero,
  # so they will not be reported as the highest ones:
  diag(cormatrix) <- NA
  cormatrix[lower.tri(cormatrix)] <- NA
  diag(numpairs) <- NA
  numpairs[lower.tri(numpairs)] <- NA
  diag(pvals) <- NA
  pvals[lower.tri(pvals)] <- NA
  # flatten the matrix into a dataframe for easy sorting
  fmr <- as.data.frame(as.table(cormatrix, useNA = "always"))
  fmr$n <- as.vector(numpairs) 
  fmr$P <- as.vector(pvals) 
  # assign human-friendly names
  # names(fmr) <- c("First.Variable", "Second.Variable","Correlation")
  names(fmr) <- c("First.Variable", "Second.Variable","Correlation", "Number.of.pairs", "P.value")
  # remove NA
  fmrclean <- fmr[!is.na(fmr$Correlation),]
  # sort and print the top n correlations
  head(fmrclean[order(abs(fmrclean$Correlation),decreasing=T),],n=numtoreport)
}


correlationsdf <- function(mydataframe)
{
  require(Hmisc)
  # find the correlations
  cormatrixlist <- rcorr(as.matrix(mydataframe))
  cormatrix <- cormatrixlist$r
  numpairs <- cormatrixlist$n
  pvals <- cormatrixlist$P
  # set the correlations on the diagonal or lower triangle to zero,
  # so they will not be reported as the highest ones:
  diag(cormatrix) <- NA
  cormatrix[lower.tri(cormatrix)] <- NA
  diag(numpairs) <- NA
  numpairs[lower.tri(numpairs)] <- NA
  diag(pvals) <- NA
  pvals[lower.tri(pvals)] <- NA
  # flatten the matrix into a dataframe for easy sorting
  fmr <- as.data.frame(as.table(cormatrix, useNA = "always"))
  fmr$n <- as.vector(numpairs) 
  fmr$P <- as.vector(pvals) 
  # assign human-friendly names
  # names(fmr) <- c("First.Variable", "Second.Variable","Correlation")
  names(fmr) <- c("Var1", "Var2","Correlation", "n", "pval")
  # remove NA
  fmrclean <- fmr[!is.na(fmr$Correlation),]
  # head(fmrclean)
  # sort and print the top n correlations
  fmrclean[order(abs(fmrclean$Correlation),decreasing=T),]
}

