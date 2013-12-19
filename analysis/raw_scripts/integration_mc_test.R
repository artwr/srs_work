# Monte Carlo Integration

# Source the interpolation domain
source("./geo_data/create_interpolation_domain_df.R")

set.seed(1794)


f <- function(x) predict(l,newdata=x)

#Number of samples
NS <- 10000
Xsamp <- runif(NS, min = , max = )
Ysamp <- runif(NS, min = , max = )