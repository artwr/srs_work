# File to use to perform the whole analysis

#rm(list=ls())

require(reshape2)
require(plyr)
require(ggplot2)
require(ggthemes)
require(scales)
require(splancs)

# LOESS models options
# Direct method allows extrapolation
lcontrold<-loess.control(surface = c("direct"),
                        statistics = c("exact"),
                        trace.hat = c("exact"),
                        cell = 0.2, iterations = 5)

# Uses interpolation on a kd-tree, will give NA outside of the 2D bounding box
lcontroli<-loess.control(surface = c("interpolate"),
                        statistics = c("exact"),
                        trace.hat = c("exact"),
                        cell = 0.2, iterations = 5)

# Auxiliary Functions
source('./analysis/raw_scripts/functions.R')
source('./analysis/raw_scripts/functions_computation.R')

#ptm1<-proc.time()
#Define rectangle used for interpolation.
# This was select as the max min coordinates 
# on tritium well network measurement
source('./geo_data/create_interpolation_domain_df.R')

# File containing parameters used for all computations like porosity
# the aquifer thickness for the C wells
source('./analysis/raw_scripts/create_subsurface_parameters_vars.R')


# Compute the aquifer thickness on an average and per year basis
source('./analysis/raw_scripts/aquifer_comp.R')

# Compute the inventory for tritium
ptm1<-proc.time()
source('./analysis/raw_scripts/tritium_comp.R')
print(proc.time()-ptm1)

# Compute the inventory for nitrate



# Compute the inventory for strontium

# Compute the inventory for cesium





print(proc.time()-ptm1)
