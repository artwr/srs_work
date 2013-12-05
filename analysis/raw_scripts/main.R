# File to use to perform the whole analysis

#setwd("./inventory")
#rm(list=ls())

require(plyr)
require(ggplot2)
require(splancs)

source('./functions.R')

#ptm1<-proc.time()
#Define rectangle used for interpolation.
# This was select as the max min coordinates 
# on tritium well network measurement
source('../../geo_data/create_interpolation_domain_df.R')


# Compute the aquifer thickness on an average and per year basis
source('./aquifer_comp.R')

# Compute the inventory for tritium
ptm1<-proc.time()
source('./tritium_comp.R')
print(proc.time()-ptm1)

# Compute the inventory for nitrate



# Compute the inventory for strontium

# Compute the inventory for cesium


require(scales)


print(proc.time()-ptm1)
