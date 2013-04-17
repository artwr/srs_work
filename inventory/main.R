# File to use to perform the whole analysis

require(plyr)
require(ggplot2)
require(splancs)


ptm1<-proc.time()
#Define rectangle used for interpolation.
# This was select as the max min coordinates 
# on tritium well network measurement
source('interpolation_domain.R')

# Compute the aquifer thickness on an average and per year basis
source('aquifer_comp.R')

# Compute the inventory for tritium
source('tritium_comp.R')

# Compute the inventory for nitrate



# Compute the inventory for strontium

# Compute the inventory for cesium


require(scales)


print(proc.time()-ptm1)
