#Sync R packages
IP <- as.data.frame(installed.packages())

MyPkgs <- subset(IP, !Priority %in% c("base", "recommended"), select =  c(Package))

# save(MyPkgs, file = "MyPkgs_laptop.Rdata")
# save(MyPkgs, file = "MyPkgs_desktop.Rdata")

# load("MyPkgs.Rdata")

# install.packages(MyPkgs$Package, dependencies = TRUE)