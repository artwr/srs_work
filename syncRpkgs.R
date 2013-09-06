#Sync R packages
IP <- as.data.frame(installed.packages())

# MyPkgs_laptop <- subset(IP, !Priority %in% c("base", "recommended"), select =  c(Package))
# save(MyPkgs, file = "MyPkgs_laptop.Rdata")
# load("MyPkgs_desktop.Rdata")
# ptoinstall<-setdiff(MyPkgs_desktop$Package,MyPkgs_laptop$Package)


# MyPkgs_desktop <- subset(IP, !Priority %in% c("base", "recommended"), select =  c(Package))
# save(MyPkgs, file = "MyPkgs_desktop.Rdata")
# load("MyPkgs_laptop.Rdata")
# ptoinstall<-setdiff(MyPkgs_laptop$Package,MyPkgs_desktop$Package)




# install.packages(MyPkgs$Package, dependencies = TRUE)
install.packages(ptoinstall, dependencies = TRUE)