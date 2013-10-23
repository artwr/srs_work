#Sync R packages
IP <- as.data.frame(installed.packages())

# MyPkgs_laptop <- subset(IP, !Priority %in% c("base", "recommended"), select =  c(Package))
# save(MyPkgs_laptop, file = "MyPkgs_laptop.Rdata")
# load("MyPkgs_desktop.Rdata")
# ptoinstall<-setdiff(MyPkgs_desktop$Package,MyPkgs_laptop$Package)
ptoinstall<-MyPkgs_laptop$Package

# MyPkgs_desktop <- subset(IP, !Priority %in% c("base", "recommended"), select =  c(Package))
# save(MyPkgs_desktop, file = "MyPkgs_desktop.Rdata")
# load("MyPkgs_laptop.Rdata")
# ptoinstall<-setdiff(MyPkgs_laptop$Package,MyPkgs_desktop$Package)

#Install the packages that need to be.
# install.packages(as.character(ptoinstall), dependencies = TRUE, type="both")
# install.packages(as.character(ptoinstall), type="both")
# install.packages(as.character(ptoinstall))