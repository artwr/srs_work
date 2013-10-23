##plot with facets for the source terms
require(ggplot2)
require(reshape2)

sourceterms<-readRDS('../source_term/sourceterms.rdata')
sourceterms2<-sourceterms[,c(1,4,6,7,8)]
# Year  Volume.L	CumulVol.L	Tritium.Ci	Tritiumafterevap.Ci	Strontium90.Ci	UNAT.Ci	Cesium137.Ci
sourcetermdatalong<-melt(sourceterms2, id.vars=c("Year"), measure.vars = c("Tritium.Ci","Strontium90.Ci","UNAT.Ci","Cesium137.Ci"),variable.name = "Name.Units", na.rm = FALSE,
     value.name = "Value")

sourcetermdatalong$flabel<-factor(sourcetermdatalong$Name.Units, labels = c("Tritium (Ci)","Strontium 90 (Ci)","U-NAT (Ci)","Cesium 137 (Ci)"))

##
plots1<-ggplot(data=sourcetermdatalong, aes(x=Year,y=Value)) + geom_point() + facet_wrap(facets=c("Name.Units"), scales = "free_y")
print(plots1)





plots2 <- ggplot(data=sourcetermdatalong, aes(x=Year,y=Value)) + geom_line(size=1.5) 
plots2 <- plots2 + theme_bw()
plots2 <- plots2 + theme(text = element_text(size = 19))
plots2 <- plots2 + ylab("Activity (Ci)")
plots2 <- plots2 +facet_wrap( ~ flabel, ncol = 2, scales = "free_y")
print(plots2)


pdf(file = "inputs.pdf", width = 6, height = 7, title = "Input to the basins as reported by Cummins et al. [1991]", 
    version = "1.6", pointsize = 12)
plots3 <- ggplot(data=sourcetermdatalong, aes(x=Year,y=Value)) + geom_line(size=1.5) 
plots3 <- plots3 + theme_bw()
plots3 <- plots3 + theme(text = element_text(size = 12))
plots3 <- plots3 + ylab("Activity (Ci)")
plots3 <- plots3 +facet_wrap( ~ flabel, ncol = 2, scales = "free_y")
print(plots3)
dev.off()


# facet_wrap(facets, nrow = NULL, ncol = NULL, scales = "fixed", shrink = TRUE, as.table = TRUE, 
#            drop = TRUE)

