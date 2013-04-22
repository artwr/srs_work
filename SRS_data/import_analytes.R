#
require(sqldf)
# analyte
select.analyte.quoted<-function (aname) {read.csv.sql("../SRS_data/results4R.csv", sql = paste0("select * from file where ANALYTE_NAME=",aname),sep="|")}
select.analyte.unquoted<-function (aname) {read.csv.sql("../SRS_data/results4R.csv", sql = paste0("select * from file where ANALYTE_NAME='",aname,"'"),sep="|")}

# t1<-read.csv.sql("results4R.csv", sql = "select * from file where ANALYTE_NAME='IODINE-129'",sep="|")

remove.negative.values<-function (nameddf) {nameddf$RESULT[nameddf$RESULT<0]<-NA; nameddf[!is.na(nameddf$RESULT),]}

#<-read.csv.sql("results4R.csv", sql = paste0("select * from file where ANALYTE_NAME=",aname)

iodine.raw<-select.analyte.unquoted('IODINE-129')
iodine.clean<-remove.negative.values(iodine.raw)
# saveRDS(iodine.clean,file="iodinecleanall.rdata")

analyte_namesD<-read.csv.sql("../SRS_data/results4R.csv", sql = "select distinct ANALYTE_NAME from file",sep="|")
analyte_namesC<-read.csv.sql("../SRS_data/resultsC4R.csv", sql = "select distinct ANALYTE_NAME from file",sep="|")

saveRDS(analyte_namesD,'../SRS_data/analyte_namesD.rdata')
saveRDS(analyte_namesC,'../SRS_data/analyte_namesC.rdata')

analyte_names<-union(analyte_namesD$ANALYTE_NAME,analyte_namesC$ANALYTE_NAME)
saveRDS(analyte_names,'../SRS_data/analyte_names.rdata')

