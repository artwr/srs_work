#
require(sqldf)
# analyte
select.analyte.quoted<-function (aname) {read.csv.sql("results4R.csv", sql = paste0("select * from file where ANALYTE_NAME=",aname),sep="|")}
select.analyte.unquoted<-function (aname) {read.csv.sql("results4R.csv", sql = paste0("select * from file where ANALYTE_NAME='",aname,"'"),sep="|")}

remove.negative.values<-function (nameddf) {nameddf$RESULT[nameddf$RESULT<0]<-NA; nameddf[!is.na(nameddf$RESULT),]}


iodine.raw<-select.analyte.unquoted('IODINE-129')
iodine.clean<-remove.negative.values(iodine.raw)
# saveRDS(iodine.clean,file="iodinecleanall.rdata")

