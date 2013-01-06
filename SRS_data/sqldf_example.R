#Test of sqldf
require(sqldf)
#require(data.table)
uranium235.raw<-read.csv.sql("results4R.csv", sql = "select * from file where ANALYTE_NAME='URANIUM-235'",sep="|")
uranium238.raw<-read.csv.sql("results4R.csv", sql = "select * from file where ANALYTE_NAME='URANIUM-238'",sep="|")
name="'TRITIUM'"
tritium11.raw<-read.csv.sql("results4R.csv", sql = paste0("select * from file where ANALYTE_NAME=",name),sep="|")
paste0("select * from file where ANALYTE_NAME='",name,"'")
selectanalyte<-function (name) {read.csv.sql("results4R.csv", sql = paste0("select * from file where ANALYTE_NAME=",name),sep="|")}
selectanalytequoted<-function (name) {read.csv.sql("results4R.csv", sql = paste0("select * from file where ANALYTE_NAME='",name,"'"),sep="|")}

