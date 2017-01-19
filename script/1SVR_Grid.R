setwd("L:/7sem/YELP/LinkPrediction")

library(data.table)
library(e1071)
library(dplyr)


setwd("DataSet")
setwd("ori")
files<-list.files()
data<-list()
index<-c(2,1,3,4,5,6,7,8)
for (i in 1:8){
  data[[i]]<-list()
  data[[i]]$ori<-data.frame()
  data[[i]]$ori<-fread(files[index[i]],header=T)
}


