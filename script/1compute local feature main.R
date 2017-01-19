#compute local feature main
#local features
library(dplyr)
library(data.table)
library(igraph)
library(expm)

# set work Space
setwd("L:/7sem/YELP/LinkPrediction/DataSet/")
list.files()
setwd("ori")
filename<-list.files()

#chose a file
filename
file_ith=5

path=filename[file_ith]

# load data
temp<-as.data.frame(fread(path,header=T))


###compute local features
names(temp)<-c("from","to","weight")
SimData<-f_preprocessed(temp)
SimData<-f_addBasic(SimData)
SimData<-f_addLocal(SimData)
SimData<-f_addPath(SimData,4)
SimData<-f_addDegree(SimData)

count_features=13
p<-dim(SimData$features)[2]
for(i in (p-count_features+1):p){
  SimData$features[,i]<-f_linearnorm2(SimData$features[,i])
}


############degree
degree_temp<-degree(SimData$Graph)

View(SimData$features)

OutputName<-paste("L:/7sem/YELP/LinkPrediction/DataSet/features/","feature10_",
                  gsub("^....","",filename[file_ith]),sep="")
write.csv(SimData$features,OutputName,row.names = F)

rm(path,temp,filename,file_ith,SimData,OutputName,i,p,count_features)


