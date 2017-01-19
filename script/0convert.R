setwd("L:/7sem/YELP/LinkPrediction/DataSet")

library(data.table)
library(e1071)
library(dplyr)

data_celegans<-fread("celegansneural [Edges].csv")
data_advogato<-fread("advogato.txt")
data_everglades<-fread("everglades.txt")
data_Geom<-fread("Geom.txt")
data_lesmis<-fread("lesmis.txt")
data_USAir1<-fread("USAir1.txt")
data_USAir2<-fread("USAir2.txt")
data_yelp<-fread("yelp.txt")

names(data_celegans)<-c("from","to","weights")
names(data_advogato)<-c("from","to","weights")
names(data_everglades)<-c("from","to","weights")
names(data_Geom)<-c("from","to","weights")
names(data_lesmis)<-c("from","to","weights")
names(data_USAir1)<-c("from","to","weights")
names(data_USAir2)<-c("from","to","weights")
names(data_yelp)<-c("from","to","weights")
file<-c("celegans","advogato","everglades","Geom","lesmis","USAir1","USAir2","yelp")
############################################################
#check loop
i=1
LOOP<-vector()
LOOP[i]<-dim(data_celegans[from==to])[1]
i=i+1
LOOP[i]<-dim(data_advogato[from==to])[1]
i=i+1
LOOP[i]<-dim(data_everglades[from==to])[1]
i=i+1
LOOP[i]<-dim(data_Geom[from==to])[1]
i=i+1
LOOP[i]<-dim(data_lesmis[from==to])[1]
i=i+1
LOOP[i]<-dim(data_USAir1[from==to])[1]
i=i+1
LOOP[i]<-dim(data_USAir1[from==to])[1]
i=i+1
LOOP[i]<-dim(data_yelp[from==to])[1]
rm(i)
#####################################################
data<-list()
for(i in 1:8){
  data[[i]]<-data.table()
}
i=1
data[[i]]<-data_celegans
i=i+1
data[[i]]<-data_advogato
i=i+1
data[[i]]<-data_everglades
i=i+1
data[[i]]<-data_Geom
i=i+1
data[[i]]<-data_lesmis
i=i+1
data[[i]]<-data_USAir1
i=i+1
data[[i]]<-data_USAir2
i=i+1
data[[i]]<-data_yelp
######################################################
#statistic
count_from<-vector()
count_to<-vector()
count_node<-vector()
max_node<-vector()
count_edge<-vector()

for(i in 1:8){
  if(min(data[[i]]$from)==0||min(data[[i]]$to)==0){
    data[[i]]$from=data[[i]]$from+1
    data[[i]]$to=data[[i]]$to+1
  }
  count_from[i]<-length(unique(data[[i]]$from))
  count_to[i]<-length(unique(data[[i]]$to))
  temp<-c(data[[i]]$from,data[[i]]$to)
  count_node[i]<-length(unique(temp))[1]
  max_node[i]<-max(temp)
  count_edge[i]<-dim(data[[i]])[1]
}
statistics<-data.frame(dataset=file,count_from,count_to,count_node,
                       count_edge,max_node)

#eraseloop
for(i in 1:6){
  data[[i]]<-filter(data[[i]],from!=to)
  
}


############################################################
#output
if(dir.exists("ori")){
  dir.create("ori")
  setwd("ori")
}
write.csv(data_celegans,"ori_celegans.csv",row.names = F)
write.csv(data_advogato,"ori_advogato.csv",row.names = F)
write.csv(data_everglades,"ori_everglades.csv",row.names = F)
write.csv(data_Geom,"ori_Geom.csv",row.names = F)
write.csv(data_lesmis,"ori_lesmis.csv",row.names = F)
write.csv(data_USAir1,"ori_USAir1.csv",row.names = F)
write.csv(data_USAir2,"ori_USAir2.csv",row.names = F)
write.csv(data_yelp,"ori_yelp.csv",row.names = F)


