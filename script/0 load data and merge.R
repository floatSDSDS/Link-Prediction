path<-"L:/7sem/YELP/LinkPrediction/DataSet"
setwd(path)
files<-list.files()
files<-files[1:6]
file<-c("celegans","everglades","geom","lesmis","usair1","yelp")

#read in data
dataset<-list()



data2<-list()
index<-c(1,3,4,5,6,8)
for(i in 1:6){
  data2[[i]]<-data.frame()
  data2[[i]]<-data[[index[i]]]
}

for(i in 1:6){
  setwd(files[i])
  dataset[[i]]<-list()
  dataset[[i]]$data<-data2[[i]]
  
  setwd("result")
  results<-list.files()
  length_DSname<-nchar(file[i])
  name<-gsub("...........$","",results)
  length_methodtemp<-nchar(name)
  name<-substr(name,length_DSname+1,length_methodtemp)
  dataset[[i]]$features<-list()
  for(j in 1:length(results)){
    dataset[[i]]$features[[j]]<-data.frame()
    temp<-fread(results[j])
    dataset[[i]]$features[[j]]<-data.frame(temp)
    dataset[[i]]$data<-cbind(dataset[[i]]$data,temp$V3)
   }
  names(dataset[[i]]$data)[4:length(dataset[[i]]$data)]<-name
  names(dataset[[i]]$data)[1:3]<-c("from","to","weight")
  setwd(path)
}


##
#norm
for(i in 1:6){
  len<-dim(dataset[[i]]$data)[2]
  if(len>3){
    for(j in 4:len){
      dataset[[i]]$data[[j]]=f_linearnorm2(dataset[[i]]$data[[j]])  
    } 
  }
}



f_linearnorm2<-function(feature){
    delta=max(feature)-min(feature)
    temp<-(feature-min(feature))/delta
    return(temp)
}