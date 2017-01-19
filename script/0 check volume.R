#0 check if volume


f_CheckVolume<-function(x,y,i){
  #return TRUE or False vector
  a<-data[[i]][from==y][to==x]
  if(dim(a)[1]==0){
    return(FALSE)
  }
  else{
    return(TRUE)
  }
}

f_DEdirected<-function(x,y,z){
  get<-temp[from==y][to==x]
  if(dim(get)[1]==0){
    return(FALSE)
    z<-temp[from==x][to==y]$weight
  }
  else{
    z=sum(get$weight)
    return(TRUE)
  }
}


i=3
data_everglades<-filter(data_everglades,from!=to)
dataset[[i]]$data[[3]]<-data_everglades$weights



##
#everglades
i=2
dataset[[i]]$data<-filter(dataset[[i]]$data,from!=to)
temp<-dataset[[i]]$data
temp<-data.table(temp)
a<-temp$weight
TF<-mapply(f_DEdirected,temp$from,temp$to,a)
temp<-mutate(temp,flag=TF)


get<-filter(temp,flag==T)
get<-mutate(get,undirectedW=0)
for(i in 1:dim(get)[1]){
  x=get$from[i]
  y=get$to[i]
  weight1=get$weight[i]
  weight2<-filter(get,from==y&to==x)$weight
  get$undirectedW[i]=weight1+weight2
}
get$weight<-get$undirectedW
get<-select(get,-undirectedW,-flag)

for(i in 1:dim(get)[1]){
  x=get$from[i]
  y=get$to[i]
  a<-which(get$from==y&get$to==x)
  if(a>i){
    get<-get[-a,] 
  }
}
temp<-filter(temp,flag==F)
temp<-select(temp,-flag)
temp<-rbind(temp,get)
temp<-arrange(temp,from,to)
dataset[[2]]$data<-temp

count_volume<-vector()
for (i in 1:8){
  temp<-mapply(f_CheckVolume,data[[i]]$from,data[[i]]$to,i)
  count_volume[i]=sum(temp)
}

statistics<-cbind(statistics,count_volume)
statistics<-cbind(statistics,count_loop=LOOP)

write.csv(statistics,"ori_statistics.csv",row.names = F)


for(i in 1:6){
  write.csv(dataset[[i]]$data,paste(file[i],"_feature.csv",sep=""),row.names = F)
}


