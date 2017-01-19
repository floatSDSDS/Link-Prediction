f_getIndex<-function(x){
  return(which(SimData$NodeIndex==x))
}

f_getKey<-function(x){
  return(SimData$Nodes[[x]]$key)
}

f_undirected<-function(x,y){
  index1<-which(temp$from==x&temp$to==y)
  index2<-which(temp$to==x&temp$from==y)
  if(length(index2)!=0){
    if(index1<index2){
      weight<-temp$weight[index1]+temp$weight[index2]
    }
    else{
      weight<--1
    }
  }
  else{
    weight<-temp$weight[index1]
  }
  return(weight)
}


f_linearnorm2<-function(feature){
  delta=max(feature)-min(feature)
  temp<-(feature-min(feature))/delta
  return(temp)
}

