#deloop and undirected

#deloop
for (i in 1:8){
  data[[i]]$deloop<-filter(data[[i]]$ori,from!=to)
}

#undirected
weight<-list()
j=1
for(i in c(2,3,7)){
  weight[[j]]<-mapply(f_undirected,data[[i]]$deloop$from,data[[i]]$deloop$to,i)
  j=j+1
}

j=1
for(i in c(2,3,7)){
  data[[i]]$deloop$weight_t<-weight[[j]]
  data[[i]]$undirected<-filter(data[[i]]$deloop,weight_t>0)%>%
    select(from,to,weight=weight_t)
  j=j+1
}

DSname<-c("advogato.csv","everglade.csv","USAir2.csv")
j=0
for(i in c(2,3,7)){
  j=j+1
  write.csv(data[[i]]$undirected,DSname[j])
}


f_undirected<-function(x,y,i){
  len1<-which(data[[i]]$deloop$from==x&data[[i]]$deloop$to==y)
  len2<-which(data[[i]]$deloop$to==x&data[[i]]$deloop$from==y)
  if(length(len2)!=0){
    if(len1<len2){
      weight<-data[[i]]$deloop$weights[len1]+data[[i]]$deloop$weights[len2]
    }
    else{
      weight<--1
    }
  }
  else{
    weight<-data[[i]]$deloop$weights[len1]
  }
  return(weight)
}
