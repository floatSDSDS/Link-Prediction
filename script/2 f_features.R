# CN Salton Jaccard Sorenson HPI HDI LHN-I AA RA PA
f_CN<-function(x,y){
  CommonNeighbour<-intersect(SimData$Nodes[[x]]$neighboursVal,
                             SimData$Nodes[[y]]$neighboursVal)
  return(length(CommonNeighbour))
}


f_Salton<-function(key1,key2,CN){
  return(CN/sqrt(key1*key2))
}

f_Jaccard<-function(x,y,CN){
  UnionNeighbour<-union(SimData$Nodes[[x]]$neighboursVal,
                        SimData$Nodes[[y]]$neighboursVal)
  return(CN/(length(UnionNeighbour)+2))
}

f_Sorenson<-function(key1,key2,CN){
  return(2*CN/(key1+key2))
}

f_HPI<-function(key1,key2,CN){
  return(CN/min(key1,key2))
}

f_HDI<-function(key1,key2,CN){
  return(CN/max(key1,key2))
}

f_LHNI<-function(key1,key2,CN){
  return(CN/(key1*key2))
}

f_PA<-function(key1,key2){
  return(key1*key2)
}

f_AA<-function(x,y){
  CommonNeighbour<-intersect(SimData$Nodes[[x]]$neighboursVal,
                             SimData$Nodes[[y]]$neighboursVal)
  if(length(CommonNeighbour)==0){
    return(0)
  }
  Val<-vector()
  for(neighbour_ith in 1:length(CommonNeighbour)){
    index<-which(SimData$NodeIndex==CommonNeighbour[neighbour_ith])
    Val[neighbour_ith]<-SimData$Nodes[[index]]$key
  }
  return(sum(log10(Val)))
}

f_RA<-function(x,y){
  CommonNeighbour<-intersect(SimData$Nodes[[x]]$neighboursVal,
                             SimData$Nodes[[y]]$neighboursVal)
  if(length(CommonNeighbour)==0){
    return(0)
  }
  Val<-vector()
  for(neighbour_ith in 1:length(CommonNeighbour)){
    index<-which(SimData$NodeIndex==CommonNeighbour[neighbour_ith])
    Val[neighbour_ith]<-SimData$Nodes[[index]]$key
  }
  return(sum(1/Val))
}


##### path based features
f_LocalPath<-function(x,y,lambda=0.5){
  adjacency<-SimData$adjacency
  sim<-adjacency%*%adjacency+lambda*(adjacency%*%adjacency%*%adjacency)
  return(sim[x,y])
}

f_Katz<-function(x,y,lambda=0.01,level=2){
  sim=lambda*SimData$adjacency
  if(level>=2){
    for(level_ith in 2:level){
      power_ith<-level_ith
      power_adj<-SimData$adjacency
      for(power_ith in 2:level_ith){
        power_adj<-power_adj%*%SimData$adjacency
      }
      sim=sim+lambda^(level_ith)*power_adj
    } 
  }
  return(sim[x,y])
}

##### degree based features

f_diffdeg<-function(key1,key2){
  return(abs(key1-key2))
}


