
f_preprocessed<-function(temp){
  
  SimData<-list()
  
  # check capability
  if(max(min(temp$weight))<0){
    #print sth
    break;
  }
  # # add one to nodes if needed
  # if(min(temp$from)==0|min(temp$to)==0){
  #   temp[,1:2]=temp[,1:2]+1
  # }
  # remove loops
  temp<-filter(temp,from!=to)
  # make the network undirected with add up weights between same couple of nodes
  weight<-mapply(f_undirected,temp$from,temp$to)
  temp$weight<-weight
  temp<-filter(temp,weight>0)
  SimData$Ori<-temp
  
  # nodes infomation
  SimData$Count_Edge<-dim(SimData$Ori)[1]
  nodes<-c(temp$from,temp$to)
  nodes<-unique(nodes)
  SimData$NodeIndex<-nodes
  SimData$CountNode<-length(nodes)
  for(j in 1:length(nodes)){
    SimData$Nodes[[j]]<-list()
    nodeNo<-nodes[j]
    SimData$Nodes[[j]]$NodeVal<-nodeNo
    SimData$Nodes[[j]]$neighbours<-filter(temp,from==nodeNo|to==nodeNo)
    SimData$Nodes[[j]]$neighboursVal<-unique(c(SimData$Nodes[[j]]$neighbours$from,
                                               SimData$Nodes[[j]]$neighbours$to))
    SimData$Nodes[[j]]$neighboursVal<-setdiff(SimData$Nodes[[j]]$neighboursVal,
                                              SimData$Nodes[[j]]$NodeVal)
    SimData$Nodes[[j]]$key<-dim(SimData$Nodes[[j]]$neighbours)[1]
  }
  return(SimData)
}

f_addBasic<-function(SimData){
  SimData$Ori$IndexFrom<-sapply(SimData$Ori$from,f_getIndex)
  SimData$Ori$IndexTo<-sapply(SimData$Ori$to,f_getIndex)
  SimData$Ori$Key1<-sapply(SimData$Ori$IndexFrom,f_getKey)
  SimData$Ori$Key2<-sapply(SimData$Ori$IndexTo,f_getKey)
  SimData$Ori$diffDeg<-mapply(f_diffdeg,SimData$Ori$Key2,SimData$Ori$Key1)
  SimData$Ori$CN<-mapply(f_CN,SimData$Ori$IndexFrom,SimData$Ori$IndexTo)
  
  SimData$features<-SimData$Ori[,1:3]
  
  
  # get sparse adj matrix
  temp<-SimData$Ori[,1:3]
  SimData$Graph<-graph.data.frame(temp[1:3]) 
  SimData$adjacency<-as_adjacency_matrix(SimData$Graph,type = "upper")
  SimData$transitivity<-transitivity(SimData$Graph)
  SimData$density<-graph.density(SimData$Graph)
  return(SimData)
}

f_addLocal<-function(SimData){
  #########################
  
  SimData$features$diffDeg<-mapply(f_diffdeg,SimData$Ori$Key2,SimData$Ori$Key1)
  SimData$features$CN<-mapply(f_CN,SimData$Ori$IndexFrom,SimData$Ori$IndexTo)
  SimData$features$Salton<-mapply(f_Salton,SimData$Ori$Key1,SimData$Ori$Key2,SimData$Ori$CN)
  SimData$features$Jaccard<-mapply(f_Jaccard,SimData$Ori$IndexFrom,SimData$Ori$IndexTo,SimData$Ori$CN)
  SimData$features$Sorenson<-mapply(f_Sorenson,SimData$Ori$Key1,SimData$Ori$Key2,SimData$Ori$CN)
  SimData$features$HPI<-mapply(f_HPI,SimData$Ori$Key1,SimData$Ori$Key2,SimData$Ori$CN)
  SimData$features$HDI<-mapply(f_HDI,SimData$Ori$Key1,SimData$Ori$Key2,SimData$Ori$CN)
  SimData$features$LHNI<-mapply(f_LHNI,SimData$Ori$Key1,SimData$Ori$Key2,SimData$Ori$CN)
  SimData$features$AA<-mapply(f_AA,SimData$Ori$IndexFrom,SimData$Ori$IndexTo)
  SimData$features$RA<-mapply(f_RA,SimData$Ori$IndexFrom,SimData$Ori$IndexTo)
  SimData$features$PA<-mapply(f_PA,SimData$Ori$Key1,SimData$Ori$Key2)
  
  return(SimData)
}

f_addDegree<-function(SimData){
  mdeg<-degree(SimData$Graph,v=SimData$NodeIndex,normalized = T)
  mcloseness<-closeness(SimData$Graph,normalized = T)
  mbetweenness<-betweenness(SimData$Graph,normalized = T)
  medge_betweenness<-edge_betweenness(SimData$Graph)
  mevcent<-evcent(SimData$Graph,scale=T)
  mpage_rank<-page.rank(SimData$Graph)
  
  
  if(!any((as.numeric(names(mdeg))-SimData$NodeIndex)!=0)){
    SimData$deg_from<-cbind(from=SimData$NodeIndex,deg1=mdeg,
                            CLS1=mcloseness,BTS1=mbetweenness,
                            evcent1=mevcent$vector,page_rank1=mpage_rank$vector)
    
    SimData$deg_to<-data.frame(to=SimData$NodeIndex,deg2=mdeg,
                               CLS2=mcloseness,BTS2=mbetweenness,
                               evcent2=mevcent$vector,page_rank2=mpage_rank$vector)
  }
  
  SimData$features$edge_BTS<-medge_betweenness
  SimData$Ori<-left_join(SimData$Ori,SimData$deg_from,by="from")
  SimData$Ori<-left_join(SimData$Ori,SimData$deg_to,by="to")
  
  return(SimData)
  
}

f_addPath<-function(SimData,level_Katz=5){
  
  SimData$features$LocalPath<-mapply(f_LocalPath,SimData$Ori$IndexFrom,SimData$Ori$IndexTo)
  SimData$features$Katz<-mapply(f_Katz,SimData$Ori$IndexFrom,SimData$Ori$IndexTo,level=level_Katz)
  
  return(SimData)
}
