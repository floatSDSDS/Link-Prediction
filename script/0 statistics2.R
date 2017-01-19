#statistics 2

count_from<-vector()
count_to<-vector()
count_node<-vector()
max_node<-vector()
count_edge<-vector()


for(i in 1:8){
  count_from[i]<-length(unique(dataset[[i]]$data$from))
  count_to[i]<-length(unique(dataset[[i]]$data$to))
  temp<-c(dataset[[i]]$data$from,dataset[[i]]$data$to)
  count_node[i]<-length(unique(temp))[1]
  max_node[i]<-max(temp)
  count_edge[i]<-dim(dataset[[i]]$data)[1]
}
statistics2<-data.frame(dataset=file,count_from,count_to,count_node,
                       count_edge,max_node)