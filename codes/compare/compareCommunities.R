#!/usr/bin/env Rscript

library(igraph)
path="../../ALL_networks/undirected/"
target="../.." #Nokia'nin ana dizini

target_memberships=file.path(target, "memberships", fsep="/")
dir.create(target_memberships, showWarnings = FALSE)
target_comparisons=file.path(target, "comparisons", fsep="/")
dir.create(target_comparisons, showWarnings = FALSE)

gt_path="../../Nokia_ground_truth.txt"

algo_list=c("louvain", "louvain", "walktrap", "walktrap", "infomap", "infomap", "infomap", "infomap")
weighted_list=c(TRUE, FALSE, TRUE, FALSE, FALSE, FALSE, TRUE, TRUE)

ground_truth=read.table(gt_path)$V1

#Edgelist uzantili dosyalarin isimlerini aliyoruz. Elle, manual yapmadan g_list'i dolduruyoruz
counter=1
file_list=strsplit(list.files(path, pattern=".edgelist"), "\\.")
g_list=c()
while(counter<=length(file_list)){
    g_list[counter]=file_list[[counter]][1]
    counter=counter+1
}#g_list=["call", "sms" ... gibi]

counter=1

for(a in algo_list){
for(i in g_list){
cat("111 ", paste(path,i,".edgelist", sep=""), "\n")
  gr=read.graph(paste(path,i,".edgelist",sep=""), format=c("edgelist"), n=137, directed=FALSE)
  gr=delete.vertices(gr,1) # yani totalde 136 node olcak
  gr_weight=read.table(paste(path,i,".weight",sep=""))$V1
  g=as.undirected(gr)

  E(g)$weight=ifelse(weighted_list[counter], gr_weight, 1)
  weighted=ifelse(weighted_list[counter], "weighted", "unweighted")

  cat("length of ", i, vcount(g), "\n")
  lv=NA

  if(a=="louvain"){
    lv= multilevel.community(graph=g,weights=E(g)$weight)
  }
  else if(a=="walktrap"){
    lv=walktrap.community(graph=g, weights=E(g)$weight)
  }
  else if(a=="infomap"){
    lv=infomap.community(graph=g,e.weights=E(g)$weight)
  }


  dir.create(file.path(target_memberships, a, fsep="/"), showWarnings = FALSE)
  dir.create(file.path(target_memberships, a, weighted, fsep="/"), showWarnings = FALSE)
  write.table(x=lv$membership,file=paste(target_memberships,"/",a, "/", weighted, "/", i, "_", weighted, "_", "undirected_membership.csv", sep=""), row.names=FALSE, col.names=FALSE)

  print(i)
  cm = c("vi", "nmi", "split.join", "rand", "adjusted.rand")
  f2 = function(x, i, a, lv, w){
    res=compare.communities(comm1=ground_truth, comm2=lv$membership, method=x)
    dir.create(file.path(target_comparisons, a, fsep="/"), showWarnings = FALSE)
    dir.create(file.path(target_comparisons, a, weighted, fsep="/"), showWarnings = FALSE)
    dir.create(file.path(target_comparisons, a, weighted, x, fsep="/"), showWarnings = FALSE)
    write.table(x=res, file=file.path(target_comparisons, a, weighted, x, paste(a, "_", i, "_", x, "_", weighted, "_undirected_comparison.csv", sep=""), fsep="/"), row.names=FALSE, col.names=FALSE)
  }
  sapply(cm, f2, i, a, lv, weighted)
}
counter=counter+1
}
