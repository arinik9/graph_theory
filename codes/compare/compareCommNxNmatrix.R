#!/usr/bin/env Rscript
library(igraph)
weighted_or_unweighted="weighted"
path_edgelist="../../ALL_networks/undirected/"
path="../../memberships/"
target=paste0("../../comparisons/NxNmatrix_",weighted_or_unweighted,"_undirected/")
dir.create(target, showWarnings = FALSE)

algo_list=c("louvain", "walktrap", "infomap")
methods = c("vi", "nmi", "split.join", "rand", "adjusted.rand")

counter=1
file_list=strsplit(list.files(path_edgelist, pattern=".edgelist"), "\\.")
g_list=c()
while(counter<=length(file_list)){
    g_list[counter]=file_list[[counter]][1]
    counter=counter+1
}#g_list=["call", "sms" ... gibi]

counter=1

#length(g_list) X length(g_list) boyutlu matrisleri (hangi algoritmalar varsa algo_list'de, o sayida) initialize ediyoruz
for(algo in 1:length(methods)){
  len_g=length(g_list)
  matrix_name=paste("matrix_", methods[algo], sep="")
  g=matrix(0, nrow=len_g, ncol=len_g)
  assign(matrix_name,g)
}

for(algo in algo_list){
  for(j in 1:length(g_list)){
     tab=read.table(paste(path,algo, "/", weighted_or_unweighted, "/", g_list[j],"_",weighted_or_unweighted,"_","undirected_membership.csv", sep=""), header=TRUE)
     mem1=tab[,1]
     for(i in 1:length(g_list)){
       if(i>=j){
         b=read.table(paste(path,algo, "/", weighted_or_unweighted, "/", g_list[i],"_",weighted_or_unweighted,"_","undirected_membership.csv", sep=""), header=TRUE)
         mem2=b[,1]

         f2 = function(x, mem1, mem2, i, j, algo){
           res=compare.communities(comm1=mem1, comm2=mem2, method=x)   
         }
	 dobind=as.vector(rbind(do.call(rbind, lapply(methods, f2, mem1, mem2, i, j, algo))))#length'i length(methods) olan vektor
	 for(v in 1:length(methods)){
           matrix_name=paste("matrix_", methods[v], sep="")
	   mat=get(matrix_name)
           mat[j,i]=dobind[v]
	   mat[i,j]=dobind[v]
           assign(matrix_name, mat) 
	 }
       }
     }
  }
  #write.csv(x=res, file=paste(target, algo, "/",weighted, "/", x, "/", a, "_", i, "_", x, "_undirected.comparison", sep=""),row.names=FALSE)
  for(v in 1:length(methods)){
    matrix_name=paste("matrix_", methods[v], sep="")
    mm=get(matrix_name)
    rownames(mm)=g_list
    colnames(mm)=g_list
    write.csv(x=mm, file=paste(target, algo, "_", methods[v], "_",weighted_or_unweighted,"_undirected_comparisonMatrix.csv", sep=""))
  }
  counter=counter+1
}
