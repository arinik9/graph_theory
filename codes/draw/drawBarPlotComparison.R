#!/usr/bin/env Rscript

library(igraph)
target="../../hist/" #Nokia'nin ana dizini
path="../../comparisons/"
dir.create(target, showWarnings = FALSE)

networks=c("bt","call_bt_undirected","call_sms_bt_undirected","call_sms_undirected","call_undirected", "sms_bt_undirected","sms_undirected")

network_labels=c("bt","call\nbt","call\nsms\nbt","call\nsms","call","sms\nbt","sms")

algo_list=c("infomap", "infomap", "louvain", "louvain", "walktrap", "walktrap")
weight_list=c("weighted","unweighted","weighted","unweighted","weighted","unweighted")
method_list=c("adjusted.rand","nmi","rand")

#names(aa)=c("0.25\nbt\nwlan\ncall\nsms","0.58\ncall\nsms\nundirected",rep("AB",25))
color=c("skyblue1","orange","mistyrose","indianred","skyblue4")
#barplot(aa,col=color,cex.names=0.2, space=2)

for (i in seq(1,length(algo_list))){
  for(method in method_list){
  
    f2 = function(network, weight, method, algo)
      res=as.double(read.table(paste(path,algo,"/",weight,"/",method,"/",algo,"_",network,"_",method,"_",weight,"_","undirected_comparison.csv",sep="")))
    #length'i length(methods) olan vektor
	  dobind=as.vector(rbind(do.call(rbind, lapply(networks, f2, weight_list[i], method, algo_list[i]))))
    #names(dobind)=network_labels
    b=strsplit(networks,"_")
    lens=c()
    for(z in seq(1,length(networks))){
      len=length(b[[z]])
      if(b[[z]][len] == "undirected"){
	len=len-1;
      }
      lens=cbind(lens,color[len])
    }
    png(paste(target,algo_list[i],"_",weight_list[i],"_",method,".png",sep=""),width = 1380, height = 1680)
    bp=barplot(dobind,col=lens,cex.names=3.8, ylim = c(0, 1), space=0.6,legend = c("1li komun", "2li komun", "3lu komun"), args.legend = list(title = "COMS", x = "topright", cex = 2))#names.arg = network_labels
    text(bp, 0, paste0(round(dobind, 3),"\n\n",network_labels),cex=2.4,pos=3)
    dev.off()
  }
}

#	pdf(paste(target_image,i,".pdf",sep=""))
#	plot(lv,g,layout=l,vertex.label=NA,vertex.size=6)
#	title(paste(graph_names[counter]," Grafi ve Louvain Algoritmasinin \nBuldugu Topluluklar\n Q= ", toString( round( lv$modularity[length(lv$modularity)],digits=2) ) ) )
	#legend("topright", title="moddularity")
#	dev.off()




