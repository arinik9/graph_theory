#!/usr/bin/env Rscript

library(igraph)
target="../../" #Nokia'nin ana dizini
path="../../ALL_networks/undirected/"

target_image=file.path(target, "images/", fsep="/")
dir.create(target_image, showWarnings = FALSE)

g_list=c("bt","call_bt_undirected","call_sms_bt_undirected", "call_sms_undirected","call_undirected","sms_bt_undirected","sms_undirected")

graph_names=c("Bluetooth","Call ve Bluetooth (undirected)","Call, Sms ve Bluetooth (undirected)","Call ve Sms (undirected)","Call (undirected)", "Sms ve Bluetooth (undirected)","Sms (undirected)")

########## BU KISMI SADECE LAYOUT'U ALABILMEK ICIN YAPIYORUZ #############
i=g_list[3]
gr=read.graph(paste(path,i,".edgelist",sep=""), format=c("edgelist"), n=137, directed=FALSE)
gr=delete.vertices(gr,1)
gr_weight=read.table(paste(path,i,".weight",sep=""))$V1
E(gr)$weight=gr_weight
g=as.undirected(gr)
lv=multilevel.community(g,E(g)$weight)
l=layout.fruchterman.reingold(g,lv)
##################################################################

counter=1;
for ( i in g_list){
	#her halukarda 1. node'u kendi atiyor. O yuzden en son 1 tane silebilmek icin n=137 yaptik
	gr=read.graph(paste(path,i,".edgelist",sep=""), format=c("edgelist"), n=137, directed=FALSE)	
	gr=delete.vertices(gr,1)
	gr_weight=read.table(paste(path,i,".weight",sep=""))$V1
	E(gr)$weight=gr_weight
	g=as.undirected(gr)
	lv=multilevel.community(g,E(g)$weight)
	#write membership relation into file
	write.csv(lv$membership,file=paste(path, i, "_undirected.membership", sep=""), row.names=FALSE)
	pdf(paste(target_image,i,".pdf",sep=""))
	plot(lv,g,layout=l,vertex.label=NA,vertex.size=6)
	title(paste(graph_names[counter]," Grafi ve Louvain Algoritmasinin \nBuldugu Topluluklar\n Q= ", toString( round( lv$modularity[length(lv$modularity)],digits=2) ) ) )
	#legend("topright", title="moddularity")
	dev.off()

	png(paste(target_image,i,".png",sep=""))
	plot(lv,g,layout=l,vertex.label=NA,vertex.size=6)
	title(paste(graph_names[counter]," Grafi ve Louvain Algoritmasinin \nBuldugu Topluluklar\n Q= ", toString(round(lv$modularity[length(lv$modularity)],digits=2))))
	dev.off()
counter=counter+1;
}




