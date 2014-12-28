##DeltaQ is a function in Louvain ALgorithm
##DeltaQ is the gain in modularity
##Value of DeltaQ is obtained by moving an isolated node into a community

#Requirement:
  #g is a graph and MUST be weighted.
  #If E(g3)$weight is NULL, so we affect 1 to edges of graph => so E(g3)$weight = 1
#INPUT:
  #community is a vector of nodes as c(1, 2, 3)
  #g is a weighted graph

#g is a network, it is expressed as adjacency matrix. g[,] gives all links 
#node is the node ID that is under interest. We will search the best community for node according to deltaQ score
#communtiy is a node group, a.k.a a vector of node IDs in R. 

#function deltaQ scores what happens when we put node into community. 

deltaQ = function(g, node, community){
#matris kullanimi ile hem weighted hem directed hem unweighted hem undirected'ta calisacak ortak fonksiyon bu sekilde olabilir.
	m=sum(g[,]) #aslinda 2m, yani mesela hem 2->1 hem 1->2 var
	### for find c_in ###
    	#c_in: the sum of the weights of the links inside community
	c_in <- sum(g[community,community])
	### for find k_i_in ###
        #the sum of the weights of the links from node(which is in parameters) to nodes in community
    #from node to nodes dedigi icin toplamanin geri kalanini yorum icinde biraktim
  	k_i_in <- sum(g[node, community])#+sum(g[community,node])
	### for find c_tot ###
        #the sum of the weights of the links incident to nodes in community
	c_tot <- sum(g[,community])+sum(g[community,])-sum(g[community,community])#self loop'lar da dahil
  	### for find k_i ###
        #the sum of the weights of the links incident to node(which is in parameters)
	k_i <- sum(g[node,])+sum(g[,node])#node'un kendi self loop'u da dahil

	#term1 <- ((c_in+(2*k_i_in))/m) - (((c_tot+k_i)/m)^2)
	#term2 <- (c_in/m) - ((c_tot/m)^2) - ((k_i/m)^2)

	#formulu genisletirsek 1. terimden ve 2. terimden bazi degiskenler biribirini goturuyor ve su haliyle ele aliriz:
	term1 <- (2*k_i_in)/m
	term2 <- 2*(c_tot*k_i)/(m*m)
	
	return(term1-term2)
}
