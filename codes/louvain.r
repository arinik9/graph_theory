library(igraph)
#importing files
source("/home/nejat/thesis/github/graph_theory/codes/deltaQ.r")
source("/home/nejat/thesis/github/graph_theory/codes/modularity2.r")

##read a graph - begin
g <- read.graph("/home/nejat/thesis/data/toydata.edgelist", format=c("edgelist"), directed=FALSE)
E(g)$weight <- read.table("/home/nejat/thesis/data/toydata.weight")$V1
## end


#function: louvain()
louvain = function(g, verbose=FALSE, nb_pass=10000, directed=FALSE){
  precision <- 0.000001
  gSize <- vcount(g)
  comm_str <- seq(1, gSize) #each node is a community at beginning
  mod <- modularity2(g, comm_str)

  lvl <- one_level(g,verbose)
  comm_str <- lvl$membership
  new_mod <- lvl$modularity
  g <- partition2graph_binary(g, comm_str, verbose, directed)
  
  while((new_mod-mod)>precision){
    mod <- new_mod
    lvl <- one_level(g,verbose)
    comm_str <- lvl$membership
    new_mod <- lvl$modularity
    g <- partition2graph_binary(g, comm_str, verbose, directed)
  }

  return(list("modularity" = new_mod, "membership" = comm_str))
}



#function: partition2graph_binary()
partition2graph_binary = function(g, comm_str, verbose=FALSE, directed=FALSE){
  if(directed){
    directed_or_undirected = "directed"
  }
  else{
    directed_or_undirected = "undirected"
  }

  different_comm <- unique(comm_str)
  length_different_comm <- length(different_comm)

  #rename communities - begin
  new_str <- vector(mode="integer",length=length(comm_str))
  final <- 1
  #length_row <- 0 # ayni zamanda, community'ler arasinda en cok node'u olanin node sayisini bulucaz
  for(node in 1:length_different_comm){
    vec <- different_comm[node]
    #secilen community'ye ait kac node oldugunu buluyoruz
    correspondant <- which(vec == comm_str)

    new_str[correspondant] <- final
    final <- final + 1

    #tum community'lere kiyasla bir community'deki max node sayisini bulma icin
    #cur_length <- length(correspondant)
    #if(length_row < cur_length){
    #  length_row <- cur_length
    #}
  }
  # rename - end

  #max node sayisini bulduktan sonra matrix'i olusturabiliriz
  #matrix'i once sifirla initialize ettik
  adj <- matrix(0,nrow=length_different_comm,ncol=length_different_comm)

  #community icindeki node'larin diger community'deki node'lar arasindaki linklere gore adj matrisini dolduruyoruz
  for(node in 1:length(new_str)){
    neighs <- neighbors(g,node)
    for(neigh in 1:length(neighs)){
      if(new_str[node] != new_str[neigh] && node != neigh && new_str[neigh]<new_str[node]){
        adj[new_str[node], new_str[neigh]] <- adj[new_str[node], new_str[neigh]] +  g[node,neigh]
        adj[new_str[neigh], new_str[node]] <- adj[new_str[neigh], new_str[node]] + g[neigh, node]
      }
    }
  }

  g1 <- graph.adjacency(mode= directed_or_undirected, adj, weighted=TRUE)
  #self loop'lari da ekliyoruz
  for(comm in 1:length_different_comm){
    g1[comm, comm] <- sum(E(induced.subgraph(g, which(new_str == comm)))$weight)
  }

  if(verbose){
    cat("in PARTITION2GRAPH_BINARY function => community structure: ", new_str, "\n")
  }  
  #graph'in node'larini bastan yaratmis olduk => eski graph'daki comm sayisi = yeni graph'daki node sayisi
  return(g1)
}


#function: one_level()
one_level = function(g, verbose=FALSE, nb_pass=10000){
  gSize <- vcount(g)
  comm_str <- seq(1, gSize) #each node is a community at beginning
  cur_mod <- new_mod <- modularity2(g, comm_str)
  min_modularity <- 0.000001

  #while init - begin
  improvement <- TRUE
  nb_pass_done <- 0
  new_mod <- new_mod+0.1 #while'a ilk girişte sorun çıkarmasın diye => cünkü new_mod must be > cur_mod
  #end

  memberships <- c()

  while(improvement && (new_mod-cur_mod)>min_modularity && nb_pass_done!=nb_pass){
    #init - begin
    improvement <- FALSE
    cur_mod <- new_mod
    nb_pass_done = nb_pass_done+1
    #init - end

    for(node_tmp in 1:gSize){
      node <- node_tmp
      node_comm <- comm_str[node]
      
      #computation of all neighboring communities of current node
      neigh <- neighbors(g, node)
      ncomm <- neigh[which(comm_str[neigh] != node_comm)]
      length_ncomm <- length(ncomm)
      #if any neighboring communities of current node exist
      if(length_ncomm != 0){
        #remove node from its current community
        comm_str[node] <- -1
        
        best_comm <- node_comm
        best_increase <- 0
        for(i in 1:length_ncomm){
          next_neigh_comm = ncomm[i]
          increase <- deltaQ(g, node, next_neigh_comm)
          if(best_increase < increase){
            best_comm <- next_neigh_comm
            best_increase <- increase
          }
        }

        #insert node in the nearest community
        comm_str[node] <- best_comm

        if(best_comm != node_comm){
          improvement <- TRUE
        }

        new_mod <- modularity2(g, comm_str)

        if(verbose){
          cat(sprintf("in ONE_LEVEL function => pass_number: %d of %d (nb_pass) => new_mod=%s, cur_mod=%s\n", nb_pass_done, nb_pass, new_mod, cur_mod))
          cat("in ONE_LEVEL function => community structure: ", comm_str, "\n")
        }
      }
    }
    memberships <- c(memberships, comm_str)#surekli memberships'in  uzerine ekliyoruz ki membvership farkliliklarini gorebilelim
  }

  return(list("modularity" = new_mod, "membership" = comm_str, "memberships" = matrix(memberships,nrow=nb_pass_done)))
}
