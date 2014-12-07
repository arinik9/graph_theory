library(igraph)
#importing files
source("/home/nejat/thesis/github/graph_theory/codes/deltaQ.r")
source("/home/nejat/thesis/github/graph_theory/codes/modularity2.r")

### TO DO - SELF_LOOP ile ilgili bir sıkıntı cıkıp cıkmama olasılıgına bak!
  #nb_selfLoops = function(g, node){
  #  res <- which(node == neighbors(g, node))
  #  if(lenght(res) == 0){
  #    return(0)
  #  }
  #  else{
  #    return(g[node, node])
  #  }
  #}

##read a graph - begin
g <- read.graph("/home/nejat/thesis/data/toydata.edgelist", format=c("edgelist"), directed=FALSE)
E(g)$weight <- read.table("/home/nejat/thesis/data/toydata.weight")$V1
## end

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
      
      #if any neighboring communities of current node exist
      if(length(ncomm) != 0){
        #remove node from its current community
        comm_str[node] <- -1
        
        best_comm <- node_comm
        best_increase <- 0
        for(i in 1:length(ncomm)){
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
          cat(sprintf("pass_number: %d of %d (nb_pass) => new_mod=%s, cur_mod=%s\n", nb_pass_done, nb_pass, new_mod, cur_mod))
    cat("community structure: ", comm_str, "\n")
        }
      }
    }
    memberships <- c(memberships, comm_str)
  }

  return(list("modularity" = new_mod, "membership" = comm_str, "memberships" = matrix(memberships,nrow=nb_pass_done)))
}
