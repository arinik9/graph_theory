deltaQ=function(node, community, community_str, g, weighted){
  #community, node'lar vektorudur: c(1, 2, 3) gibi

  #graph size
  m <- sum(E(g)$weight)
  #surekli get.edgelist() metdodunu cagirmamak icin
  #degiskene atiyoruz
  e <- get.edgelist(g)

  ### for find c_in ###
  c_in <- sum(E(induced.subgraph(g, community))$weight)

  ### for find k_i_in ###
  #node'un komsulari
  #neig <- which(edgelist[,1] == node)
  neig <- neighbors(g,node)
  #node'un community'lerdekilerle komsulugu
  #neig_com <- which(edgelist[,2][neig] %in% community)
  in_neig <- neig[which(community_str[neig] == community)]
  

    edges <- e[which(e[which(e[,1]==node),2]==in_neig),]
  
  k_i_in <- E(g)[from(node)&to(in_neig)]$weight + E(g)[from(in_neig)&to(node)]$weight
  #k_i_in <- sum(get.edge.attribute(g, "weight")[neig_com])

  ### for find c_tot ###
  #c_in degrine gerek yok burda
  c_tot <- sum(E(g)[from(community)]$weight)

  ### for find k_i ###
  k_i <- sum(E(g)[from(node)]$weight)

  #now terms of DeltaQ
  term1 <- ((c_in+k_i_in) - ((c_tot+k_i)^2/(2*m)))/(2*m)
  term2 <- (c_in - (c_tot^2)/(2*m) - (k_i^2)/(2*m))/(2*m)

  return term1 - term2

}
