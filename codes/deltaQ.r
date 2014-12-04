deltaQ=function(node, community, graph, weighted){
  #community, node'lar vektorudur: c(1, 2, 3) gibi

  #graph size
  m <- vcount(graph)
  #surekli get.edgelist() metdodunu cagirmamak icin
  #degiskene atiyoruz
  edgelist <- get.edgelist(graph)

  ### for find c_in ###
  c_in <- sum(E(induced.subgraph(graph, community))$weight)

  ### for find k_i_in ###
  #node'un komsulari
  neig <- which(edgelist[,1] == node)
  #node'un community'lerdekilerle komsulugu
  neig_com <- which(edgelist[,2][neig] %in% community)
  k_i_in <- sum(get.edge.attribute(graph, "weight")[neig_com])

  ### for find c_tot ###
  #c_in degrine gerek yok burda
  c_tot <- sum(E(graph)[from(community)]$weigh)

  ### for find k_i ###
  k_i <- sum(E(graph)[from(node)])

  #now terms of DeltaQ
  term1 <- ((c_in+k_i_in) - ((c_tot+k_i)^2/(2*m)))/(2*m)
  term2 <- (c_in - (c_tot^2)/(2*m) - (k_i^2)/(2*m))/(2*m)

  return term1 - term2






  sums=0
  m=sum(E(g)$weight)

  for ( i in coms){
    my_nodes=which(community_str==i)
    inner_tot=sum(E(induced.subgraph(g,my_nodes))$weight)
    outer_tot=sum(E(g)[from(my_nodes)]$weight)-inner_tot
    eii=(2*inner_tot)/(2*m)
    aii=((2*inner_tot )+ (outer_tot))/(2*m)
    sums=sums+(eii-(aii*aii))
  }




  return(sums)

}
