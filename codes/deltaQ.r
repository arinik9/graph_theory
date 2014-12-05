deltaQ=function(node, community, g, weighted){
  #community, node'lar vektorudur: c(1, 2, 3) gibi

  #graph size
  m <- sum(E(g)$weight)

  ### for find c_in ###
  c_in <- sum(E(induced.subgraph(g, community))$weight)

  ### for find k_i_in ###
  k_i_in <- sum(g[node, community])

  ### for find c_tot ###
  c_tot <- sum(E(g)[from(community) | to(community)]$weight)

  ### for find k_i ###
  k_i <- sum(E(g)[from(node)]$weight)

  #the terms of DeltaQ
  term1 <- ((c_in+k_i_in) - ((c_tot+k_i)^2/(2*m)))/(2*m)
  term2 <- (c_in - (c_tot^2)/(2*m) - (k_i^2)/(2*m))/(2*m)

  return term1 - term2
}
