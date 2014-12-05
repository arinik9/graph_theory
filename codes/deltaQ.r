##DeltaQ is a function in Louvain ALgorithm
##DeltaQ is the gain in modularity
##Value of DeltaQ is obtained by moving an isolated node into a community

#Requirement:
  #g is a graph and MUST be weighted.
  #If E(g3)$weight is NULL, so we affect 1 to edges of graph => so E(g3)$weight = 1
#INPUT:
  #community is a vector of nodes as c(1, 2, 3)
  #g is a weighted graph
deltaQ = function(node, community, g){
  #m: graph size OR the sum of the weights of all the links in the network
  m <- sum(E(g)$weight)

  ### for find c_in ###
    #c_in: the sum of the weights of the links inside community
  c_in <- sum(E(induced.subgraph(g, community))$weight)

  ### for find k_i_in ###
    #the sum of the weights of the links from node(which is in parameters) to nodes in community
  k_i_in <- sum(g[node, community])

  ### for find c_tot ###
    #the sum of the weights of the links incident to nodes in community
  c_tot <- sum(E(g)[from(community) | to(community)]$weight)

  ### for find k_i ###
    #the sum of the weights of the links incident to node(which is in parameters)
  k_i <- sum(E(g)[from(node)]$weight)

  #the terms of DeltaQ
  term1 <- ((c_in+k_i_in) - ((c_tot+k_i)^2/(2*m)))/(2*m)
  term2 <- (c_in - (c_tot^2)/(2*m) - (k_i^2)/(2*m))/(2*m)

  return term1 - term2
}
