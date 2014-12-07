modularity2=function(g, community_str){
  if (length(get.edge.attribute(g,"weight")) == 0){ 
    #eger graph unweighted ise link saysisini weight olarak gosteririz
    g=set.edge.attribute(g,"weight",value=1)
  }

  coms=unique(community_str)
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
