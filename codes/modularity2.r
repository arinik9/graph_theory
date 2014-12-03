modularity2=function(community_str,g,weighted){

  if (weighted){
    if (get.edge.attribute(g,"weight")){
    
    }
    else{
      g=set.edge.attribute(g,"weight",value=1)
    }
  }
  else{
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
