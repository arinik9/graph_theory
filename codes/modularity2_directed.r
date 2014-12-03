#modularity for directed graph

modularity2_directed=function(community_str,g,weighted){

  if (weighted){
    if (get.edge.attribute(g,"weight")[1]){ #get.edge.attribute(g,"weight")[1] => sadece ilk elemanin bilgisi yeterli gibi dusunebiliriz
    
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
    outer_tot=sum(E(g)[to(my_nodes)]$weight)-inner_tot #adj() ya da from() da kullanabilirdik aslinda ?
    eii=(inner_tot)/(m)
    aii=((inner_tot) + (outer_tot))/(m)
    sums=sums+(eii-(aii*aii))
  }


  return(sums)

}
