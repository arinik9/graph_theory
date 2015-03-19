#g <- read.graph("/home/nejat/thesis/github/graph_theory/codes/toydata.edgelist", format=c("edgelist"), directed=FALSE)
#E(g)$weight <- read.table("/home/nejat/thesis/github/graph_theory/codes/toydata.weight")$V1

#source("/home/nejat/nokia_experiments/data/toydata.edgelist")
#source("/home/nejat/nokia_experiments/data/toydata.weight")

#g <- read.graph("/home/nejat/thesis/data/toydata.edgelist", format=c("edgelist"), directed=FALSE)
#E(g)$weight <- read.table("/home/nejat/thesis/data/toydata.weight")$V1

require(parallel)

louvain = function(g, verbose=FALSE, directed=FALSE){ #nb_pass=10000, 
  precision    <- 0.000001
  gSize        <- vcount(g)
  comm_str     <- seq(1, gSize) #each node is a community at beginning
  mod          <- ifelse(directed, modularity_directed(g, comm_str, 1), modularity(g, comm_str, E(g)$weight))

  level        <- one_level(g, verbose, directed)
  comm_str     <- level$membership
  new_mod      <- level$modularity
  #ilk basta her bi node birer komun oldugu icin, global_str yerine seq(1,gSize) yazdik.
  #global_str'nin amaci, yeni hesaplanan comm_str bir onceki comm_str ile bag kurmasini saglar
  global_str   <- seq(1, gSize)
  result       <- partition2graph_binary(g, global_str, comm_str, verbose, directed)#baslangicta global_str ile comm_str ayni olcak
  g1           <- result$graph
  global_str   <- result$global_str

  memberships  <- rbind(global_str)
  mods <- rbind(ifelse(directed, modularity_directed(g, global_str, 1), modularity(g, global_str, E(g)$weight)))
  #plot(g1, edge.label=round(E(g1)$weight, 3))
  last=1
  while((new_mod-mod)>precision){
    print(paste("WHILE ici,  time: ",Sys.time()))
    mod        <- new_mod
    level      <- one_level(g1, verbose, directed)
    comm_str   <- level$membership
    new_mod    <- level$modularity
    result     <- partition2graph_binary(g1, global_str, comm_str, verbose, directed)
    g1         <- result$graph
    global_str <- result$global_str

    if(all(memberships[last,] == global_str) == FALSE){
        memberships <- rbind(memberships, global_str)
	      mods        <- rbind(mods, ifelse(directed, modularity_directed(g, global_str, 1), modularity(g, global_str, E(g)$weight)))
        last        <- last+1
    }
  }

  #plot(g1, edge.label=round(E(g1)$weight, 3))
  return(list("modularity" = mods, "membership" = global_str, "memberships" = memberships, "how many cluster?" = unique(global_str)))
}


#modularity for directed edges/graph
#modularity begin
modularity_directed=function(g, community_str,weighted){
    if (weighted){
       if (which.max(get.edge.attribute(g,"weight"))>1){}
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
	      inside_tot=sum(E(induced.subgraph(g,my_nodes))$weight)
	      outer_tot=(sum(E(g)[from(my_nodes)]$weight))
	      inner_tot=(sum(E(g)[to(my_nodes)]$weight))
	      eii=(inside_tot)/(m)
	      aii=((inner_tot*outer_tot))/(m*m)
	      sums=sums+(eii-(aii))
    }
    return(sums)
}
#modularity end


partition2graph_binary = function(g, global_str, comm_str, verbose, directed){
  directed_or_undirected = ifelse(directed,"directed","undirected")

  if(length(unique(global_str)) == length(comm_str) && all(unique(global_str) == comm_str)){
  #Ayni gelmemesi lazim 2 vektorun. Ayni geldiyse demekki mod degismemis, yani while'den cikacak
      return(list("graph" = g, "global_str" = global_str))
  }

  if(verbose){
  	  cat(" comm_str: ", comm_str, "\n")
  }

  comm_str_result <- c()#elde etmek istedigimiz structure
  different_comm <- unique(comm_str)

  #yani global_str = (1,1,2,2,2,3,3) ise ve comm_str = (1,2,2) ise, sunun gibi biseye donusturcez: (1,1,2,2,2,2,2)
  #yani 3. komundakiler de 2. komune katilmis
  #yani bir onceki comm_str ile yeni elde edilen comm_str arasinda bag kuruyoruz
  len <- length(global_str)#comm_str'nin ilk bastaki boyutu, hep sabit
  if(length(global_str) != length(comm_str)){#eger daha ilk defa ise, global_str'yi kullanmanin anlami yok diye boyle eliyoruz
      #cat(" different_comm: ", different_comm, "\n")
      for(node in 1:length(different_comm)){
          comm <- different_comm[node]
          #cat(" comm: ", comm, "\n")
          numbers <- which(comm == comm_str)
          if(length(numbers)>1)
              global_str[which(global_str %in% numbers)]=comm
          #numbers, bir ornekle aciklarsak: 4. komun icin; c(1,2,4,4)'te 3. ve 4. indekse karsilik geldigi icin c(3,4)tur
    }
  }

  #rename communities - begin
  new_str <- vector(mode="integer",length=length(comm_str))
  new_global_str <- vector(mode="integer",length=length(global_str))
  final <- 1

  for(node in 1:length(different_comm)){#yeniden 1den baslayarak numaralandiriyoruz   TO DO sapply olabilir a[order(a)[1:3]]=9 gibi bisey
      vec <- different_comm[node]
      #secilen community'ye ait node'larin hangi indekste oldugunu buluyoruz
      correspondant <- which(comm_str == vec)
      new_str[correspondant] <- final

      if(length(global_str) != length(comm_str)){
          correspondant <- which(global_str %in% correspondant)
          new_global_str[correspondant] <- final
      }
      final <- final + 1
  }

  if(length(global_str) == length(comm_str))#1. faz sonunda, boyle yapmak gerekiyor. 2. ve sonrakilerde for icine girecek
      new_global_str <- new_str
  # rename - end

  if(verbose)
      cat(" NEW_STR: ", new_str, "\n")# 448855 gibi structure'i 1den baslayarak 112233 gibi yeniden duzenledik

  if(verbose)
      cat(" GLOBAL_STR: ", new_global_str, "\n")# 448855 gibi structure'i 1den baslayarak 112233 gibi yeniden duzenledik

  comm_str_result = unique(new_str)
  if(verbose)
      cat(" COMM_STR_RESULT: ", comm_str_result, "\n")#bir sonraki pass'e bu community structure ile giricek

  #yeni graph'i adjacent matristen yola cikarak olusturacagiz
  adj <- matrix(0,nrow=length(comm_str_result),ncol=length(comm_str_result))

  #community icindeki node'larin diger community'deki node'lar arasindaki linklere gore adj matrisini dolduruyoruz
  for(comm in 1:length(comm_str_result)){
    nodes <- which(new_str == comm)
    #self loop'lari da ekliyoruz
    adj[comm, comm] <- sum(induced.subgraph(g, nodes)[])
    #adj[comm, comm] <- sum(E(induced.subgraph(g, nodes))$weight)# -> directed graph icin 2 ile carpmaya gerek yok galiba
    for(comm2 in 1:length(comm_str_result)){
      if(comm != comm2){
        nodes2 <- which(new_str == comm2)
        adj[comm,comm2] <- sum(g[nodes, nodes2])
      }
    }
  }
  
  g1 <- graph.adjacency(mode= directed_or_undirected, adj, weighted=TRUE)
  
  if(verbose){
      cat(" ADJACENCY: ", adj, "\n")#yeni graph'in dogru yaratilip yaratilmadigini gormek icin (self loop v.s.)
      cat("in PARTITION2GRAPH_BINARY function => community structure: ", new_str, "\n")
  }  
  #graph'in node'larini bastan yaratmis olduk => eski graph'daki comm sayisi = yeni graph'daki node sayisi
  return(list("graph" = g1, "global_str" = new_global_str))
}


deltaQ = function(g, node, neigh_comm, nodes_inside_neigh_comm, tot_weight, directed){
  m=sum(g[,])
  
  if((length(node) == length(nodes_inside_neigh_comm)) && (node == nodes_inside_neigh_comm)){
      #Bu if statement'in amaci, node ile next_neigh ayni oldugunda "Degree from node to Comm" bilgisi cikmiyor, bizi yaniltir
      #mesela node=1 ve next_neigh=1 olsun. g[1,1] aslinda 1 komunun icindeki dugumlerinin birbirleriyle yaptigi etkilesim bilgileri
      #ama "Degree from node to Comm" bilgisi icermez cunku node != next_neigh olmasi gerekir.
      k_i_in=0
  }else{
      k_i_in=ifelse(directed, sum(g[node,nodes_inside_neigh_comm])+sum(g[nodes_inside_neigh_comm,node]),sum(g[node,nodes_inside_neigh_comm]))
  }
  c_tot = tot_weight[neigh_comm]
  k_i = sum(g[node,])
  #C++ kodundaki formul: k_i_in - c_tot*k_i/m        # undirected için
  increase = ifelse(directed, k_i_in - 2*c_tot*k_i/m, k_i_in - c_tot*k_i/m)
  #increase = ifelse(directed, k_i_in/m - 2*c_tot*k_i/(m*m), 2*k_i_in/m - 2*c_tot*k_i/(m*m))
  return(increase)
}


one_level = function(g, verbose, directed){
  nb_pass=10000
  gSize <- vcount(g)
  list_node <- comm_str <- seq(1, gSize) #each node is a community at beginning
  cur_mod <- new_mod <- ifelse(directed, modularity_directed(g, comm_str, 1), modularity(g, comm_str, E(g)$weight))
  min_modularity <- 0.000001
  #while init - begin
  improvement <- TRUE
  nb_iteration_done <- 0
  new_mod <- new_mod+0.01 #while'a ilk girişte sorun çıkarmasın diye => cünkü new_mod must be > cur_mod
  #end

  #configuration begin
  tot_weight <- vector(mode='integer',length=gSize) #gSize kadar 0 koyarak initialize ettik
  for(vertex in 1:gSize){
      tot_weight[vertex] <- sum(g[vertex,])
  }
  #end

  while(improvement && (new_mod-cur_mod)>min_modularity && nb_iteration_done!=nb_pass){
    #init - begin
    improvement <- FALSE
    cur_mod <- new_mod
    nb_iteration_done = nb_iteration_done+1
    #init - end
    print(paste("nb iteration: ",as.character(nb_iteration_done),", time: ",Sys.time()))

    for(node in 1:gSize){      
      node_comm <- comm_str[node]
      #computation of all neighboring communities of current node
      neigh <- neighbors(g, node)
      neigh <- neigh[which(neigh != node)]#self loop varsa komsulari icin de kendisi de cikar, o yuzden onu cikariyoruz
      
      #if any neighboring communities of current node exist
      if(length(neigh) != 0){#hic linki olmayan node'lar olabilir
          #remove node from its current community
          tot_weight[node_comm] <- tot_weight[node_comm] - sum(g[node,])
          comm_str[node] <- -1
	        #comms: node'un komsularinin hangi komunde oldugu bilgisini veriyor
	        comms <- unique(comm_str[neigh])#komsularinin icinden ayni komunde olanlar olcagi icin, unique ile eleyecegim

    	    ncomm = list()
	        i=1 #counter
          while(i <= length(comms)){
	            ncomm[[i]]=which(comms[i] == comm_str)#which'den gelen indeks bilgisini node listesinde koydugum zaman hangi node'larn old.
	            i=i+1
          }
	        #comms variable'i, node'un komsularinin komun bilgilerini icerir. Eger bu varaible'de, node'un komun bilgisi yoksa en son ekliyoruz
	        if(!(node_comm %in% comms)){
              ncomm[[i]] <- node# i, zaten loop'te en son 1 kere daha artiyor
	            comms=c(comms, node_comm)
      	  }

	        #deltaQ_data: (modularity, community) seklinde. node'un komsulariyla olsuturdugu modularity degerleri.
          deltaQ_data <- NA # deltaQ_data'yi tanimlamak icin var bu satir

          compute_deltaQ_data <- function(neigh_comm, g, node, comms, ncomm, tot_weight, deltaQ_data, directed){
	            #comms: node'un komsu node'larinin community'lerine bakarak community listesi
	            #ncomm: hangi community'de hangi node var, onun bilgisini iceriyor
              #neigh_comm: deltaQ'nun hesaplandigi community
	            index_neigh_comm <- which(comms == neigh_comm)#neigh_comm'un comms'da kacinci sirada-yani index bilgisini veriyor -> ncomm icin
	            nodes_inside_neigh_comm = unique(ncomm[[index_neigh_comm]])#matristen yola cikark ayni komunde olan node listesi
	            increase <- deltaQ(g, node, neigh_comm, nodes_inside_neigh_comm, tot_weight, directed)
	            return(c(increase, neigh_comm))
          }
	      
	        deltaQ_data = do.call(rbind, mclapply(comms, compute_deltaQ_data, g, node, comms, ncomm, tot_weight, deltaQ_data, directed,mc.cores=5))
          best <- deltaQ_data[which.max(deltaQ_data[,1]),]#increase'i yuksek olan satir bilgileri yani o iterasyondaki bilgiler
	        tot_weight[best[2]] <- tot_weight[best[2]] + sum(g[node,])#best[2]: best_community bilgisi
          comm_str[node] <- best[2]
          counter=1
  
	        if(verbose){
              cat("comm_str", comm_str, "\n")
      	      cat("tot_weight", tot_weight, "\n")
              print(deltaQ_data)
	        }
  
	        if(verbose){
	            cat(" DELTAQ_DATA: ","\n")
	            print(deltaQ_data)
	            cat(" best sonrasi comm_str: ", comm_str,"\n")
	        }

 	        if(best[2] != node_comm){
		          improvement <- TRUE
	        }

          new_mod <- ifelse(directed, modularity_directed(g, comm_str, 1), modularity(g, comm_str, E(g)$weight))

          if(verbose){
              cat(sprintf("in ONE_LEVEL function => pass_number: %d of %d (nb_pass) => new_mod=%s, cur_mod=%s\n", nb_iteration_done, nb_pass, new_mod, cur_mod))
              cat("in ONE_LEVEL function => community structure: ", comm_str, "\n")
          }
      }
    }
  }

  return(list("modularity" = new_mod, "membership" = comm_str))
}
