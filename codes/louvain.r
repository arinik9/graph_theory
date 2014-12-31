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
  mod <- modularity(g, comm_str)

  level <- one_level(g,verbose)
  comm_str <- level$membership
  new_mod <- level$modularity
  #ilk basta her bi node birer komun oldugu icin, previous_str yerine seq(1,gSize) yazdik.
  #previous_str'nin amaci, yeni hesaplanan comm_str bir onceki comm_str ile bag kurmasini saglar
  result <- partition2graph_binary(g, seq(1, gSize), comm_str, verbose, directed)#baslangicta previous_str ile comm_str ayni olcak
  g1 <- result$graph
  previous_str <- result$previous_str
  comm_str_result <- result$comm_str_result
  plot(g1, edge.label=round(E(g)$weight, 3))
  cat(" WHILE ONCESI - WHILE ONCESI \n")
  while((new_mod-mod)>precision){
    mod <- new_mod
    level <- one_level(g1,verbose)
    comm_str <- level$membership
    new_mod <- level$modularity
    result <- partition2graph_binary(g, previous_str, comm_str, verbose, directed)
    g1 <- result$graph
    mat_comm_str <- result$mat_comm_str
    comm_str_result <- result$comm_str_result
    previous_str <- result$previous_str
  }

  return(list("modularity" = new_mod, "membership" = previous_str, "how many cluster?" = unique(previous_str)))
}



#function: partition2graph_binary()
partition2graph_binary = function(g, previous_str, comm_str, verbose=FALSE, directed=FALSE){
  if(directed){
    directed_or_undirected = "directed"
  }
  else{
    directed_or_undirected = "undirected"
  }

  if(length(unique(previous_str)) == length(comm_str) && all(unique(previous_str) == comm_str)){
  #Ayni gelmemesi lazim 2 vektorun. Ayni geldiyse demekki mod degismemis, yani while'den cikacak
    return(list("graph" = g, "comm_str_result" = comm_str, "previous_str" = previous_str))
  }

  cat(" previous_str: ", previous_str, "\n")
  cat(" comm_str: ", comm_str, "\n")

  comm_str_result <- c()#elde etmek istedigimiz structure
  different_comm <- unique(comm_str)
  length_different_comm <- length(different_comm)


  #new_str'den bakarak ilk bastaki length_comm_str boyutundaki new_comm_str yaraticaz
  #yani previous_str = (1,1,2,2,2,3,3) ise ve comm_str = (1,2,2) ise, sunun gibi biseye donusturcez: (1,1,2,2,2,2,2)
  #yani 3. komundakiler de 2. komune katilmis
  #yani bir onceki comm_str ile yeni elde edilen comm_str arasinda bag kuruyoruz
  len <- length(previous_str)#comm_str'nin ilk bastaki boyutu, hep sabit
  if(length(previous_str) != length(comm_str)){#eger daha ilk defa ise, previous_str'yi kullanmanin anlami yok diye boyle eliyoruz
    cat(" different_comm: ", different_comm, "\n")
    for(node in 1:length_different_comm){
      comm <- different_comm[node]
      cat(" comm: ", comm, "\n")
      numbers <- which(comm == comm_str)
      cat(" numbers: ", numbers, "\n")
      if(length(numbers)>1){
        #numbers, bir ornekle aciklarsak: c(1,2,4,4)'te 3. ve 4. indekse karsilik geldigi icin c(3,4)tur
        for(i in 1:length(numbers)){
        previous_str[which(previous_str == numbers[i])] <- comm
        }
      cat(" previous_str: ", previous_str, "\n")
      }
    }
  }
  else{#eger ilk defa comm_str kullaniliyorsa yani previous_str'nin onemi henuz yok ise
    previous_str = comm_str
  }

  #rename communities - begin
  new_str <- vector(mode="integer",length=length(previous_str))
  final <- 1
  length_row <- 0 # ayni zamanda, community'ler arasinda en cok node'u olanin node sayisini bulucaz
  max_comm <- 0

  for(node in 1:length_different_comm){#yeniden 1den baslayarak numaralandiriyoruz
    vec <- different_comm[node]
    #secilen community'ye ait node'larin hangi indekste oldugunu buluyoruz
    correspondant <- which(vec == previous_str)
    new_str[correspondant] <- final
    final <- final + 1
  }
  # rename - end
  cat(" NEW_STR: ", new_str, "\n")# 448855 gibi structure'i 1den baslayarak 112233 gibi yeniden duzenledik

  comm_str_result = unique(new_str)
  cat(" COMM_STR_RESULT: ", comm_str_result, "\n")#bir sonraki pass'e bu community structure ile giricek

  #yeni graph'i adjacent matristen yola cikarak olusturacagiz
  adj <- matrix(0,nrow=length(comm_str_result),ncol=length(comm_str_result))

  #community icindeki node'larin diger community'deki node'lar arasindaki linklere gore adj matrisini dolduruyoruz
  for(comm in 1:length(comm_str_result)){
    nodes <- which(new_str == comm)
    #self loop'lari da ekliyoruz
    adj[comm, comm] <- sum(E(induced.subgraph(g, nodes))$weight)
    for(comm2 in 1:length(comm_str_result)){
      if(comm != comm2){
        nodes2 <- which(new_str == comm2)
        adj[comm,comm2] <- sum(g[nodes, nodes2])
      }
    }
  }
  cat(" ADJACENCY: ", adj, "\n")#yeni graph'in dogru yaratilip yaratilmadigini gormek icin (self loop v.s.)
  g1 <- graph.adjacency(mode= directed_or_undirected, adj, weighted=TRUE)

  if(verbose){
    cat("in PARTITION2GRAPH_BINARY function => community structure: ", new_str, "\n")
  }  
  #graph'in node'larini bastan yaratmis olduk => eski graph'daki comm sayisi = yeni graph'daki node sayisi
  return(list("graph" = g1, "comm_str_result" = comm_str_result, "previous_str" = new_str))
}


#function: one_level()
one_level = function(g, verbose=FALSE, nb_pass=10000){
  gSize <- vcount(g)
  list_node <- comm_str <- seq(1, gSize) #each node is a community at beginning
  cur_mod <- new_mod <- modularity(g, comm_str)
  min_modularity <- 0.000001

  #while init - begin
  improvement <- TRUE
  nb_iteration_done <- 0
  new_mod <- new_mod+0.01 #while'a ilk girişte sorun çıkarmasın diye => cünkü new_mod must be > cur_mod
  #end

  #configuration begin
  #in_weight <- vector(mode='integer',length=gSize) #gSize kadar 0 koyarak initialize ettik
  tot_weight <- vector(mode='integer',length=gSize) #gSize kadar 0 koyarak initialize ettik
  for(node in 1:gSize){
    tot_weight[node] <- sum(g[node,])
    #in_weight[node] <- sum(g[node,node])
  }
  #end

  memberships <- c()

  while(improvement && (new_mod-cur_mod)>min_modularity && nb_iteration_done!=nb_pass){
    #init - begin
    improvement <- FALSE
    #cur_mod <- new_mod
    cur_mod <- new_mod
    nb_iteration_done = nb_iteration_done+1
    #init - end

    for(node_tmp in 1:gSize){
      node <- node_tmp
      node_comm <- comm_str[node]
      
      #computation of all neighboring communities of current node
      neigh <- neighbors(g, node)
      neigh <- neigh[which(neigh != node)]#self loop varsa komsulari icin de kendisi de cikar, o yuzden onu cikariyoruz
print(node)
print(neigh)
      length_neigh <- length(neigh)
      comms <- unique(comm_str[neigh])#komsularinin icinden ayni komunde olanlar olcagi icin, unique ile eleyecegim
      ncomm <- matrix(0,length(comms),length(comm_str))
print(comms)
      for(i in 1:length(comms)){
	l = list_node[which(comms[i] == comm_str)]#which'den gelen indeks bilgisini node listesinde koydugum zaman hangi node'larn old. biliriz
	print(l)
	ncomm[i,1:length(l)] <- l
      }
      #if any neighboring communities of current node exist
      if(length_neigh != 0){
        #remove node from its current community
        #in_weight[node] <- in_weight[node] - sum(g[node,])
        tot_weight[node_comm] <- tot_weight[node_comm] - sum(g[node,])
        comm_str[node] <- -1

        best_comm <- node_comm
cat(" best_com init: ", best_comm, "\n")
        best_increase <- 0
        for(neigh_comm in 1:length(comms)){
print('basladi')
          next_neigh = unique(ncomm[neigh_comm,])[which(0 != ncomm[neigh_comm,])]#ayni komunde olan node listesi
	  #increase <- deltaQ(g, node, next_neigh)#yeni degistirildi
	#DELTAQ fonksiyonu yerine - begin
          m=sum(g[,])
	  k_i_in=sum(g[node,next_neigh])
	  c_tot = tot_weight[neigh_comm]
	  k_i = sum(g[,node])
	  #C++ kodundaki formul: k_i_in - c_tot*k_i/m
	  increase = 2*k_i_in/m - 2*c_tot*k_i/(m*m)
	#DELTAQ end
	        cat(" DELTAQ, INCREASE: ", increase, "\n")
          if(best_increase < increase){ #increase'in pozitif olmasi lazim, yoksa orjinal community'sinde kalir
            best_comm <- comm_str[next_neigh][which(comm_str[next_neigh] != -1)][1]#next_neigh 1den fazla bile olsa ilkinin commun bilgisi yeterli
            best_increase <- increase
          }
        }
cat(" best_com sonra: ", best_comm, "\n")
        #insert node in the nearest community
	tot_weight[best_comm] <- tot_weight[best_comm] + sum(g[node,])
        comm_str[node] <- best_comm

        if(best_comm != node_comm){
          improvement <- TRUE
        }
        new_mod <- modularity(g, comm_str)

        if(verbose){
          cat(sprintf("in ONE_LEVEL function => pass_number: %d of %d (nb_pass) => new_mod=%s, cur_mod=%s\n", nb_iteration_done, nb_pass, new_mod, cur_mod))
          cat("in ONE_LEVEL function => community structure: ", comm_str, "\n")
        }
      }
    }
    memberships <- c(memberships, comm_str)#surekli memberships'in  uzerine ekliyoruz ki membvership farkliliklarini gorebilelim
  }

  return(list("modularity" = new_mod, "membership" = comm_str, "memberships" = matrix(memberships,nrow=nb_iteration_done)))
}
