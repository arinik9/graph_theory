#!/usr/bin/env Rscript

#########
# INPUT #
#########

## read supporting.nodes file
# supporting_nodes.csv'deki her satir, growth_rate.csv'de ayni no'lu satira
# karsilik geliyor

nodes = read.csv("supporting_nodes3.csv", header=FALSE, strip.white=TRUE, stringsAsFactors=FALSE)
growth.rate = read.csv(file="growth_rate3.csv", header=TRUE, stringsAsFactors=FALSE)
db = read.csv("walktrap_weighted_community.csv", header=FALSE, strip.white=TRUE, stringsAsFactors=FALSE)

growth.rate$nodes = nodes['V1']
comms = unique(growth.rate['comm'])

#alfa ve bheta, Measure matrix'ini olustururken katsayi olarak kullaniliyor
alfa = 1
bheta = 1


for(c in comms[,]){
  # For each community in the file
  cat("\n ### komun:", c, " ### \n")
  # komun pattern'ini her satirda bulabilmek icin grep'e gore ceviriyoruz

  regex=gsub(" ", ".*", c)
  comm.nodes=c();
  # c komununde olan node butun id'leri buluyoruz
  for(i in seq(1,nrow(db))){
    if(grepl(regex, paste(db[i,],collapse=","))){
      comm.nodes=c(comm.nodes,i)
    }
  }

  set = growth.rate[which(growth.rate['comm'] == c),]
  size = nrow(set)

  #############################
  # Measure Matrixi olusturma #
  #############################

  # m1: anlamsal benzerlik yani item benzerligine bakilarak matrix olusturma
  # Jaccard Index kullaniliyor benzerlik kriterinde

  m1 = matrix(0, size, size)
  l1 = strsplit(set['attr'][,], " ")

  for(i in seq(1,length(l1))){
    for(j in seq(1,length(l1))){
      if(j>i){
        m1[i,j] = (length(which(l1[[i]] %in% l1[[j]])))/(length(unique(c(l1[[i]],l1[[j]]))))
        m1[j,i] = m1[i,j]
      }
    }
  }

  # m2: supporting nodes benzerligine bakilarak matrix olusturma
  # Jaccard Index kullaniliyor benzerlik kriterinde

  m2 = matrix(0, size, size)
  l2 = strsplit(set[['nodes']][,], " ")

  for(i in seq(1,length(l2))){
    for(j in seq(1,length(l2))){
      if(j>i){
        m2[i,j] = (length(which(l2[[i]] %in% l2[[j]])))/(length(unique(c(l2[[i]],l2[[j]]))))
        m2[j,i] = m2[i,j]
      }
    }
  }

  print("Measure matrix M:")
  m=(alfa*m1 + bheta*m2)/(alfa+bheta)
  print(m)
  
  ##########
  # KMEANS #
  ##########

  # Should be choose an optimum K
  
  if(size>50){
    # %10 of number of patterns => threshold. The quotion %10 may be changed
    k = as.integer(size/10)
  } else{
    # To eleminate some possibilites, we choose a threshold value
    totElt = sum(seq(1,size-1))
    threshold = (sum(m)/2)/totElt
    # We don't want this situation:
    # Number of patterns: 5  AND K centers:5
    # Should be k>5 in this example
    trues = length(which((m>threshold) == TRUE))/2
    k = as.integer((trues/totElt)*size)
  }

  cat("### Choice of K: ", k,"###\n")

  res = kmeans(m, k)

  ################################
  # Selection of Represantatives #
  ################################

  covered = c()
  bests =c()
  for(i in seq(1,k)){
    n = which(res$cluster == i)
    best = n[which.max(set[n,"growth_rate"])]
    covered = unique(c(covered, strsplit(set[best, "nodes"][[1]], " ")[[1]]))
    bests = rbind(bests, set[best,])
  }

  ##########
  # OUTPUT #
  ##########

  print("### The represantatives of each cluster ###")
  print(bests)
  print("### Union of supporting nodes of the represantatives ###")
  print(covered)
  in.indexes = which(comm.nodes %in% covered)
  print("### Anomalies ###")
  print(paste(comm.nodes[-c(in.indexes)], collapse=" "))
  cat("\n\n\n\n")
}
