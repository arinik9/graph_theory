#!/usr/bin/env Rscript

#############
# LIBRARIES #
#############

#install.packages("combinat")
#install.packages("klaR", repos="http://R-Forge.R-project.org")
library(klaR)

#############
# FUNCTION #
#############

# supporting_nodes_file'deki her satir, patterns_file'de ayni no'lu satira karsilik gelmeli
# alfa ve beta, Measure matrix'ini olustururken katsayi olarak kullaniliyor
# K: number of centers => kullanici K degerini kendi secmek isteyebilir
# algorithm: "kmeans" or "kmodes". Default is "kmeans"
# verbose: To see Measure Matrix M, do verbose = 1

#Return: None (simdilik)

getAnomalies = function(supporting_node_file, patterns_file, db_file, algorithm="kmeans", 
                        alfa=1, beta=1, k=0, verbose = 0){

  nodes = read.csv(supporting_node_file, header=FALSE, strip.white=TRUE, stringsAsFactors=FALSE)
  growth.rate = read.csv(file=patterns_file, header=TRUE, stringsAsFactors=FALSE)
  db = read.csv(db_file, header=FALSE, strip.white=TRUE, stringsAsFactors=FALSE)

  growth.rate$nodes = nodes['V1']
  comms = unique(growth.rate['comm'])

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

    m=(alfa*m1 + beta*m2)/(alfa+beta)
    if(verbose){
      print("Measure matrix M:")
      print(m)
    }
    
    ##########
    # KMEANS #
    ##########

  if(k <= 0){ #Yani kullanici K degeri girmediyse ya da negatif deger girdiyse
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
  }

    cat("### Choice of K: ", k,"###\n")

    res = NULL
    if(algorithm == "kmeans"){
      res = kmeans(m,k)
    } else{
      res = kmodes(m,k)
    }

    ################################
    # Selection of Represantatives #
    ################################
    covered = c()
    bests =c()
    for(i in seq(1,k)){
      n = which(res$cluster == i)
      print(n)
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
}


#########
# INPUT #
#########

##### TEST1 ######
supporting_node_file = "supporting_nodes1.csv"
patterns_file = "growth_rate1.csv"
db_file = "walktrap_weighted_community.csv"

getAnomalies(supporting_node_file, patterns_file, db_file, "kmeans")
#getAnomalies(supporting_node_file, patterns_file, db_file, "kmodes")


##### TEST2 #####
supporting_node_file = "supporting_nodes2.csv"
patterns_file = "growth_rate2.csv"
db_file = "walktrap_weighted_community.csv"

getAnomalies(supporting_node_file, patterns_file, db_file, "kmeans")
#getAnomalies(supporting_node_file, patterns_file, db_file, "kmodes")


###### TEST3 #####
supporting_node_file = "supporting_nodes3.csv"
patterns_file = "growth_rate3.csv"
db_file = "walktrap_weighted_community.csv"

getAnomalies(supporting_node_file, patterns_file, db_file, "kmeans")
#getAnomalies(supporting_node_file, patterns_file, db_file, "kmodes")
