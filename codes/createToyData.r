## for create community structure with 3 graphs

## create example graph
g1 <- graph.full(5)
V(g1)$name <- 1:5    
g2 <- graph.full(5)
V(g2)$name <- 6:10
g3 <- graph.ring(5)
V(g3)$name <- 11:15
g <- g1 %du% g2 %du% g3 + edge('1', '6') + edge('1', '11')

##normal distribution for give a weight to each vertices
E(g)$weight <- as.integer(rnorm(ecount(g), mean=8, sd=3))

#save graph for re-read each time
write.graph(g, "/home/nejat/tez/data/toydata.edgelist", format=c("edgelist"))
write.table(E(g)$weight, "/home/nejat/tez/data/toydata.weight", col.names=FALSE, row.names=FALSE,quote=FALSE)

##read a table of our graph
g <- read.graph("/home/nejat/tez/data/toydata.edgelist", format=c("edgelist"), directed=FALSE)
E(g)$weight <- read.table("/home/nejat/tez/data/toydata.weight")$V1

lv <- multilevel.community(g, E(g)$weight)
lv$modularity
lv$membership
##bize en sonki sonucu verir
## 1.node -> 1.commun'e ait
## 2. node -> 1.commun'e ait
## 15. node -> 3.commun'e ait
[1] 1 1 1 1 1 2 2 2 2 2 3 3 3 3 3

lv$memberships
## 1. ve 2. pass birlikte
[,1] [,2] [,3] [,4] [,5] [,6] [,7] [,8] [,9] [,10] [,11] [,12] [,13] [,14] [,15]
## 1. pass
[1,]    1    1    1    1    1    2    2    2    2     2     4     4     3     3   4
## 2. pass
[2,]    1    1    1    1    1    2    2    2    2     2     3     3     3     3   3

## sadece 1.pass
lv$memberships[1,]
## sadece 2.pass
lv$memberships[2,]

#########################
#########################

##directed graph no:1
 adj <- matrix(c(0, 0, 0, 0, 1, 0, 1, 0, 1, 0, 0, 0, 1, 1, 0, 0), nrow=4, ncol=4)
 g5 <- graph.adjacency(adj,weighted=TRUE)
 V(g1)$name <- 1:4

## directed graph no: 2
 graph <- erdos.renyi.game(5, 0.3, type=c("gnp", "gnm"), directed = TRUE, loops = FALSE)
 V(g2)$name <- 5:9

## directed graph no: 3
 g3 <- ba.game(n=5, directed=TRUE)
 V(g3)$name <- 10:14

# Union of g1, g2, g3
 g <- g1 %du% g2 %du% g3 + edge('1', '6') + edge('8', '14')
# normal distribution for the weights 
 E(g)$weight <- as.integer(rnorm(ecount(g), mean=8, sd=3))


#directed graph'i kullanmak icin:
##read table ile okut
g <- read.graph("~/thesis/data/toydata2.edgelist", format=c("edgelist"), directed=TRUE)
E(g)$weight <- read.table("~/thesis/data/toydata2.weight")$V1

#weighted graph cizdirmek icin:
plot(g, edge.label=round(E(g)$weight, 3))

#########################
#########################

modularity(g,lv$membership,weight=E(g)$weight)

#directed graph'da community'lere ayirmak icin multilevel.community() ya da fastgreedy.community() kullanamadim
#o yuzden sunu kullandim:  edge.betweenness.community()
edge.betweenness.community(graph=g, membership=membership, weights=E(g)$weight)
Graph community structure calculated with the edge betweenness algorithm
Number of communities (best split): 3 
Modularity (best split): 0.5124182 
Membership vector:
 1  2  3  4  5  6  7  8  9 10 11 12 13 14 
 1  1  1  1  2  2  2  2  2  3  3  3  3  3 

# ben graph'i olustururken 3 ayri graph'in birlesimiyle elde etmistim. 
#yukardaki graph'a gore kendi membership vektorumu olusuturup modularity alacagim

membership= c(1,1,1,1,2,2,2,2,2,3,3,3,3,3)
modularity(g,membership,weight=E(g)$weight)
cevap: [1] 0.52375

## find degree_in & degree_out
> degree(g1, v=V(g1), mode = c("in")) #mode="in" de calisiyor galiba
1 2 3 4 
0 2 1 2 
> degree(g1, v=V(g1), mode = c("out"))
1 2 3 4 
3 1 1 0 
> degree(g1, v=V(g1), mode = c("total"))
1 2 3 4 
3 3 2 2 



