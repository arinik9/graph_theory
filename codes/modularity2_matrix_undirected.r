part=lv$membership
adj_mat=get.adjacency(g, attr="weight")

modularity2_matrix_undirected=function(adj_mat, part){
m=sum(adj_mat)/2 # simetrik matris oldugundan
v_num=vcount(g)
new_sum=0

for ( i in 1:v_num){
for ( j in 1:v_num){
if(part[i]==part[j]){
k_i=sum(E(g)[from(i)]$weight)
k_j=sum(E(g)[from(j)]$weight)
new_sum = new_sum + ((adj_mat[i,j])-(k_i*k_j/(2*m)))
}
}
}
return(new_sum/(2*m))

}
