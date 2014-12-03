part=lv$membership
adj_mat=get.adjacency(g, attr="weight")

modularity2_matrix_directed=function(adj_mat, part){
m=sum(adj_mat)
v_num=vcount(g)
new_sum=0

for ( i in 1:v_num){
for ( j in 1:v_num){
if(part[i]==part[j]){
k_i_in=sum(E(g)[to(i)]$weight) 
k_j_out=sum(E(g)[from(j)]$weight)
new_sum = new_sum + ((adj_mat[i,j])-(k_i_in*k_j_out)/m)
}
}
}
return(new_sum/m)

}
