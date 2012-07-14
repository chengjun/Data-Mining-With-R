library(igraph)
g <- graph.ring(10)
plot(g)
shortest.paths(g)
get.shortest.paths(g, 5)
get.all.shortest.paths(g, 0, 5:7)

average.path.length(g) # for the whole network
path.length.hist(g)

## Weighted shortest paths
el <- matrix(nc=3, byrow=TRUE,
             c(0,1,0, 0,2,2, 0,3,1, 1,2,0, 1,4,5, 1,5,2, 2,1,1, 2,3,1,
               2,6,1, 3,2,0, 3,6,2, 4,5,2, 4,7,8, 5,2,2, 5,6,1, 5,8,1,
               5,9,3, 7,5,1, 7,8,1, 8,9,4) )
g2 <- add.edges(graph.empty(10), t(el[,1:2]), weight=el[,3])
shortest.paths(g2, mode="out")