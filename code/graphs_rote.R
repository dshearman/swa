#!/usr/bin/R


library("igraph")

egocentric1 = function(g, vertex) {
    ## degree 1 ego centric graph

    #a = get.adjacency(make_ego_graph(g, 1, vertex)[[1]])
    a = get.adjacency(g)
    d = dim(a)
    mask = rep(0,d[2])
    mask[vertex] = 1
    for (x in 1:d[1]) {
        if (x != vertex) {
            a[x,] = a[x,] * mask
        }
    }
    g2 = graph.adjacency(a, mode = "undirected")
    g3 = make_ego_graph(g2, 1, vertex)[[1]]
    return(g3)
}

egocentric1.5 = function(g, vertex) {
    return(make_ego_graph(g, 1, vertex)[[1]])
}

egocentric2 = function(g, vertex) {
    return(make_ego_graph(g, 2, vertex)[[1]])
}


vertices = 6

graphChoice = sample(c("erdos", "barabasi"), 1)

deg = 0

while (deg == 0) {

    if (graphChoice == "erdos") {
        g = erdos.renyi.game(n = vertices, p = 0.5)
    } else {
        g = barabasi.game(n = vertices, power = 0.8, directed = FALSE)
    }
    deg = prod(degree(g))
}


vertex = sample(vertices, 1)

answers = list(
    adjacencyMatrix = as.matrix(get.adjacency(g)),
    adjacenyList = get.edgelist(g),
    egocentricDegree1 = egocentric1(g, vertex),
    egocentricDegree1.5 = egocentric1.5(g, vertex),
    #egocentricDegree2 = egocentric2(g, vertex),
    degreeDistribution = degree.distribution(g)*length(V(g)),
    density = graph.density(g),
    diameter = diameter(g),
    degreeCentrality = degree(g),
    closenessCentrality = 1/closeness(g),
    betweenCentrality = betweenness(g),
    graphType = graphChoice
    )


plot(g)
cat("Tasks:
1.  Compute the adjacency matrix for the shown graph.
2.  Compute the adjacency list for the shown graph.
3.  Sketch the degree 1 ego-centric graph, centered on vertex ", vertex, ".
4.  Sketch the degree 1.5 ego-centric graph, centered on vertex ", vertex, ".
5.  Compute the degree distribution.
6.  Compute the graph density.
7.  Compute the graph diameter.
8.  Compute the degree centrality for each vertex.
9.  Compute the closeness centrality for each vertex.
10. Compute the between centrality for each vertex.
11. Is the graph more similar to an Erdos-Renyi graph or Barabasi-Albert graph?
", sep = "")
invisible(readline(prompt="Press [enter] to reveal the answers."))
print(answers)

par(mfrow = c(1,2))
plot(answers$egocentricDegree1)
plot(answers$egocentricDegree1.5)
par(mfrow = c(1,1))
