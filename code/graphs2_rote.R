#!/usr/bin/R


library("igraph")


randomWalk = function(g, start, end, length) {
    T = probabilityTransitionMatrix(g)
    
    p = rep(0, nrow(T))
    p[start] = 1
    for (a in 1:length) {
        p = T %*% p
    }
    return(p[end])
}

stationaryDistribution = function(g) {
    d = degree(g)
    return(d/sum(d))
}


probabilityTransitionMatrix = function(g) {
    A = t(as.matrix(get.adjacency(g)))
    T = A %*% diag(1/colSums(A))
    return(T)
}

smoothProbabilityTransitionMatrix = function(g, lambda) {
    T = probabilityTransitionMatrix(g)
    n = nrow(T)
    B = matrix(1/n, n, n)
    return(lambda*T + (1-lambda)*B)
}

graphVolume = function(g) {
    return(sum(degree(g)))
}

verifyStationary = function(g) {
    T = probabilityTransitionMatrix(g)
    e = eigen(T)
    pos = abs(Re(e$values) - 1) < 1e-5
    p = Re(e$vectors[,pos])
    p = p/sum(p)
    n = runif(length(p))
    n = n/sum(n)
    x = sample(c(0,1), 1)
    if (x == 0) {
        return(list(g = g, p = n, a = 0))
    }
    return(list(g = g, p = p, a = 1))
}

ergodicGraph = function(g) {
    return(!is.infinite(sum(distances(g, mode = "out"))))
}


vertices = 6



g = erdos.renyi.game(n = vertices, p = 0.5)
while (!ergodicGraph(g)) {
    g = erdos.renyi.game(n = vertices, p = 0.5)
}

x = sample(vertices, 2, replace = FALSE)
start = x[1]
finish = x[2]
plot(g)
cat("When taking a random walk of length 1 from vertex ", start, " in the shown graph, what is the probability of arriving at vertex ", finish, "?\n", sep = "")
invisible(readline(prompt="Press [enter] to reveal the answers."))
print(randomWalk(g, start, finish, 1))

invisible(readline(prompt="Press [enter] to continue."))


## undirected

g = erdos.renyi.game(n = vertices, p = 0.5)
while (!ergodicGraph(g)) {
    g = erdos.renyi.game(n = vertices, p = 0.5)
}

plot(g)
cat("Compute the stationary distribution of the shown graph.\n", sep = "")
invisible(readline(prompt="Press [enter] to reveal the answers."))
print(stationaryDistribution(g))
invisible(readline(prompt="Press [enter] to continue."))


g = erdos.renyi.game(n = vertices, p = 0.5)
while (!ergodicGraph(g)) {
    g = erdos.renyi.game(n = vertices, p = 0.5)
}

plot(g)
cat("Compute the probability transition matrix of the shown graph.\n", sep = "")
invisible(readline(prompt="Press [enter] to reveal the answers."))
print(probabilityTransitionMatrix(g))
invisible(readline(prompt="Press [enter] to continue."))

g = erdos.renyi.game(n = vertices, p = 0.5)
while (!ergodicGraph(g)) {
    g = erdos.renyi.game(n = vertices, p = 0.5)
}

plot(g)
cat("Compute the volume of the shown graph.\n", sep = "")
invisible(readline(prompt="Press [enter] to reveal the answers."))
print(graphVolume(g))
invisible(readline(prompt="Press [enter] to continue."))




## directed

g = erdos.renyi.game(n = vertices, p = 0.5, directed = TRUE)
while (!ergodicGraph(g)) {
    g = erdos.renyi.game(n = vertices, p = 0.5, directed = TRUE)
}

x = sample(vertices, 2, replace = FALSE)
start = x[1]
finish = x[2]
plot(g)
cat("When taking a random walk of length 1 from vertex ", start, " in the shown graph, what is the probability of arriving at vertex ", finish, "?\n", sep = "")
invisible(readline(prompt="Press [enter] to reveal the answers."))
print(randomWalk(g, start, finish, 1))
invisible(readline(prompt="Press [enter] to continue."))



g = erdos.renyi.game(n = vertices, p = 0.5, directed = TRUE)
while (!ergodicGraph(g)) {
    g = erdos.renyi.game(n = vertices, p = 0.5, directed = TRUE)
}

plot(g)
cat("Compute the probability transition matrix of the shown graph.\n", sep = "")
invisible(readline(prompt="Press [enter] to reveal the answers."))
print(probabilityTransitionMatrix(g))
invisible(readline(prompt="Press [enter] to continue."))


g = erdos.renyi.game(n = vertices, p = 0.5, directed = TRUE)
while (!ergodicGraph(g)) {
    g = erdos.renyi.game(n = vertices, p = 0.5, directed = TRUE)
}

plot(g)
z =verifyStationary(g)
cat("Is the following vector the stationary distribution for the shown graph?\n", sep = "")
print(z$p)
invisible(readline(prompt="Press [enter] to reveal the answers."))
if (z$a == 0) print("No")
if (z$a == 1) print("Yes")
invisible(readline(prompt="Press [enter] to continue."))


g = erdos.renyi.game(n = vertices, p = 0.5, directed = TRUE)
plot(g)
cat("Is the shown graph ergodic?\n", sep = "")
invisible(readline(prompt="Press [enter] to reveal the answers."))
print(ergodicGraph(g))
invisible(readline(prompt="Press [enter] to continue."))

g = erdos.renyi.game(n = vertices, p = 0.5, directed = TRUE)
while (!ergodicGraph(g)) {
    g = erdos.renyi.game(n = vertices, p = 0.5, directed = TRUE)
}

lambda = sample(0:10, 1)/10
plot(g)
cat("Compute the smoothed probability transition matrix, using lambda = ", lambda, ".\n", sep = "")
invisible(readline(prompt="Press [enter] to reveal the answers."))
print(smoothProbabilityTransitionMatrix(g, lambda))

