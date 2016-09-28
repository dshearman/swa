#!/usr/bin/R


distance.minkowski = function(x, y, p) {
    return((sum((abs(x - y))^p))^(1/p))
}

distance.euclidean = function(x, y) {
    distance.minkowski(x, y, p = 2)
}

distance.manhattan = function(x, y) {
    distance.minkowski(x, y, p = 1)
}


distance.maximum = function(x, y) {
    return(max(abs(x - y)))
}

distance.binary = function(x, y) {
    u = sum((x != 0) | (y != 0))
    if (u == 0) {
        return(1)
    }
    n = sum((x != 0) & (y != 0))
    return(1 - n/u)
}


distance.cosine = function(x,y) {
    normx = sqrt(x %*% x)
    normy = sqrt(y %*% y)
    cosine = x %*% y /normx/normy
    return(1 - cosine)
}


randomVector = function(d) {
    x = round(runif(d, min = -10, max = 10))
    m = (rpois(3, lambda = 1) > 0)
    return(x*m)
}


distanceMatrix = function(x, distance) {
    n = length(x)
    D = matrix(0, n, n)
    for (a in 1:n) {
        for (b in 1:n) {
            D[a,b] = distance(x[[a]],x[[b]])
        }
    }
    return(D)
}


d = 3
n = 3

x = lapply(rep(d,n), randomVector)

answers = list(
    euclideanDist = distanceMatrix(x, distance.euclidean),
    manhattanDist = distanceMatrix(x, distance.manhattan),
    maximumDist = distanceMatrix(x, distance.maximum),
    binaryDist = distanceMatrix(x, distance.binary),
    cosineDist = distanceMatrix(x, distance.cosine)
    )


#X = Reduce(rbind, x)

cat("Compute the distance matrix for the following set of vectors:\n")
print(x)
cat("using the distance metrics:
1. Euclidean distance
2. Minkowski distance
3. Maximum distance
4. Binary distance
5. Cosine distance
")
invisible(readline(prompt="Press [enter] to reveal the answers."))
print(answers)

