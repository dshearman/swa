library("twitteR")
library("ROAuth")


## start one off -------------------------------------

cred <- OAuthFactory$new(consumerKey="3AXr5dXGB2OaFZ7NS3yJLg",
                         consumerSecret="4UH9XYsxhGziFoOEQ39E7mQKL8jo2WohuZM3nIu9Tu0",
                         requestURL="https://api.twitter.com/oauth/request_token",
                         accessURL="https://api.twitter.com/oauth/access_token",
                         authURL="https://api.twitter.com/oauth/authorize")
cred$handshake()

save(cred, file="twitter.Rdata")

## end one off -------------------------------------

load("twitter.Rdata")

registerTwitterOAuth(cred)

####----------------------------------------------------------
#### Part 1: Get the tweets

tweets = searchTwitter('kevin bacon', n = 100, lang = "en")
tweets.df = twListToDF(tweets)


####----------------------------------------------------------
#### Part 2: Top 10 IDF words

library("tm")
library("SnowballC")

tweet.corpus = Corpus(VectorSource(tweets.df$text))

tweet.corpus = tm_map(tweet.corpus,
             function(x) iconv(x, to='UTF8', sub='byte'))

current.stopwords = c(stopwords("english"), "abbcbbb")

tweet.corpus = tm_map(tweet.corpus, removeNumbers)
tweet.corpus = tm_map(tweet.corpus, removePunctuation)
tweet.corpus = tm_map(tweet.corpus, stripWhitespace)
tweet.corpus = tm_map(tweet.corpus, tolower)
tweet.corpus = tm_map(tweet.corpus, removeWords, current.stopwords)
tweet.corpus = tm_map(tweet.corpus, stripWhitespace)
tweet.corpus = tm_map(tweet.corpus, stemDocument)

tweet.corpus = tm_map(tweet.corpus, PlainTextDocument)

tweet.dtm = DocumentTermMatrix(tweet.corpus)
tweet.matrix = as.matrix(tweet.dtm)

IDF = function(X) {
    N = nrow(tweet.matrix)
    return(log(N/colSums(X > 0)))
}

idf.weight = IDF(tweet.matrix)

idf.weight[order(idf.weight, decreasing=TRUE)[1:10]]

## These weights are not useful. They show the words with ony one
## document appearance.


####----------------------------------------------------------
#### Part 3

tweet.wdtm = weightTfIdf(tweet.dtm)
tweet.wmatrix = as.matrix(tweet.wdtm)

vec.norm = function(x) {
    ## compute the norm of vector x
    return(sqrt(x %*% x))
}

normalise.vector = function(x) {
    ## scale the vector x to be unit length
    return(x/vec.norm(x))
}

norm.tweet.wmatrix = apply(tweet.wmatrix, 1, normalise.vector)
D = 1 - norm.tweet.wmatrix %*% t(norm.tweet.wmatrix)
d = cmdscale(D, k=2)
plot(d)

## There appear to be three clusters.

####----------------------------------------------------------
#### Part 4: Words in clusters


cosine.dissimilarity = function(x, y) {
    ## normalise row vector lengths
    norm.x = normalise.vector(x)
    norm.y = normalise.vector(y)
    ## return 1 - inner product of the normalised vectors
    return(1 - (norm.x %*% norm.y))
}

library("skmeans")

SSB = function(clusters, M) {
    sk = skmeans(M, clusters)
    mean.prototype = apply(sk$prototypes, 2, mean)
    return(sum(apply(sk$prototypes, 1, cosine.dissimilarity,mean.prototype)))
}

SSB.set = sapply(2:15, SSB, norm.tweet.wmatrix)
## examine the elbow
plot(2:15,SSB.set)

## Choose three clusters
sk = skmeans(norm.tweet.wmatrix, 3)
## Find top words for each cluster

rownames(norm.tweet.wmatrix) = colnames(tweet.wmatrix)

cluster.top.words = function(k, norm.tweet.wmatrix, sk) {
    terms = rownames(norm.tweet.wmatrix)
    dx = apply(norm.tweet.wmatrix, 1, cosine.dissimilarity, sk$prototype[k,])
    return(terms[order(dx)[1:10]])
}

## print words
lapply(1:3, cluster.top.words, norm.tweet.wmatrix, sk)


####----------------------------------------------------------
#### Part 5

## Term topic matrix has the top words for each cluster representing
## each topic.

word.count = nrow(norm.tweet.wmatrix)
topic.count = nrow(sk$prototypes) # same as the number of clusters
T = matrix(0, word.count, topic.count)
top.words = 10 # number of words wanted from each cluster

## Put a count of 1 for all of the top words in each cluster
for (a in 1:topic.count) {
    ## compute the distance of each term to the cluster centre
    dx = apply(norm.tweet.wmatrix, 1, cosine.dissimilarity, sk$prototype[a,])
    ## identify the position of the closest words
    top.word.positions = order(dx)[1:top.words]
    ## add the closest words to T
    T[top.word.positions,a] = 1
}


## Multiply with W to obtain tweet topic matrix
Z = tweet.wmatrix %*% T
rownames(Z) = c(1:nrow(Z))
## Create adjacency matrix
A = Z %*% t(Z)


library("igraph")
## Create graph from adjacency matrix
G = graph.adjacency(A, diag=FALSE, mode="undirected", weighted=TRUE)
plot(G)

unconnected = (degree(G) == 0)
A = A[!unconnected,!unconnected]
G = graph.adjacency(A, diag=FALSE, mode="undirected", weighted=TRUE)
plot(G)


####----------------------------------------------------------
#### Part 6: Closeness ordering

O = closeness(G)
R = as.numeric(names(sort(O)))

## Comparing orderings

## New ordering
tweets.df$text[R]
## Twitter ordering
tweets.df$text

## What is the difference? What is the new ordering of?
