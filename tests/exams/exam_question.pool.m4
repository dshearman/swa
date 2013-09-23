m4_changequote(`<[', `]>')

m4_define(_precision,<[
% begin.rcode setup, echo=FALSE
  options(digits=$1)
  opts_chunk$set(comment=NA, highlight=FALSE)
knit_hooks$set(document = function(x) {
  gsub('(\\\\end\\{knitrout\\})\n', '\\1', x)
})
% end.rcode 
]>)



m4_define(_q1,<[

%%%% Mean, standard deviation, mode, median, quantiles

\squestion
  
  % begin.rcode echo=FALSE, results="hide", message=FALSE
    require("igraph")
    $@
    #g = graph.formula(A-B, A-C, A-D, B-D)
    d = degree(g)
    close = 1/closeness(g)
    n = length(d)
    names(d) = LETTERS[1:n]
 % end.rcode 

 Using the following graph:

 \begin{center}
    % begin.rcode echo=FALSE,results="asis", fig=TRUE, fig.width=4, fig.height=4
    par(mar = c(0,0,0,0))
    plot(g, layout=layout.fruchterman.reingold, vertex.size = 35, vertex.label = LETTERS[1:n])
    % end.rcode 
  \end{center}

  \begin{enumerate}
  \item Construct the adjacency matrix.
  \item Calculate the degree distribution.
  \item Calculate the closeness centrality for each vertex.
  \item Identify which vertex is the most central.
  \end{enumerate}

  \begin{workingbox}
    \marknote{For each of these four questions, give 1 mark for the correct
    working and 1 mark for the right answer.}
    
  \begin{enumerate}
  \item The adjacency matrix is:
    % begin.rcode echo=FALSE,results="verbatim", message=FALSE
    print(as.matrix(get.adjacency(g)))
    % end.rcode 

  \item	The degree distribution is:
    % begin.rcode echo=FALSE,results="verbatim"
    table(degree(g))
    % end.rcode 

  \item The closeness centrality for each vertex is:
    \begin{center}
      \begin{tabular}{\rinline{paste(rep('c',n),collapse='')}}
        \rinline{paste(names(d),collapse=" & ")} \\
        \rinline{paste(close,collapse=" & ")}
      \end{tabular}
    \end{center}
  \item The most central vertex is vertex \rinline{names(which.min(close))}.
  
  \end{enumerate}

  \end{workingbox}]>)



m4_define(_q2,<[

%%%% Mean, standard deviation, mode, median, quantiles


\squestion
  
  % begin.rcode echo=FALSE,results="hide", message=FALSE
    require("mvtnorm")
    require("xtable")
    $@
    #g = graph.formula(A-B, A-C, A-D, B-D)

latex.matrix = function(X) {
  nc = ncol(X)
  nr = nrow(X) 
  text = c("\\begin{array}{", paste(rep('c',nc),collapse=''), "}\n")
  for (a in 1:nr) {
    text = c(text, paste(format(X[a,]),collapse=" & "), " \\\\\n", sep="")
  }
  text = c(text, "\\end{array}\n", sep="")
  return(paste(text, collapse=" "))
}

set.seed(1)
cluster1 = rmvnorm(4, mean = c(5,5), sigma = matrix(c(1,0,0,1),2,2))
cluster2 = rmvnorm(4, mean = c(2,12), sigma = matrix(c(1,0,0,1),2,2))
cluster3 = rmvnorm(2, mean = c(15,9), sigma = matrix(c(1,0,0,1),2,2))
X = rbind(cluster1, cluster2, cluster3)
m = apply(X,2,mean)
zero.mean = function(x, m) { return(x - m) }
Z = t(apply(X,1,zero.mean, m))
n = nrow(Z)
wss <- (nrow(Z)-1)*sum(apply(Z,2,var))
bss = 0
cluster.count = 6
for (i in 2:cluster.count) {
    z = kmeans(Z, centers=i)
    wss[i] <- z$tot.withinss
    bss[i] <- z$betweenss
}
tss = z$totss
i = 2
z = kmeans(Z, centers=i)
wss[i] <- z$tot.withinss
bss[i] <- z$betweenss
rwss = wss
wss[2] = NA
wssf = format(wss)
wssf[is.na(wss)] = ""
z = kmeans(Z, centers=2)
A = cbind(Z,z$cluster)
colnames(A) = c("x1","x2","Cluster")
# wss
# z$totss - sum(diag(table(z$cluster)) %*% (z$centers)^2)

 % end.rcode 

 The following \rinline{n} centred data points ($\bar{x} = [ 0 ~ 0 ]$)
 and their associated cluster:
 % begin.rcode echo=FALSE,results="verbatim"
 print(A)
 % end.rcode 
 have the within cluster sum of squares (SSW) values:
    \begin{center}
      \begin{tabular}{c|\rinline{paste(rep('c',cluster.count),collapse='')}}
        $k$ & \rinline{paste(1:cluster.count,collapse=" & ")} \\
	\hline
        SSW & \rinline{paste(wssf,collapse=" & ")}
      \end{tabular}
    \end{center}
 and the total sum of squares value (SST) \rinline{format(tss)}.

 Given that the cluster centres for $k = 2$ are:
 % begin.rcode echo=FALSE,results="verbatim"
 print(z$centers)
 % end.rcode 
 \begin{enumerate}
 \item Compute the SSW for $k = 2$.
 \item Plot the elbow bend plot for this data.
 \item Determine how many clusters is the most suitable for the data.
 \end{enumerate}

  \begin{workingbox}

 \begin{enumerate}
 \item SSW = SST - SSB. SSB = 
\begin{align*}
 A = \left [ \rinline{latex.matrix(diag(table(z$cluster)))}  \right ]
 \left [ \rinline{latex.matrix(z$centers^2)} \right ] = 
 \left [ \rinline{latex.matrix(diag(table(z$cluster)) %*% z$centers^2)} \right ]
\end{align*}
giving SSB = \rinline{format(sum(diag(table(z$cluster)) %*% z$centers^2))}, therefore SSW = \rinline{format(tss)} - 
\rinline{format(sum(diag(table(z$cluster)) %*% z$centers^2))} = 
\rinline{format(tss - sum(diag(table(z$cluster)) %*% z$centers^2))}.

 \item Plot the elbow bend plot for this data.
 \begin{center}
    % begin.rcode echo=FALSE,results="asis", fig=TRUE, fig.width=5, fig.height=4
    #par(mar = c(0,0,0,0))
    plot(1:cluster.count,rwss, xlab="Number of clusters", ylab="SSW")
    % end.rcode 
  \end{center}

 \item Determine how many clusters is the most suitable for the data.

 The number of clusters is determined by where the elbow occurs.

 \end{enumerate}


  
  \end{workingbox}]>)



m4_define(_text_index_q1,<[

%%%% construct text index, weight and query

\squestion
  
  % begin.rcode echo=FALSE,results="hide", message=FALSE
    require("tm")
    $@
    docs <- c("Go dog, go!", "Stop cat, stop", "The dog stops the cat and the bird.")
    query = c("stop", "dog")
    stop = c("a", "the", "is", "to", "it", "and")
    corp = Corpus(VectorSource(docs))
    corp.pp = tm_map(corp, removePunctuation)
    corp.pp = tm_map(corp.pp, casefold)
    corp.pp = tm_map(corp.pp, removeWords, stop)
    corp.pp = tm_map(corp.pp, stemDocument)
    A = as.matrix(DocumentTermMatrix(corp.pp))
    N = length(corp.pp)
    ft = apply(A > 0,2,sum)
    W = log(A + 1) %*% diag(log(N/ft))
    colnames(W) = colnames(A)
    qcols = colnames(A) %in% query
    inner.prod = apply(W[,qcols],1,sum)
    doc.length = sqrt(apply(W^2,1,sum))
    query.length = sqrt(sum(length(query)))
 % end.rcode 

  Using the three documents:

  \begin{enumerate}[i.]
  \item \rinline{docs[1]}
  \item \rinline{docs[2]}
  \item \rinline{docs[3]}
  \end{enumerate}

  \begin{enumerate}

  \item Write out the contents of each document after removing
  puctuation, casefolding, stopword removal and stemming has been
  performed. Use the stopword list \{\rinline{paste(stop, collapse=", ")}\}.

  \item Construct the document-term index containing term frequencies.

  \item Construct the document-term index containing TF-IDF weights.
 
  \item Calculate the query score for each document using the query ``\rinline{paste(query, collapse=" ")}''.
  \end{enumerate}

  \begin{workingbox}
    \begin{enumerate}
    \item The pre-processed documents are:
        \begin{enumerate}
	\item \rinline{corp.pp[[1]]}
	\item \rinline{corp.pp[[2]]}
	\item \rinline{corp.pp[[3]]}
	\end{enumerate}
    
    \item Document-term index containing frequencies is:
    % begin.rcode echo=FALSE,results="verbatim"
    print(A)
    % end.rcode 

    \item TF-IDF weighting is $w_{d,t} = \log(f_{d,t} + 1)\log(N/f_t)$. 
    $N = \rinline{N}$, $f_t = [~\rinline{paste(ft, collapse="~")}~]$.
    Document-term index containing TF-IDF weights is:
    % begin.rcode echo=FALSE,results="verbatim"
    print(W)
    % end.rcode 
    
    \item The query score for each document using the query ``stop dog'' is:
    \begin{enumerate}
    \item $\rinline{format(inner.prod[1])}/(\rinline{format(doc.length[1])}\times\rinline{format(query.length)}) = \rinline{format(inner.prod[1]/(doc.length[1]*query.length))}$
    \item $\rinline{format(inner.prod[2])}/(\rinline{format(doc.length[2])}\times\rinline{format(query.length)}) = \rinline{format(inner.prod[2]/(doc.length[2]*query.length))}$
    \item $\rinline{format(inner.prod[3])}/(\rinline{format(doc.length[3])}\times\rinline{format(query.length)}) = \rinline{format(inner.prod[3]/(doc.length[3]*query.length))}$
    \end{enumerate}

  \end{enumerate}

  
  \end{workingbox}]>)


m4_define(_link_analysis_q1,<[

%%%% Mean, standard deviation, mode, median, quantiles

\squestion
  
  % begin.rcode echo=FALSE,results="hide"
    require("igraph")
    $@
    g = graph.formula(A+-+B, A+-C, A+-D, A+-E, B+-C, B+-D, D+-+E)
    alpha = 0.8
    A = t(as.matrix(get.adjacency(g)))
    T = A %*% diag(1/apply(A,2,sum))
    N = dim(A)[1]
    J = matrix(rep(1/N, N*N), N, N)
    R = alpha*T + (1-alpha)*J
    e = eigen(R)
    pos = which(abs(e$values - 1) < 1e-8)
    station = e$vectors[,pos]/sum(e$vectors[,pos])
 % end.rcode 


 Using the following graph:

 \begin{center}
    % begin.rcode echo=FALSE,results="asis", fig=TRUE, fig.width=4, fig.height=4
    par(mar = c(0,0,0,0))
    plot(g, layout=layout.fruchterman.reingold, vertex.size = 35, vertex.label = LETTERS[1:N])
    % end.rcode 
  \end{center}

  \begin{enumerate}
  \item Contruct the probability transition matrix.
  \item State if the graph ergodic and why or why not.
  \item Contruct the random surfer probability transition matrix using $\alpha = \rinline{alpha}$.
  \item Determine if the stationary distribution for the random surfer transition matrix is $\vec{p} = [~\rinline{paste(format(station), collapse="~")}~]$.
  \end{enumerate}

  \begin{workingbox}

 \begin{enumerate}
 \item The probability transition matrix is:
    % begin.rcode echo=FALSE,results="verbatim"
    print(T)
    % end.rcode 
 \item The graph is ergodic if there is a path from all vertices to all other vertices.
 \item The random surfer probability transition matrix using $\alpha = \rinline{alpha}$ is:
    % begin.rcode echo=FALSE,results="verbatim"
    print(R)
    % end.rcode 
 \item The stationary distribution satisfies $\vec{p} = T\vec{p}$. By multiplying $T$ and $\vec{p}$, we get:
    % begin.rcode echo=FALSE,results="verbatim"
    print(R %*% station)
    % end.rcode 
    If this is equal to $\vec{p}$, then $\vec{p}$ is the stationary distribution.

 \end{enumerate}


  
  \end{workingbox}]>)




m4_define(_sentiment_q1,<[

\squestion
  
  % begin.rcode echo=FALSE,results="hide"
    require("tm")
    $@
positive = c("My teeth shine #funfun","#funfun I love my teeth","#funfun is fun fun")
negative = c("No shine #funfun","No love for fun fun","Where is my teeth shine #funfun")

all.terms = names(table(strsplit(tolower(paste(positive, negative, collapse=" ")),split=" ")))
p = table(factor(strsplit(tolower(paste(positive, collapse=" ")),split=" ")[[1]],all.terms))
n = table(factor(strsplit(tolower(paste(negative, collapse=" ")),split=" ")[[1]],all.terms))
p.p = p/sum(p)
n.p = n/sum(n)

test = "my fun teeth shine"
tests = strsplit(tolower(test),split=" ")[[1]]
pos = which(all.terms %in% tests)


P.d.p = prod(p.p[pos])
P.d.n = prod(n.p[pos])
P.p = length(positive)/(length(positive) + length(negative))
P.n = length(negative)/(length(positive) + length(negative))

llr = log(P.p/P.n) + sum(log(p.p/n.p)[pos])
tweet.class = "positive"
if (llr < 0) tweet.class = "negative"

freqs = rbind(p,n)
rownames(freqs) = c("Positive","Negative")
probs = rbind(p.p,n.p)
rownames(probs) = c("Positive","Negative")

item = "\\item"

unhash = function(result) gsub("#", "\\#", result, fixed = TRUE)

 % end.rcode 

 Given the following set of labelled tweets:

 Positive tweets:
 \begin{itemize} \rinline{unhash(paste(item, positive, collapse=" "))}  \end{itemize}
 Negative Tweets:
 \begin{itemize}
 \rinline{unhash(paste(item, negative, collapse = " "))}
 \end{itemize}
  \begin{enumerate} 
  \item compute the log likelihood ratio of the tweet ``\rinline{test}'' being positive versus it being negative using Naive Bayes classification.,
  \item determine if the tweet has positive or negative sentiment, based on the log likelihood ratio.
  \end{enumerate}
  
  \begin{workingbox}

  The frequency of each word given its sentiment (positive or negative) is:
    % begin.rcode echo=FALSE,results="verbatim"
    print(freqs)
    % end.rcode 

  The probability of each word given its sentiment (positive or negative) is:
    % begin.rcode echo=FALSE,results="verbatim"
    print(probs)
    % end.rcode 

  The probability ratios are:
    % begin.rcode echo=FALSE,results="verbatim"
    print(p.p/n.p)
    % end.rcode 

  And the log probability ratios are:
    % begin.rcode echo=FALSE,results="verbatim"
    print(log(p.p/n.p))
    % end.rcode 

  The log likelihood ratio of the tweet ``\rinline{test}'' is:
  \begin{align*}
  \log{\frac{P(S|D)}{P(S'|D)}} &= \log{\frac{P(S)}{P(S')}} + {\sum_{i} \log{\frac{P(w_i|S)}{P(w_i|S')}}} \\
  &= \log{\frac{\rinline{P.p}}{\rinline{P.n}}} + \rinline{format(sum(log(p.p/n.p)[pos]))} \\
  &= \rinline{format(log(P.p/P.n) + sum(log(p.p/n.p)[pos]))}
  \end{align*}
  Therefore the tweet is classified as \rinline{tweet.class}.


  \end{workingbox}]>)
