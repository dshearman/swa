m4_changequote(`<[', `]>')

m4_define(_precision,<[
\begin{Scode}{echo=false,results=hide}
  options(digits=$1)
\end{Scode}]>)


m4_define(_q1,<[

%%%% Mean, standard deviation, mode, median, quantiles

\squestion
  
  \begin{Scode}{echo=false,results=hide}
    require("igraph")
    $@
    #g = graph.formula(A-B, A-C, A-D, B-D)
    d = degree(g)
    close = 1/closeness(g)
    n = length(d)
    names(d) = LETTERS[1:n]
 \end{Scode}

 Using the following graph:

 \begin{center}
  \scalebox{0.6}{
    \begin{Scode}{echo=false,results=tex,fig=true, width=2, height=2}
    par(mar = c(0,0,0,0))
    plot(g, layout=layout.fruchterman.reingold, vertex.size = 35, vertex.label = LETTERS[1:n])
    \end{Scode}
  }
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
    \begin{Scode}{echo=false,results=verbatim}
    print(as.matrix(get.adjacency(g)))
    \end{Scode}

  \item	The degree distribution is:
    \begin{Scode}{echo=false,results=verbatim}
    table(degree(g))
    \end{Scode}

  \item The closeness centrality for each vertex is:
    \begin{center}
      \begin{tabular}{\Sexpr{paste(rep('c',n),collapse='')}}
        \Sexpr{print(paste(names(d),collapse=" & "))} \\
        \Sexpr{print(paste(close,collapse=" & "))}
      \end{tabular}
    \end{center}
  \item The most central vertex is vertex \Sexpr{names(which.min(close))}.
  
  \end{enumerate}

  \end{workingbox}]>)



m4_define(_q2,<[

%%%% Mean, standard deviation, mode, median, quantiles

\squestion
  
  \begin{Scode}{echo=false,results=hide}
    require("mvtnorm")
    require("xtable")
    $@
    #g = graph.formula(A-B, A-C, A-D, B-D)

latex.matrix = function(X) {
  nc = ncol(X)
  nr = nrow(X) 
  cat("\\begin{array}{", paste(rep('c',nc),collapse=''), "}\n", sep="")
  for (a in 1:nr) {
    cat(paste(format(X[a,]),collapse=" & "), " \\\\\n", sep="")
  }
  cat("\\end{array}\n", sep="")
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

 \end{Scode}

 The following \Sexpr{n} centred data points ($\bar{x} = [ 0 ~ 0 ]$)
 and their associated cluster:
 \begin{Scode}{echo=false,results=verbatim}
 print(A)
 \end{Scode}
 have the within cluster sum of squares (SSW) values:
    \begin{center}
      \begin{tabular}{c|\Sexpr{paste(rep('c',cluster.count),collapse='')}}
        $k$ & \Sexpr{print(paste(1:cluster.count,collapse=" & "))} \\
	\hline
        SSW & \Sexpr{print(paste(wssf,collapse=" & "))}
      \end{tabular}
    \end{center}
 and the total sum of squares value (SST) \Sexpr{format(tss)}.

 Given that the cluster centres for $k = 2$ are:
 \begin{Scode}{echo=false,results=verbatim}
 print(z$centers)
 \end{Scode}
 \begin{enumerate}
 \item Compute the SSW for $k = 2$.
 \item Plot the elbow bend plot for this data.
 \item Determine how many clusters is the most suitable for the data.
 \end{enumerate}

  \begin{workingbox}

 \begin{enumerate}
 \item SSW = SST - SSB. SSB = 
\begin{align*}
 A = \left [
 \begin{Scode}{echo=false,results=tex}
 latex.matrix(diag(table(z$cluster)))
 \end{Scode}
 \right ]
 \left [
 \begin{Scode}{echo=false,results=tex}
 latex.matrix(z$centers^2)
 \end{Scode}
 \right ] = 
 \left [
 \begin{Scode}{echo=false,results=tex}
 latex.matrix(diag(table(z$cluster)) %*% z$centers^2)
 \end{Scode}
 \right ]
\end{align*}
giving SSB = \Sexpr{format(sum(diag(table(z$cluster)) %*% z$centers^2))}, therefore SSW = \Sexpr{format(tss)} - 
\Sexpr{format(sum(diag(table(z$cluster)) %*% z$centers^2))} = 
\Sexpr{format(tss - sum(diag(table(z$cluster)) %*% z$centers^2))}.

 \item Plot the elbow bend plot for this data.
 \begin{center}
  \scalebox{0.9}{
    \begin{Scode}{echo=false,results=tex,fig=true, width=5, height=4}
    #par(mar = c(0,0,0,0))
    plot(1:cluster.count,rwss, xlab="Number of clusters", ylab="SSW")
    \end{Scode}
  }
  \end{center}

 \item Determine how many clusters is the most suitable for the data.

 The number of clusters is determined by where the elbow occurs.

 \end{enumerate}


  
  \end{workingbox}]>)



m4_define(_text_index_q1,<[

%%%% construct text index, weight and query

\squestion
  
  \begin{Scode}{echo=false,results=hide}
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
 \end{Scode}

  Using the three documents:

  \begin{enumerate}[i.]
  \item \Sexpr{docs[1]}
  \item \Sexpr{docs[2]}
  \item \Sexpr{docs[3]}
  \end{enumerate}

  \begin{enumerate}

  \item Write out the contents of each document after removing
  puctuation, casefolding, stopword removal and stemming has been
  performed. Use the stopword list \{\Sexpr{paste(stop, collapse=", ")}\}.

  \item Construct the document-term index containing term frequencies.

  \item Construct the document-term index containing TF-IDF weights.
 
  \item Calculate the query score for each document using the query ``\Sexpr{paste(query, collapse=" ")}''.
  \end{enumerate}

  \begin{workingbox}
    \begin{enumerate}
    \item The pre-processed documents are:
        \begin{enumerate}
	\item \Sexpr{corp.pp[[1]]}
	\item \Sexpr{corp.pp[[2]]}
	\item \Sexpr{corp.pp[[3]]}
	\end{enumerate}
    
    \item Document-term index containing frequencies is:
    \begin{Scode}{echo=false,results=verbatim}
    print(A)
    \end{Scode}

    \item TF-IDF weighting is $w_{d,t} = \log(f_{d,t} + 1)\log(N/f_t)$. 
    $N = \Sexpr{N}$, $f_t = [~\Sexpr{paste(ft, collapse="~")}~]$.
    Document-term index containing TF-IDF weights is:
    \begin{Scode}{echo=false,results=verbatim}
    print(W)
    \end{Scode}
    
    \item The query score for each document using the query ``stop dog'' is:
    \begin{enumerate}
    \item $\Sexpr{format(inner.prod[1])}/(\Sexpr{format(doc.length[1])}\times\Sexpr{format(query.length)}) = \Sexpr{format(inner.prod[1]/(doc.length[1]*query.length))}$
    \item $\Sexpr{format(inner.prod[2])}/(\Sexpr{format(doc.length[2])}\times\Sexpr{format(query.length)}) = \Sexpr{format(inner.prod[2]/(doc.length[2]*query.length))}$
    \item $\Sexpr{format(inner.prod[3])}/(\Sexpr{format(doc.length[3])}\times\Sexpr{format(query.length)}) = \Sexpr{format(inner.prod[3]/(doc.length[3]*query.length))}$
    \end{enumerate}

  \end{enumerate}

  
  \end{workingbox}]>)


m4_define(_link_analysis_q1,<[

%%%% Mean, standard deviation, mode, median, quantiles

\squestion
  
  \begin{Scode}{echo=false,results=hide}
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
 \end{Scode}


 Using the following graph:

 \begin{center}
  \scalebox{0.6}{
    \begin{Scode}{echo=false,results=tex,fig=true, width=2, height=2}
    par(mar = c(0,0,0,0))
    plot(g, layout=layout.fruchterman.reingold, vertex.size = 35, vertex.label = LETTERS[1:N])
    \end{Scode}
  }
  \end{center}

  \begin{enumerate}
  \item Contruct the probability transition matrix.
  \item State if the graph ergodic and why or why not.
  \item Contruct the random surfer probability transition matrix using $\alpha = \Sexpr{alpha}$.
  \item Determine if the stationary distribution for the random surfer transition matrix is $\vec{p} = [~\Sexpr{paste(format(station), collapse="~")}~]$.
  \end{enumerate}

  \begin{workingbox}

 \begin{enumerate}
 \item The probability transition matrix is:
    \begin{Scode}{echo=false,results=verbatim}
    print(T)
    \end{Scode}
 \item The graph is ergodic if there is a path from all vertices to all other vertices.
 \item The random surfer probability transition matrix using $\alpha = \Sexpr{alpha}$ is:
    \begin{Scode}{echo=false,results=verbatim}
    print(R)
    \end{Scode}
 \item The stationary distribution satisfies $\vec{p} = T\vec{p}$. By multiplying $T$ and $\vec{p}$, we get:
    \begin{Scode}{echo=false,results=verbatim}
    print(R %*% station)
    \end{Scode}
    If this is equal to $\vec{p}$, then $\vec{p}$ is the stationary distribution.

 \end{enumerate}


  
  \end{workingbox}]>)




m4_define(_sentiment_q1,<[

\squestion
  
  \begin{Scode}{echo=false,results=hide}
    require("tm")
    $@
    positive <- c("This is the best!","I like it a lot.")
    negative <- c()
    test = "Getting one next week."
 \end{Scode}

 Compute the log likelihood ratio for positive versus negative
 sentiment of the tweet ``\Sexpr{print(test)}'' using the training set:

  \begin{workingbox}


  
  \end{workingbox}]>)
