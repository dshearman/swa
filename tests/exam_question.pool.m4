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
    n = length(d)
    names(d) = LETTERS[1:n]
 \end{Scode}


 Calculate the degree centrality for each vertex in the following
 graph and identify which vertex is the most central according to
 degree centrality.

 \begin{center}
  \scalebox{0.6}{
    \begin{Scode}{echo=false,results=tex,fig=true, width=2, height=2}
    par(mar = c(0,0,0,0))
    plot(g, layout=layout.fruchterman.reingold, vertex.size = 35, vertex.label = LETTERS[1:n])
    \end{Scode}
  }
  \end{center}

  \begin{workingbox}
    \marknote{For each of these four questions, give 1 mark for the correct
    working and 1 mark for the right answer.}
    
    The degree centrality for each vertex is:
    \begin{center}
      \begin{tabular}{\Sexpr{paste(rep('c',n),collapse='')}}
        \Sexpr{print(paste(names(d),collapse=" & "))} \\
        \Sexpr{print(paste(d,collapse=" & "))}
      \end{tabular}
    \end{center}
    The most central vertex is vertex \Sexpr{names(which.max(d))}.
  
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

