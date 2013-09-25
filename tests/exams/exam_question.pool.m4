m4_changequote(`<[', `]>')

m4_define(_precision,<[
  % begin.rcode setup, echo=FALSE
  options(digits=$1)
  opts_chunk$set(comment=NA, highlight=FALSE)
  % end.rcode 
]>)



m4_define(_graphs_q1,<[

  \squestion
  
  % begin.rcode echo=FALSE, results="hide", message=FALSE
  require("igraph")
  $@
  #g = graph.formula(A-B, A-C, A-D, B-D)
  d = degree(g)
  close = 1/closeness(g)
  n = length(d)
  V(g)$color = "white"
  E(g)$color = "black"
  names(d) = LETTERS[1:n]
  % end.rcode 

  The following graph shows the Facebook connections between five soft drink companies.

  \begin{center}
    % begin.rcode echo=FALSE,results="asis", fig=TRUE, fig.width=4, fig.height=4
    par(mar = c(0,0,0,0))
    plot(g, layout=layout.fruchterman.reingold, vertex.size = 35, vertex.label = LETTERS[1:n])
    % end.rcode 
  \end{center}

  Using this graph:
  \begin{enumerate}
  \item Construct the adjacency matrix.
  \item Calculate the degree distribution.
  \item Calculate the closeness centrality for each vertex.
  \item Identify which soft drink company is the most central.
  \end{enumerate}
  
  \begin{workingbox}
    \marknote{For each of these four questions, give 1 mark for the correct
      working and 1 mark for the right answer.}
    
    \begin{enumerate}
    \item The adjacency matrix is:
      \begin{center}
        \begin{minipage}{0.5\textwidth}
          % begin.rcode echo=FALSE,results="verbatim", message=FALSE
          print(as.matrix(get.adjacency(g)))
          % end.rcode 
        \end{minipage}
        \xmark{1}
      \end{center}

      
    \item The degree distribution is:
      \begin{center}
        \begin{minipage}{0.5\textwidth}
          % begin.rcode echo=FALSE,results="verbatim"
          table(degree(g))
          % end.rcode 
        \end{minipage}
        \xmark{1}
      \end{center}
      
    \item The closeness centrality for each vertex is:
      \begin{center}
        \begin{tabular}{\rinline{paste(rep('c',n),collapse='')}}
          \rinline{paste(names(d),collapse=" & ")} \\
          \rinline{paste(close,collapse=" & ")}
        \end{tabular}
      \xmark{2}
      \end{center}

      
    \item The most central vertex is vertex \rinline{names(which.min(close))}.
      \xmark{1}
      
    \end{enumerate}
    
  \end{workingbox}
]>)



m4_define(_clustering_q1,<[
  
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
  wssf[is.na(wss)] = "-"
  z = kmeans(Z, centers=2)
  A = cbind(Z,z$cluster)
  colnames(A) = c("x1","x2","Cluster")
  colnames(z$centers) = c("x1","x2")

  # wss
  # z$totss - sum(diag(table(z$cluster)) %*% (z$centers)^2)
  
  % end.rcode 

  The following \rinline{n} centred data points ($\bar{x} = [ 0 ~ 0 ]$)
  and their associated cluster for $k = 2$:
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
  \item Determine how many clusters are the most suitable for the data.
  \end{enumerate}
  
  \begin{workingbox}
    
    \begin{enumerate}
    \item SSW = SST - SSB.  To find SSW, we either calculate the sum
      of squares of all distances from each cluster centre to its
      associated points, or calculate SSB, then SSW = SST - SSB.  SSB
      is the sum of squares of all distances from the centre of the
      points to the cluster centres.
      
      SSB = 
      \begin{align*}
        A = \left [ \rinline{latex.matrix(diag(table(z$cluster)))}  \right ]
        \left [ \rinline{latex.matrix(z$centers^2)} \right ] = 
        \left [ \rinline{latex.matrix(diag(table(z$cluster)) %*% z$centers^2)} \right ]
      \end{align*}
      giving SSB = \rinline{format(sum(diag(table(z$cluster)) %*% z$centers^2))}, therefore SSW = \rinline{format(tss)} - 
      \rinline{format(sum(diag(table(z$cluster)) %*% z$centers^2))} = 
      \rinline{format(tss - sum(diag(table(z$cluster)) %*% z$centers^2))}.
      \xmark{2}

        
      \item Plot the elbow bend plot for this data.
      \begin{center}
        \begin{minipage}{0.9\textwidth}
          \begin{center}
          % begin.rcode echo=FALSE,results="asis", fig=TRUE, fig.width=5, fig.height=4
          #par(mar = c(0,0,0,0))
          plot(1:cluster.count,rwss, xlab="Number of clusters", ylab="SSW")
          % end.rcode 

        \end{center}
        \end{minipage}
        \xmark{2}
      \end{center}
        
      \item Determine how many clusters is the most suitable for the data.
            
        The number of clusters is determined by where the elbow occurs.
        Therefore the student should report the position of the elbow.
        \xmark{1}
        
    \end{enumerate}


  
  \end{workingbox}
]>)



m4_define(_text_index_q1,<[

  %%%% construct text index, weight and query
  
  \squestion
  
  % begin.rcode echo=FALSE,results="hide", message=FALSE
  require("tm")
  require("SnowballC")
  $@
  docs <- c("Go dog, go!", "Stop cat, stop", "The dog stops the cat and the bird.")
  query = c("stop", "dog")
  stop = c("a", "the", "is", "to", "it", "and", "go")
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

  query.vec = rep(0,length(qcols))
  query.vec[which(qcols)] = 1
  
  inner.prod = apply(A[,qcols],1,sum)
  doc.length = sqrt(apply(A^2,1,sum))
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
    punctuation, casefolding, stopword removal and stemming has been
    performed. Use the stopword list \{\rinline{paste(stop, collapse=", ")}\}.
    
  \item Construct the document-term index containing term frequencies.

  \item Calculate the cosine similarity score of each document to the
    query ``\rinline{paste(query, collapse=" ")}'' using the term
    frequencies.
    
  \item If we used TF-IDF weights, show which word would have the
    greatest weight in document 3.

  \end{enumerate}
  
  \begin{workingbox}
    \begin{enumerate}
    \item The pre-processed documents are:
      \begin{enumerate}
      \item \rinline{corp.pp[[1]]}
      \item \rinline{corp.pp[[2]]}
      \item \rinline{corp.pp[[3]]}
      \end{enumerate}
      \xmark{1}
            
    \item Document-term index containing frequencies is:
      \begin{center}
      \begin{minipage}{0.5\textwidth}
      % begin.rcode echo=FALSE,results="verbatim"
      print(A)
      % end.rcode 
      \end{minipage}
      \xmark{1}
      \end{center}
      
    \item The query vector is $\vec{q} = [~\rinline{paste(query.vec, collapse="~")}~]$.
    Therefore the query score for each document using the query ``stop dog'' is:
      \begin{enumerate}
      \item $\rinline{format(inner.prod[1])}/(\rinline{format(doc.length[1])}\times\rinline{format(query.length)}) = \rinline{format(inner.prod[1]/(doc.length[1]*query.length))}$
      \item $\rinline{format(inner.prod[2])}/(\rinline{format(doc.length[2])}\times\rinline{format(query.length)}) = \rinline{format(inner.prod[2]/(doc.length[2]*query.length))}$
      \item $\rinline{format(inner.prod[3])}/(\rinline{format(doc.length[3])}\times\rinline{format(query.length)}) = \rinline{format(inner.prod[3]/(doc.length[3]*query.length))}$
      \end{enumerate}
      \xmark{2}
      
    \item TF-IDF weighting is $w_{d,t} = \log(f_{d,t} + 1)\log(N/f_t)$, where
      $N = \rinline{N}$, $f_t = [~\rinline{paste(ft, collapse="~")}~]$.
      The weights for document 3 are:
      % begin.rcode echo=FALSE,results="verbatim"
      print(W[3,])
      % end.rcode 
      Showing that \rinline{colnames(W)[which.max(W[3,])]} has the greatest weight.
      \xmark{1}
    \end{enumerate}
    
    
  \end{workingbox}
]>)


m4_define(_link_analysis_q1,<[

  %%%% Mean, standard deviation, mode, median, quantiles

  \squestion
  
  % begin.rcode echo=FALSE,results="hide"
  require("igraph")
  $@
  g = graph.formula(A+-+B, A+-C, A+-D, A+-E, B+-C, D+-+E)
  V(g)$color = "white"
  E(g)$color = "black"
  alpha = 0.8
  A = t(as.matrix(get.adjacency(g)))
  T = A %*% diag(1/apply(A,2,sum))
  N = dim(A)[1]
  J = matrix(rep(1/N, N*N), N, N)
  R = alpha*T + (1-alpha)*J
  e = eigen(R)
  pos = which(abs(e$values - 1) < 1e-8)
  station = e$vectors[,pos]/sum(e$vectors[,pos])
  non.station = c(0.49, 0.34, 0.10, 0.04, 0.03)
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
  \item State if the graph is ergodic and why or why not.
  \item Contruct the random surfer probability transition matrix using $\alpha = \rinline{alpha}$.
  \item Determine if the stationary distribution for the random surfer
    transition matrix is $\vec{p} = [~\rinline{paste(format(non.station), collapse="~")}~]$.
  \end{enumerate}
  
  \begin{workingbox}
    
    \begin{enumerate}
    \item The probability transition matrix is:
      \begin{center}
      \begin{minipage}{0.5\textwidth}
      % begin.rcode echo=FALSE,results="verbatim"
      print(T)
      % end.rcode 
      \end{minipage}
      \xmark{1}
      \end{center}
    \item The graph is ergodic if there is a path from all vertices to
      all other vertices. This student should report if all paths
      exist, and if not, where a path does not exist. \xmark{1}
      
    \item The random surfer probability transition matrix using $\alpha = \rinline{alpha}$ is:
      \begin{center}
      \begin{minipage}{0.5\textwidth}
      % begin.rcode echo=FALSE,results="verbatim"
      print(R)
      % end.rcode 
      \end{minipage}
      \xmark{2}
      \end{center}
    \item The stationary distribution satisfies $\vec{p} = T\vec{p}$. By multiplying $T$ and $\vec{p}$, we get:
      % begin.rcode echo=FALSE,results="verbatim"
      print(R %*% station)
      % end.rcode 
      If this is equal to $\vec{p}$, then $\vec{p}$ is the stationary distribution.
      \xmark{1}
      
    \end{enumerate}
    
    
  
  \end{workingbox}
]>)




m4_define(_sentiment_q1,<[

  \squestion
  
  % begin.rcode echo=FALSE,results="hide"
  require("tm")
  $@
  positive = c("My teeth shine #funfun","#funfun love my teeth","#funfun is fun fun")
  negative = c("No shine #funfun","No love fun fun","Where is my teeth shine #funfun")
  
  all.terms = names(table(strsplit(tolower(paste(positive, negative, collapse=" ")),split=" ")))
  p = table(factor(strsplit(tolower(paste(positive, collapse=" ")),split=" ")[[1]],all.terms))
  n = table(factor(strsplit(tolower(paste(negative, collapse=" ")),split=" ")[[1]],all.terms))
  p.p = p/sum(p)
  n.p = n/sum(n)
  
  test = "fun teeth shine"
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

  The Funfun toothpaste company wants to gauge the sentiment towards their toothpaste.
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
    \begin{center}
    \begin{minipage}{0.9\textwidth}
    % begin.rcode echo=FALSE,results="verbatim"
    print(probs, digits=2)
    % end.rcode 
    \end{minipage}
    ~\xmark{2}
    \end{center}
    
    The probability ratios are:
    % begin.rcode echo=FALSE,results="verbatim"
    print(p.p/n.p, digits=2)
    % end.rcode 
    
    And the log probability ratios are:
    % begin.rcode echo=FALSE,results="verbatim"
    print(log(p.p/n.p), digits=2)
    % end.rcode 
    
    The log likelihood ratio of the tweet ``\rinline{test}'' is:
    \begin{align*}
      \log{\frac{P(S|D)}{P(S'|D)}} &= \log{\frac{P(S)}{P(S')}} + {\sum_{i} \log{\frac{P(w_i|S)}{P(w_i|S')}}} \xmark{1} \\
      &= \log{\frac{\rinline{P.p}}{\rinline{P.n}}} + \rinline{format(sum(log(p.p/n.p)[pos]))} \\
      &= \rinline{format(log(P.p/P.n) + sum(log(p.p/n.p)[pos]))}  \xmark{1}
    \end{align*} 
    Therefore the tweet is classified as \rinline{tweet.class}. \xmark{1}
    
    
  \end{workingbox}
]>)

m4_define(_SimpleExposure_q1,<[
\squestion

The table below shows the counts of \emph{reach} by age group and gender for a particular facebook page.

\begin{center}
   % begin.rcode echo=FALSE, results="asis"
require(xtable, quietly=TRUE)
ages = c("13--17","18--24","25--34","35--44","45--54","55--65","65+")
genders = c("F","M")

pM = c(0.1,0.2,0.2,0.15,0.15,0.1,0.1)
pF = c(0.1,0.15,0.15,0.2,0.2,0.1,0.1)

#SEED = 12345
$@
set.seed(SEED)
x = t(cbind(rmultinom(1,44,pF), rmultinom(1,96,pM)))

dimnames(x) = list(genders, ages)

x = cbind(x, Total = rowSums(x))
x = rbind(x, Total=colSums(x))

algn = c(rep("r", ncol(x)), "|", "r")
print(xtable(x, digits=0, align=algn), floating=FALSE, hline.after=c(-1,0, nrow(x)-1, nrow(x)))
% end.rcode
\end{center}


The owner of this page is interested in determining whether the age profile of reach varies by gender.

\begin{enumerate}
\item Identify one problem with using a $\chi^2$ (Chi-squared) test
  with this data.
\item Write down a reduced table using new age groups 13--24, 25--34, 35--44,
  and 45+.
\item Find expected counts for each entry in the reduced table
  assuming gender and age group are independent.
\item Calculate a $\chi^2$ statistic for testing whether the age
  profile varies by gender, and state its degrees of freedom.
\end{enumerate}

]>)


m4_define(_BACI_q1,<[
\squestion

A major bank is about that to start an
advertising campaign. Before doing so 
it decides to collect information about mentions
on Twitter to measure the impact of the campaign. 
Using a search the company collects the number of
mentions of their product for 3 randomly chosen days before the
campaign and 3 randomly chosen days shortly
after the campaign begins. They also collect the number of mentions for
their main competitor on the same number of days before and after
the campaign. The data are in the left table below. The right table contains the
sums of the square roots of the data.


\begin{center}
% begin.rcode echo=FALSE, results="asis"
require(xtable, quietly=TRUE)
mu = c(100, 130, 70, 70)
n =3
X = factor(rep(c("Company","Competitor"), 2*c(n,n)))
Z = factor(rep(rep(c("Before","After"), c(n,n)),2), levels=c("Before","After"))
$@
set.seed(SEED)
Y = rpois(4*n, rep(mu, rep(n,4)))

tab=tapply(Y, list(X,Z), FUN=paste, collapse=",")
print(xtable(tab, align="|l|c|c|"), floating=FALSE, hline.after=c(-1,0,1,2))

cat("\\hspace{1cm}")
xtab = xtabs(sqrt(Y)~X+Z)
print(xtable(xtab, align="|l|r|r|", digits=2), floating=FALSE, hline.after=c(-1,0,1,2))
% end.rcode
\end{center}

\begin{enumerate}
\item Explain why using a square root transformation is advisable for
  count data.
\item Calculate the \emph{contrast} for the interaction between
  company and time.
\item Calculate the \emph{Sum of Squares} for the interaction between
  company and time.
\item Given that the sum of squares for error is 1.820, find the
  $F$-statistic for the interaction between
  company and time, and state its degrees of freedom.
\end{enumerate}


]>)

m4_define(_Trend_q1,<[
\squestion

The jTele mobile phone company is looking at the tweet count containing their hash-tag, in the three days after the release
of a new model. The counts are aggregated into four time periods; midnight to 6am, 6am to noon, noon to 6pm and 6pm to midnight.
The data are below.

\begin{center}
% begin.rcode echo=FALSE, results="asis"
s = c(10,20,30,15)
trend = seq(20, 70, length=12)

$@
#set.seed(56235)
set.seed(SEED)
xx = rpois(length(trend), trend + s)
tmp = matrix(xx, ncol=4, byrow=TRUE)
dimnames(tmp) = list(paste("Day",1:3), paste("Period", 1:4))
print(xtable(tmp), floating=FALSE)
% end.rcode
\end{center}

The company decides to estimate the trend in the data, after a square root transformation, and allowing for a 
periodic (seasonal) effect. The tables below shows the estimated moving average trend, and periodic components.

\begin{center}
{\bf Trend}\\[1ex]
% begin.rcode echo=FALSE, results="asis"
d = decompose(ts(sqrt(xx), freq=4))
et = as.numeric(d$trend)
etc = formatC(et, format="f", digits=2)
etc[etc==" NA"] = ""
tmp = matrix(etc, ncol=4, byrow=TRUE)
dimnames(tmp) = list(paste("Day",1:3), paste("Period", 1:4))

tmp[1,4] = "$\\star$"
print(xtable(tmp), floating=FALSE, sanitize.text.function=function(x) x)
% end.rcode

{\bf Periodic}\\[1ex]
% begin.rcode echo=FALSE, results="asis"
ss = d$figure
ssc = formatC(ss, format="f", digits=3)
tmp = matrix(ssc, ncol=4, byrow=TRUE)
dimnames(tmp) = list("Periodic", paste("Period", 1:4))

tmp[1,4] = "$\\star$"
print(xtable(tmp), floating=FALSE, sanitize.text.function=function(x) x)
% end.rcode
\end{center}

\begin{enumerate}
\item Compute the missing trend component marked with a $\star$.
\item Compute the missing periodic component marked with a $\star$.
\end{enumerate}
]>)

m4_define(_Visual_q1,<[
\squestion

\begin{enumerate}
\item In conducting a principal component analysis R produces the following summary output.

% begin.rcode echo=FALSE, results="asis"
require(clusterGeneration, quietly=TRUE)
require(mvtnorm, quietly=TRUE)
#set.seed(66245)
$@
set.seed(SEED)

S = genPositiveDefMat(7)
X = rmvnorm(50, sigma=S$Sigma)
tab = summary(prcomp(X))$importance
tab = formatC(tab, format="f", digits=3)
tab[3,c(1,4)] = "$\\star$"
print(xtable(tab), floating=FALSE, sanitize.text.function=function(x) x)
% end.rcode
Compute the missing entries marked $\star$.

\item Compute the binary metric between the following two tweets.
\begin{center}
\begin{tabular}{lc}
Tweet 1&assault assistance disadvantaged university students begins\\
Tweet 2&believe more students doing university better
\end{tabular}
\end{center}
\end{enumerate}

]>)