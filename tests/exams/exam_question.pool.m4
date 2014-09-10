% -*- Rnw -*-

m4_changequote(`<[', `]>')

m4_define(_precision,<[
  % begin.rcode setup, echo=FALSE
  options(digits=$1)
  opts_chunk$set(comment=NA, highlight=FALSE)

  missing.symbol = "$\\star$"
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
  V(g)$label.color = "black"
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
  \item Tabulate the degree distribution.
  \item Calculate the closeness centrality for each vertex.
  \item Using the above results, identify which soft drink company is the most central.
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
          table(factor(degree(g), c(0,1,2,3,4,5)))
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

      
    \item The most central vertex is vertex \rinline{names(which(close == min(close)))}.
      \xmark{1}
      
    \end{enumerate}
    
  \end{workingbox}
]>)



m4_define(_clustering_q1,<[
  
  \squestion
  
  % begin.rcode echo=FALSE,results="hide", message=FALSE
  require("mvtnorm")
  require("xtable")
  
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
  $@

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
  wssf[is.na(wss)] = missing.symbol
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
  \item Determine how many clusters are the most suitable for the data and explain why.
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
      \xmark{2}
            
    \item Document-term index containing frequencies is:
      \begin{center}
      \begin{minipage}{0.5\textwidth}
      % begin.rcode echo=FALSE,results="verbatim"
      print(A)
      % end.rcode 
      \end{minipage}
      \xmark{2}
      \end{center}
      
    \item The query vector is $\vec{q} = [~\rinline{paste(query.vec, collapse="~")}~]$.
    Therefore the query score for each document using the query ``\rinline{paste(query, collapse=" ")}'' is:
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
      \xmark{2}
    \end{enumerate}
    
    
  \end{workingbox}
]>)


m4_define(_link_analysis_q1,<[

  %%%% Mean, standard deviation, mode, median, quantiles

  \squestion
  
  % begin.rcode echo=FALSE,results="hide"
  require("igraph")
  $@

  V(g)$color = "white"
  V(g)$label.color = "black"
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
  
  % begin.rcode echo=FALSE,results="hide",message=FALSE
  require("tm")
  $@
  
  all.terms = names(table(strsplit(tolower(paste(positive, negative, collapse=" ")),split=" ")))
  p = table(factor(strsplit(tolower(paste(positive, collapse=" ")),split=" ")[[1]],all.terms))
  n = table(factor(strsplit(tolower(paste(negative, collapse=" ")),split=" ")[[1]],all.terms))
  p.p = p/sum(p)
  n.p = n/sum(n)
  
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
  \item calculate and tabulate the word distribution for both positive and negative sentiment.
  \item compute the log likelihood ratio of the tweet ``\rinline{test}'' being positive versus it being negative, using Naive Bayes classification.
  \item determine if the tweet ``\rinline{test}'' has positive or negative sentiment, based on the log likelihood ratio.
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

% GS
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


% GS
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
RSS = sum(aov(sqrt(Y)~X*Z)$residuals^2)
% end.rcode
\end{center}

\begin{enumerate}
\item Explain why using a square root transformation is advisable for
  count data.
\item Calculate the \emph{contrast} for the interaction between
  company and time.
\item Calculate the \emph{Sum of Squares} for the interaction between
  company and time.
\item Given that the sum of squares for error is \rinline{round(RSS,3)}, find the
  $F$-statistic for the interaction between
  company and time, and state its degrees of freedom.
\end{enumerate}


]>)

% GS
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

tmp[1,4] = missing.symbol
print(xtable(tmp), floating=FALSE, sanitize.text.function=function(x) x)
% end.rcode

{\bf Periodic}\\[1ex]
% begin.rcode echo=FALSE, results="asis"
ss = d$figure
ssc = formatC(ss, format="f", digits=3)
tmp = matrix(ssc, ncol=4, byrow=TRUE)
dimnames(tmp) = list("Periodic", paste("Period", 1:4))

tmp[1,4] = missing.symbol
print(xtable(tmp), floating=FALSE, sanitize.text.function=function(x) x)
% end.rcode
\end{center}

\begin{enumerate}
\item Compute the missing trend component marked with a \rinline{missing.symbol}.
\item Compute the missing periodic component marked with a \rinline{missing.symbol}.
\end{enumerate}
]>)

% GS
m4_define(_Visual_q1,<[
\squestion

In conducting a principal component analysis R produces the following summary output.

% begin.rcode echo=FALSE, results="asis"
require(clusterGeneration, quietly=TRUE)
require(mvtnorm, quietly=TRUE)
#set.seed(66245)
$@
set.seed(SEED)

S = genPositiveDefMat(7)
X = rmvnorm(50, sigma=S$Sigma)
tab = round(summary(prcomp(X))$importance,3)
tab[3,] = cumsum(tab[2,])
tab = formatC(tab, format="f", digits=3)
ans = tab[3, c(1,4)]
tab[3,c(1,4)] = missing.symbol
print(xtable(tab), floating=FALSE, sanitize.text.function=function(x) x)
% end.rcode
Compute the missing entries marked \rinline{missing.symbol}.

  \begin{workingbox}
    Missing entries are \rinline{ans[1]} and \rinline{ans[2]} respectively.
\xmark{2}
  \end{workingbox}
]>)

% GS
m4_define(_Visual_q2,<[
\squestion

% begin.rcode echo=FALSE, results="asis"
#set.seed(66245)
$@
set.seed(SEED)
% end.rcode

Compute the binary metric between the following two tweets.
\begin{center}
\begin{tabular}{ll}
Tweet 1: & assault assistance disadvantaged university students begins\\
Tweet 2: & believe more students doing university better
\end{tabular}
\end{center}

]>)

m4_define(_graphs_q2,<[

  \squestion
  
  % begin.rcode echo=FALSE, results="hide", message=FALSE
  require("igraph")
  $@
  #g = graph.formula(A-B, A-C, A-D, B-D)
  d = degree(g)
  b = betweenness(g)
  close = 1/closeness(g)
  n = length(d)
  V(g)$color = "white"
  V(g)$label.color = "black"
  E(g)$color = "black"
  names(d) = LETTERS[1:n]
  % end.rcode 

  The following graph shows the relationships between a set of YouTube clips.

  \begin{center}
    % begin.rcode echo=FALSE,results="asis", fig=TRUE, fig.width=4, fig.height=4
    par(mar = c(0,0,0,0))
    plot(g, layout=layout.fruchterman.reingold, vertex.size = 35, vertex.label = LETTERS[1:n])
    % end.rcode 
  \end{center}

  Using this graph:
  \begin{enumerate}
  \item Construct the adjacency matrix.
  \item Compute the graph diameter.
  \item Calculate the betweenness centrality for each vertex.
  \item Identify if this graph is more similar to an Erdös-Renyi graph or a Barabasi-Albert graph.
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
        \xmark{2}
      \end{center}

      
    \item The diameter is the longest shortest path. For this graph, the diameter is \rinline{diameter(g)}.
      \begin{center}
        \begin{minipage}{0.5\textwidth}
          % begin.rcode echo=FALSE,results="verbatim"
          table(factor(degree(g), c(0,1,2,3,4,5)))
          % end.rcode 
        \end{minipage}
        \xmark{2}
      \end{center}
      
    \item The betweenness centrality is a sum of the proportion of shortest paths that pass through the vertex. The betweenness centrality for each vertex is:
      \begin{center}
        \begin{tabular}{\rinline{paste(rep('c',n),collapse='')}}
          \rinline{paste(names(b),collapse=" & ")} \\
          \rinline{paste(b,collapse=" & ")}
        \end{tabular}
      \xmark{2}
      \end{center}

      
    \item The degree distribution must be examined and compared to
      each of the random graph degree distributions. The one with the
      more similar distribution is the more similar graph.  \xmark{2}
      
    \end{enumerate}
    
  \end{workingbox}
]>)


m4_define(_clustering_q2,<[
  
  \squestion
  
  % begin.rcode echo=FALSE,results="hide", message=FALSE
  require("mvtnorm")
  require("xtable")
  
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
  $@

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
  bss[2] = NA
  wssf = format(wss)
  bssf = format(bss)
  wssf[is.na(wss)] = missing.symbol
  bssf[is.na(bss)] = missing.symbol
  z = kmeans(Z, centers=2)
  A = cbind(Z,z$cluster)
  colnames(A) = c("x1","x2","Cluster")
  colnames(z$centers) = c("x1","x2")

  ssb = sum(diag(table(z$cluster)) %*% z$centers^2)

  bss.complete = bss
  bss.complete[2] = ssb
  
  # wss
  # z$totss - sum(diag(table(z$cluster)) %*% (z$centers)^2)
  
  % end.rcode 

  The following \rinline{n} centred data points ($\bar{x} = [ 0 ~ 0 ]$)
  and their associated cluster for $k = 2$:
  % begin.rcode echo=FALSE,results="verbatim"
  print(A)
  % end.rcode 
  have the between cluster sum of squares (SSB) values:
  \begin{center}
    \begin{tabular}{c|\rinline{paste(rep('c',cluster.count),collapse='')}}
      $k$ & \rinline{paste(1:cluster.count,collapse=" & ")} \\
      \hline
      SSB & \rinline{format(paste(bssf,collapse=" & "))}
    \end{tabular}
  \end{center}
  and the total sum of squares value (SST) \rinline{format(tss)}.
  
  Given that the cluster centres for $k = 2$ are:
  % begin.rcode echo=FALSE,results="verbatim"
  print(z$centers)
  % end.rcode 
  \begin{enumerate}
  \item Compute the SSB for $k = 2$.
  \item Plot the elbow bend plot for this data.
  \item Determine how many clusters are the most suitable for the data and explain why.
  \end{enumerate}
  
  \begin{workingbox}
    
    \begin{enumerate}
    \item SSB is the sum of squares of all distances from the centre
      of the points to the cluster centres.
      
      SSB = 
      \begin{align*}
        A = \left [ \rinline{latex.matrix(diag(table(z$cluster)))}  \right ]
        \left [ \rinline{latex.matrix(z$centers^2)} \right ] = 
        \left [ \rinline{latex.matrix(diag(table(z$cluster)) %*% z$centers^2)} \right ]
      \end{align*}
      giving SSB = \rinline{format(ssb)}.
        \xmark{4}
        
      \item Plot the elbow bend plot for this data.
      \begin{center}
        \begin{minipage}{0.9\textwidth}
          \begin{center}
          % begin.rcode echo=FALSE,results="asis", fig=TRUE, fig.width=5, fig.height=4
          #par(mar = c(0,0,0,0))
          plot(1:cluster.count,bss.complete, xlab="Number of clusters", ylab="SSB")
          % end.rcode 

        \end{center}
        \end{minipage}
        \xmark{2}
      \end{center}
        
      \item Determine how many clusters is the most suitable for the data.
            
        The number of clusters is determined by where the elbow occurs.
        Therefore the student should report the position of the elbow.
        \xmark{2}
        
    \end{enumerate}


  
  \end{workingbox}
]>)

% GS

m4_define(_SimpleExposure_qS,<[
\squestion

The table below shows the counts of \emph{reach} by age group and gender for a particular facebook page.

\begin{center}
   % begin.rcode echo=FALSE, results="asis"
require(xtable, quietly=TRUE)
ages = c("13--24","25--34","35--44","45+")
genders = c("F","M")

pM = c(0.2,0.4,0.3,0.1)
pF = c(0.1,0.3,0.5,0.1)

#SEED = 12345
$@
set.seed(SEED)
x = t(cbind(rmultinom(1,75,pF), rmultinom(1,125,pM)))

dimnames(x) = list(genders, ages)

x = cbind(x, Total = rowSums(x))
x = rbind(x, Total=colSums(x))

algn = c(rep("r", ncol(x)), "|", "r")
print(xtable(x, digits=0, align=algn), floating=FALSE, hline.after=c(-1,0, nrow(x)-1, nrow(x)))
% end.rcode
\end{center}


The owner of this page is interested in determining whether the age profile of reach varies by gender.

\begin{enumerate}
\item Find expected counts for each entry in the table
  assuming gender and age group are independent.
\item Calculate a $\chi^2$ statistic for testing whether the age
  profile varies by gender, and state its degrees of freedom.
\item Compute a 95\% confidence interval for the proportion of females. (Recall that $z_{0.025} = 1.960$)
\end{enumerate}

  \begin{workingbox}
    
    \begin{enumerate}
\item Expected counts are $\frac{Row\times Column}{Total}$ which gives;
\begin{center}
% begin.rcode echo=FALSE, results="asis"
r = nrow(x)
c = ncol(x)
e = round(outer(x[-r,c], x[r,-c], "*")/ x[r,c],3)
r = r-1
c = c-1
X2 = sum((x[1:r,1:c]-e)^2/e)
df = (r-1)*(c-1)
nF = x["F","Total"]
nT = x["Total","Total"]
p = nF/nT
sp = sqrt(p*(1-p)/nT)
algn = c(rep("r", ncol(e)), "r")
print(xtable(e, digits=3, align=algn), floating=FALSE)#, hline.after=c(-1,0, nrow(x)-1, nrow(x)))
% end.rcode
\end{center}
\xmark{2}
\item $\chi^2$ statistic is $\sum \frac{(O-E)^2}{E}$ which gives \rinline{X2} on \rinline{df} degrees of freedom.\xmark{3}
\item The proportion of females is \rinline{p} therefore a 95\% comfidemce interval is given by
$$\hat{p} \pm z_{\alpha/2} \sqrt{\hat{p}(1-\hat{p})/N}$$
which is
$$\rinline{p} \pm 1.960 \times\sqrt{\rinline{p*(1-p)/nT}}$$
that is,
$$(\rinline{p-1.960*sp}, \rinline{p+1.960*sp})$$\xmark{3}
\end{enumerate}

\end{workingbox}
]>)


% GS
m4_define(_Visual_qS,<[
\squestion

% begin.rcode echo=FALSE, results="asis"
#set.seed(66245)
$@
set.seed(SEED)
tweet1 = "Remembering Lou Reed lifes work rock musician"
tweet2 = "Lou Reed proved career rock music mean striving publicity"
fun = function(x) length(unique(unlist(strsplit(x," "))))
n1 = fun(tweet1)
n2 = fun(tweet2)
n12 = fun(paste(tweet1,tweet2))
% end.rcode

Compute the binary metric between the following two tweets, which have had stop words removed.
\begin{center}
\begin{tabular}{ll}
Tweet 1: & \rinline{tweet1}\\
Tweet 2: & \rinline{tweet2}\\
\end{tabular}
\end{center}
Would stemming change the result

\begin{workingbox}
Tweet1 has \rinline{n1} unique words and tweet2 has \rinline{n2} unique words. There are \rinline{n1+n2-n12} words in common.
The binary metric is therefore, \rinline{(2*n12-n1-n2)/n12}. \xmark{1}

In the case stemming would cchange the result as "musician" 
would stem the same as "music" increasing the number of common words. \xmark{1}
\end{workingbox}
]>)

% GS
m4_define(_BACI_qS,<[
\squestion

A retail chain is about to start an
advertising campaign. Before doing so 
it collects information about mentions
on Twitter to measure the impact of the campaign. 
The company collects the number of
mentions of their product for 4 randomly chosen days before the
campaign and 4 randomly chosen days shortly
after the campaign begins. They also collect the number of mentions for
their main competitor. The data are in the first table below. The second table contains the
sums of the square roots of the data.


\begin{center}
% begin.rcode echo=FALSE, results="asis"
require(xtable, quietly=TRUE)
mu = c(200,230,150,150)
n =4
X = factor(rep(c("Company","Competitor"), 2*c(n,n)))
Z = factor(rep(rep(c("Before","After"), c(n,n)),2), levels=c("Before","After"))
$@
set.seed(SEED)
Y = rpois(4*n, rep(mu, rep(n,4)))

tab=tapply(Y, list(X,Z), FUN=paste, collapse=",")
print(xtable(tab, align="|l|c|c|"), floating=FALSE, hline.after=c(-1,0,1,2))

cat("\\\\[2ex]")
xtab = xtabs(sqrt(Y)~X+Z)
cI = xtab[1,1]+xtab[2,2] - xtab[1,2] - xtab[2,1]
SSI = cI^2/(4*n)
print(xtable(xtab, align="|l|r|r|", digits=2), floating=FALSE, hline.after=c(-1,0,1,2))
RSS = sum(aov(sqrt(Y)~X*Z)$residuals^2)
MSE = round(RSS,3)/(4*(n-1))
% end.rcode
\end{center}

\begin{enumerate}
\item Explain why using a square root transformation is advisable for
  count data.
\item Calculate the \emph{Sum of Squares} for the interaction between
  company and time.
\item Given that the sum of squares for error is \rinline{round(RSS,3)} find the
  $F$-statistic for the interaction between
  company and time, and state its degrees of freedom.
\end{enumerate}

\begin{workingbox}
\begin{enumerate}
\item Count data would have a variance proportional (equal to) the mean. ie non-constant variance.
The square root transformation is variance stabilising. \xmark{2}
\item The sum of squares for the interaction is \rinline{SSI}. \xmark{4}
\item The $F$-statistic is \rinline{SSI/MSE} on 1 and \rinline{4*(n-1)} degrees of freedom. \xmark{2}
\end{enumerate}
\end{workingbox} 

]>)

% GS
m4_define(_Trend_qS,<[
\squestion

A mobile phone company is looking at the tweet count containing their hash-tag, in the three weeks after the release
of a new model. The counts are aggregated to the daily level.
The data are below.

\begin{center}
% begin.rcode echo=FALSE, results="asis"
s = c(10,20,30,30,30,20,10)
trend = seq(20, 130, length=21)

$@
#set.seed(782355)
set.seed(SEED)
xx = rpois(length(trend), trend + s)
tmp = matrix(xx, ncol=7, byrow=TRUE)
dimnames(tmp) = list(paste("Week",1:3), paste("Day", 1:7))
print(xtable(tmp), floating=FALSE)
% end.rcode
\end{center}

The company decides to estimate the trend in the data, after a square root transformation, and allowing for a 
periodic (seasonal) effect. The tables below shows the estimated moving average trend, and periodic components.

\begin{center}
{\bf Trend}\\[1ex]
% begin.rcode echo=FALSE, results="asis"
d = decompose(ts(sqrt(xx), freq=7))
et = as.numeric(d$trend)
etc = formatC(et, format="f", digits=2)
etc[etc==" NA"] = ""
tmp = matrix(etc, ncol=7, byrow=TRUE)
dimnames(tmp) = list(paste("Week",1:3), paste("Day", 1:7))
adat = round(sqrt(xx)[8:14],3)
tmp[2,4] = missing.symbol
print(xtable(tmp), floating=FALSE, sanitize.text.function=function(x) x)
% end.rcode

{\bf Periodic}\\[1ex]
% begin.rcode echo=FALSE, results="asis"
ss = d$figure
ssc = formatC(ss, format="f", digits=3)
tmp = matrix(ssc, ncol=7, byrow=TRUE)
dimnames(tmp) = list("Periodic", paste("Day", 1:7))
tmp[1,6] = missing.symbol
ansS = -sum(as.numeric(tmp[1,-6]))
print(xtable(tmp), floating=FALSE, sanitize.text.function=function(x) x)
% end.rcode
\end{center}

\begin{enumerate}
\item Compute the missing trend component marked with a \rinline{missing.symbol}.
\item Compute the missing periodic component marked with a \rinline{missing.symbol}.
\end{enumerate}

\begin{workingbox}
\begin{enumerate}
\item The trend is a 7-point moving average on the sqrt scale, so is
\begin{center}
\rinline{paste("(", paste(adat, collapse="+"), ")/7", sep="")} $=$ \rinline{sum(adat)/7}
\end{center}
\xmark{4}

\item The seasonal component should sum to zero so the missing component is \rinline{ansS}
\xmark{2}
\end{enumerate}
\end{workingbox}
]>)


m4_define(_link_analysis_q2,<[

  %%%% Mean, standard deviation, mode, median, quantiles

  \squestion
  
  % begin.rcode echo=FALSE,results="hide"
  require("igraph")
  require("xtable")
  $@

  V(g)$color = "white"
  V(g)$label.color = "black"
  E(g)$color = "black"
  alpha = 0.8
  A = t(as.matrix(get.adjacency(g)))
  T = A %*% diag(1/apply(A,2,sum))
  N = dim(A)[1]
  J = matrix(rep(1/N, N*N), N, N)
  R = alpha*T + (1-alpha)*J
  e = eigen(R)
  pos = which(abs(e$values - 1) < 1e-8)
  station = degree(g)/sum(degree(g))
  begin.dist = rep(0,N)
  begin.dist[1] = 1
  
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
  \item Compute the state distribution after we begin on vertex 1 and
    take two random steps.
  \item Compute the stationary distribution for the graph and verify
    that it is the stationary distribution.
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
        \xmark{2}
      \end{center}
    \item The graph is ergodic if there is a path from all vertices to
      all other vertices. This student should report if all paths
      exist, and if not, where a path does not exist. \xmark{2}
      
    \item If we begin on vertex 1, the initial state distribution is:
      \begin{align*}
        \vec{p}_0 = [~\rinline{paste(format(begin.dist), collapse="~")}~]
      \end{align*}
      After one random step the state distribution is:
      \begin{align*}
        \vec{p}_1 = T\vec{p}_0 = [~\rinline{paste(format(T %*% begin.dist), collapse="~")}~]
      \end{align*}
      After two random steps the state distribution is:
      \begin{align*}
        \vec{p}_2 = T\vec{p}_1 = [~\rinline{paste(format(T %*% T %*% begin.dist), collapse="~")}~]
      \end{align*} \xmark{2}
    \item The probability of each vertex in the stationary
      distribution of an undirected graph is proportional to the
      number of edges connected to the vertex. Therefore the
      stationary distribution is:
      \begin{align*}
        \vec{p} = [~\rinline{paste(format(station), collapse="~")}~]
      \end{align*}
      We can verify that this is the stationary distribution by showing that $\vec{p} = T\vec{p}$
      \xmark{2}
      
    \end{enumerate}
    
    
  
  \end{workingbox}

]>)



m4_define(_graphs_q3,<[

  \squestion
  
  % begin.rcode echo=FALSE, results="hide", message=FALSE
  require("igraph")
  $@
  #g = graph.formula(A-B, A-C, A-D, B-D)
  d = degree(g)
  close = 1/closeness(g)
  n = length(d)
  V(g)$color = "white"
  V(g)$label.color = "black"
  E(g)$color = "black"
  names(d) = LETTERS[1:n]
  % end.rcode 

  The following graph shows ``Recommended Video'' links between the
  top five Youtube videos.

  \begin{center}
    % begin.rcode echo=FALSE,results="asis", fig=TRUE, fig.width=4, fig.height=4
    par(mar = c(0,0,0,0))
    plot(g, layout=layout.fruchterman.reingold, vertex.size = 35, vertex.label = LETTERS[1:n])
    % end.rcode 
  \end{center}

  Using this graph:
  \begin{enumerate}
  \item Construct the adjacency matrix.
  \item Tabulate the degree distribution and state if the graph is
    more similar to a Erdös-Renyi graph or Barabasi-Albert graph.
  \item Calculate the graph density.
  \item Calculate the closeness centrality for each vertex.
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
          table(factor(degree(g), c(0,1,2,3,4,5)))
          % end.rcode 
        \end{minipage}
        \xmark{1}
      \end{center}
      If the distribution is similar to Poisson, the graph is a
      Erdös-Renyi graph. If the distribution is similar to
      Exponential, the graph is a Barabasi-Albert graph.

    \item The graph density is \rinline{graph.density(g)}.
      
    \item The closeness centrality for each vertex is:
      \begin{center}
        \begin{tabular}{\rinline{paste(rep('c',n),collapse='')}}
          \rinline{paste(names(d),collapse=" & ")} \\
          \rinline{paste(close,collapse=" & ")}
        \end{tabular}
      \xmark{2}
      \end{center}

      
    \item The most central vertex is vertex \rinline{names(which(close == min(close)))}.
      \xmark{1}
      
    \end{enumerate}
    
  \end{workingbox}
]>)



m4_define(_clustering_q3,<[
  
  \squestion
  
  % begin.rcode echo=FALSE,results="hide", message=FALSE
  
  set.seed(1)
  $@

  rownames(X) = LETTERS[1:nrow(X)]
  d = dist(X)
  D = as.matrix(d)
  D.missing = D
  z = D[missing[1],missing[2]]
  D.missing[missing[1],missing[2]] = NA
  D.missing[missing[2],missing[1]] = NA
  h = hclust(d, method="complete")
  min.dist = function(D) {
    a = dim(D)[1]
    row = which.min(apply(D + diag(rep(10,a)), 2, min))
    col = apply(D + diag(rep(10,a)), 2, which.min)[row]
    return(c(row,col))
  }
  
  merge.rows = function(D, pos) {
    a = D[pos[2],,drop=FALSE]
    A = D[-pos[2],,drop=FALSE]
    A[pos[1],] = apply(rbind(A[pos[1],,drop=FALSE],a),2,max)
    return(A)
  }
  
  merge.row.col = function(D, pos) {
   pos = sort(pos)
   Dt = merge.rows(D,pos)
   E = merge.rows(t(Dt),pos)
   mname = paste(colnames(D)[pos[1]],"-",colnames(D)[pos[2]], sep="")
   colnames(E)[pos[1]] = mname
   rownames(E)[pos[1]] = mname
   return(E)
  }
  % end.rcode 

  
  While attempting to cluster a set of user profile data, our laptop
  battery discharged, leaving us with an incomplete distance matrix.
  
  The user profile data from $\rinline{nrow(X)}$ users is given below:
  % begin.rcode echo=FALSE,results="verbatim"
  print(X)
  % end.rcode 
  The incomplete distance matrix is:
  % begin.rcode echo=FALSE,results="verbatim"
  print(D.missing)
  % end.rcode 
  \begin{enumerate}
  \item Which metric was used to compute the distances?
  \item Compute the missing distance (marked ``NA'').
  \item Calculate the three distance matrices, showing the distance
    between 3 clusters, 2 clusters and 1 cluster, using complete
    linkage clustering.
  \item Plot the dendrogram of the hierarchical clustering.
  \end{enumerate}
  
  \begin{workingbox}
    
    \begin{enumerate}
    \item Euclidean distance was used.
    \item The missing distance is \rinline{z}.
    \item The set of distance matrices are:
    % begin.rcode echo=FALSE,results="verbatim"
    E = D
    p = min.dist(E)
    E = merge.row.col(E, p)
    print("Three clusters")
    print(E)
    p = min.dist(E)
    E = merge.row.col(E, p)
    print("Two clusters")
    print(E)
    p = min.dist(E)
    E = merge.row.col(E, p)
    print("One cluster")
    print(E)
    % end.rcode 
    Note that the value of the numbers along the diagonal of each
    matrix are not important, since we do not compare clusters to themselves.
    \end{enumerate}
    \item The dendrogram:
    \begin{center}
    % begin.rcode echo=FALSE,results="asis", fig=TRUE, fig.width=3, fig.height=3
    plot(h)
    % end.rcode 
    Take note of the clusters formed and the height of the vertical lines.
    \end{center}
      

  
  \end{workingbox}
]>)




m4_define(_sentiment_q3,<[
  
  \squestion
  
  % begin.rcode echo=FALSE,results="hide", message=FALSE
  
  $@

  D = as.matrix(dist(X))
  l = rep("negative",n)
  l[positive] = "positive"
  names(l) = c(1:n)
  classify = c(5,6,7)
  l.removed = l
  l.removed[classify] = "-"
  classified = (1:n)[-classify]
  
  knn = function(x, D, l, k) {
    names(which.max(table(l[as.numeric(names(sort(D[x,])[1:k]))])))
  }

  k1.class = sapply(5:7, knn, D[,classified], l, k1)
  k2.class = sapply(5:7, knn, D[,classified], l, k2)
  
  eval = function(k.class, pn.class) {
    mean(k.class[which(l[classify] == pn.class)] == pn.class)
  }
  k1p = eval(k1.class,"positive")
  k2p = eval(k2.class,"positive")
  k1n = eval(k1.class,"negative")
  k2n = eval(k2.class,"negative")
  
  % end.rcode 

  The text in a set of \rinline{nrow(X)} Web pages was compared using a distance metric
  and the following distance table was obtained.
  % begin.rcode echo=FALSE,results="verbatim"
  print(D)
  % end.rcode 
  A user study provided the sentiment class for the following Web pages:
  % begin.rcode echo=FALSE,results="verbatim"
  print(l.removed[-classify])
  % end.rcode 
  \begin{enumerate}
  \item Use the $k$ nearest neighbour classifier
    to classify the sentiment of Web pages that are
    missing classification (Web pages \rinline{classify}). Provide two
    classifications for each Web page, the first where
    $k = \rinline{k1}$ and the second where $k = \rinline{k2}$.
  \item Given that the true class of Web pages \rinline{classify} is 
    \rinline{l[classify]}), plot the Receiver operating characteristic
    for $k = \rinline{k1}$ and $k = \rinline{k2}$. Make sure to
    clearly label the plot.
  \item From these results, which of using $k = \rinline{k1}$ or $k =
    \rinline{k2}$ is more similar to random guessing? Explain your reasoning.
  \end{enumerate}
  
  \begin{workingbox}
    
    \begin{enumerate}
    \item The classification of pages \rinline{classify} for $k =
      \rinline{k1}$ is \rinline{k1.class}         \xmark{2}
    \item The classification of pages \rinline{classify} for $k =
      \rinline{k2}$ is \rinline{k2.class}        \xmark{2}
    \item To plot the Receiver operating characteristic, we first
      compute the sensitivity and specificity.
      \begin{itemize}
      \item For $k \rinline{k1}$: Sensitivity =
        \rinline{k1p}, Specificity = \rinline{k1n}
      \item For $k \rinline{k2}$: Sensitivity =
        \rinline{k2p}, Specificity = \rinline{k2n}
      \end{itemize} \xmark{2}
      \begin{center}
        % begin.rcode echo=FALSE,results="asis", fig=TRUE, fig.width=4, fig.height=4
        x = c(0,1)
        #par(mgp = c(1.5, 0.4, 0), mar=c(2.5,2.5,0.1,0.1), cex=0.8)
        plot(1-k1n,k1p, type="p", xlim=x, ylim=x, xlab="1 - Specificity", ylab = "Sensitivity", pch=1)
        points(1-k2n,k2p, pch=2)
        legend("bottomright", c(paste("k =", k1), paste("k =", k2)), pch=1:2)
        % end.rcode 
      \end{center}\xmark{1}
      \item The method that is closest to the diagonal is more similar
        to random guessing. \xmark{1}
    \end{enumerate}
    
  \end{workingbox}
]>)


m4_define(_link_analysis_q3,<[

  %%%% Mean, standard deviation, mode, median, quantiles

  \squestion
  
  % begin.rcode echo=FALSE,results="hide"
  require("igraph")
  require("xtable", quietly=TRUE)

  $@

  
  V(g)$color = "white"
  V(g)$label.color = "black"
  E(g)$color = "black"
  alpha = 0.8
  A = t(as.matrix(get.adjacency(g)))
  T = A %*% diag(1/apply(A,2,sum))
  N = dim(A)[1]
  J = matrix(rep(1/N, N*N), N, N)
  R = alpha*T + (1-alpha)*J
  e = eigen(R)
  pos = which(abs(e$values - 1) < 1e-8)
  ud = degree(as.undirected(g))
  station = ud/sum(ud)
  begin.dist = rep(0,N)
  begin.dist[1] = 1
  
  % end.rcode 
  

  The following directed graph shows the links between the top
  \rinline{N} contributors to Stack Overflow.
  \begin{center}
    % begin.rcode echo=FALSE,results="asis", fig=TRUE, fig.width=4, fig.height=4
    par(mar = c(0,0,0,0))
    plot(g, layout=layout.fruchterman.reingold, vertex.size = 35, vertex.label = LETTERS[1:N])
    % end.rcode 
  \end{center}
  
  \begin{enumerate}
  \item Contruct the probability transition matrix.
  \item State if the graph is ergodic and why or why not.
  \item If we ignore the direction of the edges, we obtain an
    undirected graph. Compute the stationary distribution of this
    undirected graph.
  \item Is the stationary distribution for the undirected graph the
    same as the stationary distribution for the directed graph? Give
    evidence for your answer.
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
        \xmark{2}
      \end{center}
    \item The graph is ergodic if there is a path from all vertices to
      all other vertices. This student should report if all paths
      exist, and if not, where a path does not exist. \xmark{2}
      
    \item The probability of each vertex in the stationary
      distribution of an undirected graph is proportional to the
      number of edges connected to the vertex. 
      The degree of each vertex is:
      \begin{align*}
        \vec{p} = [~\rinline{paste(format(ud), collapse="~")}~]
      \end{align*}
      Therefore the
      stationary distribution is:
      \begin{align*}
        \vec{p} = [~\rinline{paste(format(station), collapse="~")}~]
      \end{align*}
      \xmark{2}
    \item The stationary distribution of the directed graph satisfies 
      $\vec{p} = T\vec{p}$. By multiplying the transition matrix and
      the undirected stationary distribution, we obtain:
      % begin.rcode echo=FALSE,results="verbatim"
      print(T %*% station)
      % end.rcode 
      If this is similar to the undirected stationary distribution,
      then the undirected stationary distribution is also the
      stationary distribution for the directed graph. Otherwise it is
      not.
      \xmark{2}
    \end{enumerate}
    
    
  
  \end{workingbox}

]>)




m4_define(_text_index_q2,<[

  %%%% construct text index, weight and query
  
  \squestion
  
  % begin.rcode echo=FALSE,results="hide", message=FALSE
  $@


  rownames(A) = paste("D",1:nrow(A), sep="")
  colnames(A) = paste("T",1:ncol(A), sep="")
  
  IDF = log(nrow(A)/apply(A > 0,2,sum))
  TF = log(A + 1)
  TF.IDF = TF %*% diag(IDF)


  qv = as.numeric(colnames(A) %in% query)
  d.norm = sqrt(apply(TF.IDF^2,1,sum))
  q.norm = sqrt(sum(qv^2))
  ds = (TF.IDF %*% qv)/(d.norm * q.norm)
  
  % end.rcode 

  A set of similar tweets (\rinline{rownames(A)} containing the terms
  \rinline{colnames(A)}) has been preprocessed and converted into the
  following term frequency index:
  % begin.rcode echo=FALSE,results="verbatim"
  print(A)
  % end.rcode 

    
  \begin{enumerate}

  \item Provide one reason for and one reason against using stop
    word when preprocessing text for search engine.
    
  \item Compute the TF-IDF weight of each item in the term frequency index.
    
  \item Use cosine similarity to compute the set of document scores
    for the query containing the terms ``\rinline{query}'', and rank
    the documents by their relevance to the query.
    
  \item Cosine similarity consists of an inner product of the document
    and query vectors, divided by the norm of the document and query
    vectors. Is it necessary to divide by the document
    vector norm and the query vector norm? Explain your reasoning.

  \end{enumerate}
  
  \begin{workingbox}
    \begin{enumerate}
    \item Reason for: removing words make the search process more
      efficient. Reason against: some stop words may be important
      query terms, depending on the text collection.       \xmark{2}
    \item The TF weights are:
      % begin.rcode echo=FALSE,results="verbatim"
      print(TF)
      % end.rcode 
      The IDF weights are:
      % begin.rcode echo=FALSE,results="verbatim"
      print(IDF)
      % end.rcode 
      giving the weighted term frequencies:
      % begin.rcode echo=FALSE,results="verbatim"
      print(TF.IDF)
      % end.rcode 
      \xmark{2}
    \item The norm of each document vector is:
      % begin.rcode echo=FALSE,results="verbatim"
      print(d.norm)
      % end.rcode 
      The norm of the query vector is:
      % begin.rcode echo=FALSE,results="verbatim"
      print(q.norm)
      % end.rcode 
      Therefore the documents scores are:
      % begin.rcode echo=FALSE,results="verbatim"
      print(ds)
      % end.rcode 
      giving the document ranking \rinline{rownames(A)[rank(-ds)]}.
      \xmark{2}
    \item Dividing by the document norm removes the effect of the
      document length (meaning a document wont get a high score just
      because it is long) and so is required. Dividing by the query
      norm does the same for the query and so is constant for all
      document scores, but since we are interested in the rank and not
      the scores, dividing by the query norm has no effect on the rank,
      and so is not needed.
      \xmark{2}
    \end{enumerate}
    
    
  \end{workingbox}
]>)



m4_define(_SimpleExposure_q2,<[
\squestion

% begin.rcode echo=FALSE, results="hide", message=FALSE

$@


set.seed(SEED)

X = rbind(
t(rmultinom(1, 38, pA)),
t(rmultinom(1, 25, pB)),
t(rmultinom(1, 2, pC)))


page = paste("Page", LETTERS[1:3])
dimnames(X) = list(continent, page)
Y = cbind(X, Total = rowSums(X))
Y = rbind(Y, Total=colSums(Y))
Z = X[c(1,2),]
Z[2,] = Z[2,] + X[3,]
rownames(Z)[2] = paste(rownames(X)[2],rownames(X)[3], sep=" - ")

E = (rowSums(Z) %o% colSums(Z))/sum(Z)

x = sum((Z - E)^2/E)
df = (nrow(Z) - 1)*(ncol(Z) - 1)
% end.rcode



Omnibiz provide information using five different languages on all of
their Web pages. Data was gathered to identify if there was a
relationship between a customers city of origin and the first
page they have ``liked''.
\begin{center}
  % begin.rcode echo=FALSE,results="verbatim"
  print(Y)
  % end.rcode
\end{center}

\begin{enumerate}
\item Identify one problem with using a $\chi^2$ (Chi-squared) test
  with this data.
\item Transform the table so that it is appropriate for a $\chi^2$ test.
\item Find expected counts for each entry in the reduced table
  assuming city of origin and Web page are independent.
\item Calculate a $\chi^2$ statistic for testing whether the city of origin
  varies by Web page, and state its degrees of freedom.
\item Does the test statistic provide evidence that the page likes are
  related to the city of origin? Explain your answer.
\end{enumerate}

  \begin{workingbox}
    \begin{enumerate}
    \item The $\chi^2$ test requires that all cells are 5 or
      more. This table contain cells with values less than 5. \xmark{1}
    \item To transform the data, we merge the last two rows:
      % begin.rcode echo=FALSE,results="verbatim"
      print(Z)
      % end.rcode 
      \xmark{2}
    \item The expected counts are:
      % begin.rcode echo=FALSE,results="verbatim"
      print(E)
      % end.rcode 
      \xmark{2}
    \item The test statistic is $\chi^2 = \rinline{x}$ with df = \rinline{df}.
      \xmark{2}
    \item If the test statistic small, there is no evidence of a
      relationship between continent and first like. \xmark{1}
    \end{enumerate}
  \end{workingbox}


]>)



m4_define(_BACI_q2,<[
\squestion

Youtube are planning on inserting commercial breaks in popular videos
that run for more than 10 minutes, but are concerned about the effect
it will have on its viewers. To gauge the effect of adding commercial
breaks, the number of views of the video ``Cats falling'' was recorded for
three days before and three day after inserting commercials. The number of
views was also collected on the same days for the video ``Cats
smiling'', which had no commercials inserted.  The data are in the
left table below. The right table contains the sums of the square
roots of the data.

\begin{center}
% begin.rcode echo=FALSE, results="asis"
require(xtable, quietly=TRUE)

$@

X = factor(rep(c("Cats falling","Cats smiling"), 2*c(n,n)))
Z = factor(rep(rep(c("Before","After"), c(n,n)),2), levels=c("Before","After"))

set.seed(SEED)
Y = rpois(4*n, rep(mu, rep(n,4)))

tab=tapply(Y, list(X,Z), FUN=paste, collapse=",")
print(xtable(tab, align="|l|c|c|"), floating=FALSE, hline.after=c(-1,0,1,2))

cat("\\hspace{1cm}")
xtab = xtabs(sqrt(Y)~X+Z)
print(xtable(xtab, align="|l|r|r|", digits=2), floating=FALSE, hline.after=c(-1,0,1,2))
RSS = sum(aov(sqrt(Y)~X*Z)$residuals^2)
% end.rcode
\end{center}

\begin{enumerate}
\item Explain why using a square root transformation is advisable for
  count data.
\item Calculate the \emph{contrast} for the interaction between
  the views of the video ``Cats falling'' and time.
\item Calculate the \emph{Sum of Squares} for the interaction between
   the views of the video ``Cats falling'' and time.
 \item Given that the sum of squares for error is
   \rinline{round(RSS,3)}, find the $F$-statistic for the interaction
   between the views of the video ``Cats falling'' and time, and state
   its degrees of freedom.
\end{enumerate}


]>)





m4_define(_Trend_q2,<[
\squestion


Facebook want to dynamically allocate bandwidth to provide users with
a guaranteed quality of service, therefore they need to examine how
usage fluctuates through the day. The counts of user access over three
successive days are given below, grouped into three successive time periods.

\begin{center}
% begin.rcode echo=FALSE, results="asis"
s = c(50,70,40)
trend = seq(30, 60, length=9)

$@
#set.seed(56235)
set.seed(SEED)
xx = rpois(length(trend), trend + s)
tmp = matrix(xx, ncol=3, byrow=TRUE)
dimnames(tmp) = list(paste("Day",1:3), paste("Period", 1:3))
print(xtable(tmp), floating=FALSE)
% end.rcode
\end{center}

To examine the fluctuation for each day, we break the data into its
trend and periodic components, after a square root transformation. The
tables below shows the estimated moving average trend, and periodic
components.

\begin{center}
{\bf Trend}\\[1ex]
% begin.rcode echo=FALSE, results="asis"
d = decompose(ts(sqrt(xx), freq=3))
et = as.numeric(d$trend)
etc = formatC(et, format="f", digits=2)
etc[etc==" NA"] = ""
tmp = matrix(etc, ncol=3, byrow=TRUE)
dimnames(tmp) = list(paste("Day",1:3), paste("Period", 1:3))

tmp[2,2] = missing.symbol
print(xtable(tmp), floating=FALSE, sanitize.text.function=function(x) x)
% end.rcode

{\bf Periodic}\\[1ex]
% begin.rcode echo=FALSE, results="asis"
ss = d$figure
ssc = formatC(ss, format="f", digits=3)
tmp = matrix(ssc, ncol=3, byrow=TRUE)
dimnames(tmp) = list("Periodic", paste("Period", 1:3))

tmp[1,2] = missing.symbol
print(xtable(tmp), floating=FALSE, sanitize.text.function=function(x) x)
% end.rcode
\end{center}

\begin{enumerate}
\item Compute the missing trend component marked with a \rinline{missing.symbol}.
\item Compute the missing periodic component marked with a \rinline{missing.symbol}.
\item Which period would you recommend allocating the most network
  bandwidth to? Explain your answer.
\end{enumerate}
]>)
