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
  \item Construct the probability transition matrix.
  \item State if the graph is ergodic and why or why not.
  \item Construct the random surfer probability transition matrix using $\alpha = \rinline{alpha}$.
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

  tests = strsplit(tolower(test),split=" ")[[1]]
  pos = which(all.terms %in% tests)
  npos = which(!(all.terms %in% tests))

  p.tables = lapply(lapply(strsplit(tolower(positive), split=" "), factor, all.terms), table)
  p = apply(do.call(rbind, p.tables) > 0, 2, sum)

  n.tables = lapply(lapply(strsplit(tolower(negative), split=" "), factor, all.terms), table)
  n = apply(do.call(rbind, n.tables) > 0, 2, sum)

  nall.terms = all.terms
  nall.terms[npos] = paste("~", all.terms[npos], sep="")

  #p = table(factor(strsplit(tolower(paste(positive, collapse=" ")),split=" ")[[1]],all.terms))
  #n = table(factor(strsplit(tolower(paste(negative, collapse=" ")),split=" ")[[1]],all.terms))
  p.N = rep(length(p.tables), length(p))
  n.N = rep(length(n.tables), length(n))
  p.p = p/p.N
  n.p = n/n.N

  names(p.p) = nall.terms
  names(n.p) = nall.terms
  p.p[npos] = 1 - p.p[npos]
  n.p[npos] = 1 - n.p[npos]
  
  freqs = rbind(p,n)
  rownames(freqs) = c("Positive","Negative")
  probs = rbind(p.p,n.p)
  rownames(probs) = c("Positive","Negative")

  # rule of succession
  adjust = which((p.p == 0) | (p.p == 1))
  p[adjust] = p[adjust] + 1
  p.N[adjust] = p.N[adjust] + 2

  adjust = which((n.p == 0) | (n.p == 1))
  n[adjust] = n[adjust] + 1
  n.N[adjust] = n.N[adjust] + 2

  p.p = p/p.N
  n.p = n/n.N

  names(p.p) = nall.terms
  names(n.p) = nall.terms
  p.p[npos] = 1 - p.p[npos]
  n.p[npos] = 1 - n.p[npos]

  sprobs = rbind(p.p,n.p)
  rownames(sprobs) = c("Positive","Negative")


  
  
  P.d.p = prod(p.p[pos])
  P.d.n = prod(n.p[pos])
  P.p = length(positive)/(length(positive) + length(negative))
  P.n = length(negative)/(length(positive) + length(negative))
  
  llr = log(P.p/P.n) + sum(log(p.p/n.p))
  tweet.class = "positive"
  if (llr < 0) tweet.class = "negative"
  

  
  
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
    
    The number of tweets containing each word is:
    % begin.rcode echo=FALSE,results="verbatim"
    print(freqs)
    % end.rcode 
    
    The probability of each word appearing in a tweet given its sentiment (positive or negative) is:
    \begin{center}
    \begin{minipage}{0.9\textwidth}
    % begin.rcode echo=FALSE,results="verbatim"
    print(probs, digits=2)
    % end.rcode 
    \end{minipage}
    ~\xmark{2}
    \end{center}
    
    We adjust the probability of 0 and 1 using the Rule of Succession:
    \begin{center}
    \begin{minipage}{0.9\textwidth}
    % begin.rcode echo=FALSE,results="verbatim"
    print(sprobs, digits=2)
    % end.rcode 
    \end{minipage}
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
      &= \log{\frac{\rinline{P.p}}{\rinline{P.n}}} + \rinline{format(sum(log(p.p/n.p)))} \\
      &= \rinline{format(log(P.p/P.n) + sum(log(p.p/n.p)))}  \xmark{1}
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

  \begin{workingbox}
    \begin{enumerate}[a.]
    \item The $\chi^2$ test requires that all cells have a frequency of at
      least 5. For this table, two cells have counts less than 5. (1 mark)

    \item The reduced table it computed by summing the merged columns to obtain:

      % begin.rcode echo=FALSE,results="verbatim", message=FALSE    
      ages = c("13--24","25--34","35--44","45+")
      y = x[,c(2,3,4,5)]
      y[,1] = apply(x[,c(1,2)],1,sum)
      y[,4] = apply(x[,c(5,6,7)],1,sum)
      print(y)
      % end.rcode

    \item The expected values are computed as $e_{i,j} = {\sum r_{i}c_{j}/\text{total}}$, giving:
      % begin.rcode echo=FALSE,results="verbatim", message=FALSE    
      gtotal = apply(y,1,sum)
      atotal = apply(y,2,sum)
      ttotal = sum(y)
      e = gtotal %o% atotal/ttotal
      chisq = sum((y - e)^2/e)
      print(e)
      % end.rcode

    \item The $\chi^2$ test statistic is computed as $\chi^2 = {\sum_{i,j} (o_{i,j} - e_{i,j})^2/e_{i,j}}$
      with degrees of freedom $(r-1)(c-1) = 3$, giving the value $\chi^2$:
      \rinline{chisq}
    \end{enumerate}
  \end{workingbox}
  
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
BC = xtab[2,1]
AC = xtab[2,2]
BI = xtab[1,1]
AI = xtab[1,2]
contrast = (BC+AI) - (AC+BI)
n = 3
SSI = contrast^2/(4*n)
SSE = RSS
MSE = SSE/(4 * (n-1))
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

\begin{workingbox}
\begin{enumerate}
\item Count data would have a variance proportional (equal to) the mean. ie non-constant variance.
The square root transformation is variance stabilising. \xmark{1}
\item The contrast for the interaction between company and time is given as $(BC+AI) - (AC+BI)$ = 
\rinline{contrast}.\xmark{2}
\item The sum of squares for the interaction is \rinline{SSI}, where $n = \rinline{n}$. \xmark{2}
\item The $F$-statistic is SSI/MSE = \rinline{SSI/MSE} on 1 and \rinline{4*(n-1)} degrees of freedom. \xmark{3}
\end{enumerate}
\end{workingbox} 


]>)

% GS
m4_define(_Trend_q1,<[
\squestion

The jTele mobile phone company is looking at the tweet count
containing their hash-tag, in the three days after the release of a
new model. The counts are aggregated into four time periods; midnight
to 6am, 6am to noon, noon to 6pm and 6pm to midnight.  The data are
below.

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

part1 = tmp[1,4]
tmp[1,4] = missing.symbol
print(xtable(tmp), floating=FALSE, sanitize.text.function=function(x) x)
% end.rcode

{\bf Periodic}\\[1ex]
% begin.rcode echo=FALSE, results="asis"
ss = d$figure
ssc = formatC(ss, format="f", digits=3)
tmp = matrix(ssc, ncol=4, byrow=TRUE)
dimnames(tmp) = list("Periodic", paste("Period", 1:4))

part2 = tmp[1,4]
tmp[1,4] = missing.symbol
print(xtable(tmp), floating=FALSE, sanitize.text.function=function(x) x)
% end.rcode
\end{center}

\begin{enumerate}
\item Compute the missing trend component marked with a \rinline{missing.symbol}.
\item Compute the missing periodic component marked with a \rinline{missing.symbol}.
\end{enumerate}

\begin{workingbox}
\begin{enumerate}
\item The missing trend component is a moving average with window size 4, therefore we use the window (0.5, 1,1,1,0.5), giving the average \rinline{part1}.
\item The periodic components have a mean of zero, meaning their sum is 0. Therefore the missing component is \rinline{part2}.
\end{enumerate}
\end{workingbox}


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
xans = tab[3, c(1,4)]
tab[3,c(1,4)] = missing.symbol
print(xtable(tab), floating=FALSE, sanitize.text.function=function(x) x)
% end.rcode
Compute the missing entries marked \rinline{missing.symbol}.

  \begin{workingbox}
    Missing entries are \rinline{xans[1]} and \rinline{xans[2]} respectively.
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

\begin{workingbox}
  Tweet 1 contains 6 words, tweet 2 contains 6 words, 2 words are shared, giving (4 + 4)/(4 + 2 + 4) = 0.8
\end{workingbox}


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
  \item Construct the probability transition matrix.
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
  top \rinline{n} Youtube videos.

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
  \item Using complete linkage clustering, calculate the three
    distance matrices, showing the distance between 3 clusters, 2
    clusters and 1 cluster.
  \item Sketch the dendrogram of the hierarchical clustering.
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
  #rownames(D) = paste("Page", 1:n)
  #colnames(D) = paste("Page", 1:n)
  
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
    classifications for each unclassified Web page, the first where
    $k = \rinline{k1}$ and the second where $k = \rinline{k2}$.
  \item Given that the true class of Web pages \rinline{classify} is 
    \rinline{l[classify]}, plot the Receiver operating characteristic
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
  \item Construct the probability transition matrix.
  \item State if the graph is ergodic and describe why or why not.
  \item If we ignore the direction of the edges, we obtain an
    undirected graph. Compute the stationary distribution of this
    undirected graph.
  \item Is the stationary distribution for the undirected form of the graph the
    same as the stationary distribution for the directed form of the graph? Give
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
    word removal when preprocessing text for a search engine.
    
  \item Compute the TF-IDF weight of each item in the term frequency index.
    
  \item Use cosine similarity to compute the set of document scores
    for the query containing the terms ``\rinline{query}'', and rank
    the documents by their relevance to the query.
    
  \item Cosine similarity consists of three parts: 1) an inner product
    of the document and query vectors, 2) division by the norm of the
    document vector, and 3) division by the norm of the query vector.
    For ranking documents, are the last two parts of Cosine similarity
    necessary?  Explain your reasoning.

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
      giving the document ranking
      \rinline{rownames(A)[rank(-ds, ties.method = c("first"))]}.
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
AI = xtab[1,2]
BI = xtab[1,1]
AC = xtab[2,2]
BC = xtab[2,1]
contrast = (BC+AI) - (AC+BI)
SSi = contrast^2/(4*n)
MSE = RSS/(4*(n-1))
F = SSi/MSE
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


\begin{workingbox}
\begin{enumerate}
\item Count data would have a variance proportional (equal to) the
  mean. ie non-constant variance.  The square root transformation is
  variance stabilising. \xmark{2}
\item The contrast for the interaction between company and time is
  given as $(BC+AI) - (AC+BI)$ = (\rinline{BC} + \rinline{AI}) -
  (\rinline{AC} + \rinline{BI}) = \rinline{contrast}
\item The sum of squares for the interaction is $SS_{\text{int}} =
  \text{contrast}^2/4n = \rinline{SSi}$. \xmark{4}
\item The $F$-statistic is $SS_{\text{int}}/MS_{E} =
  \rinline{SSi/MSE}$ on 1 and \rinline{4*(n-1)} degrees of freedom,
  where $MS_E = SS_E/(4(n-1))$. \xmark{2}
\end{enumerate}
\end{workingbox} 


]>)





m4_define(_Trend_q2,<[
\squestion


Facebook want to dynamically allocate bandwidth to provide users with
a guaranteed quality of service, therefore they need to examine how
usage fluctuates through the day. The counts of user access over three
successive days are given below, grouped into three successive time periods.

\begin{center}
% begin.rcode echo=FALSE, results="asis"

$@
#set.seed(56235)
require("xtable")
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
d = stats::decompose(ts(sqrt(xx), freq=3))
et = as.numeric(d$trend)
etc = formatC(et, format="f", digits=2)
etc[etc==" NA"] = ""
tmp = matrix(etc, ncol=3, byrow=TRUE)
dimnames(tmp) = list(paste("Day",1:3), paste("Period", 1:3))

tmp[2,2] = missing.symbol
adat = round(sqrt(xx)[4:6],3)
print(xtable(tmp), floating=FALSE, sanitize.text.function=function(x) x)
% end.rcode

{\bf Periodic}\\[1ex]
% begin.rcode echo=FALSE, results="asis"
ss = d$figure
ssc = formatC(ss, format="f", digits=3)
tmp = matrix(ssc, ncol=3, byrow=TRUE)
dimnames(tmp) = list("Periodic", paste("Period", 1:3))

tmp[1,2] = missing.symbol
ansS = -sum(as.numeric(tmp[1,-2]))
print(xtable(tmp), floating=FALSE, sanitize.text.function=function(x) x)
% end.rcode
\end{center}

\begin{enumerate}
\item Compute the missing trend component marked with a \rinline{missing.symbol}.
\item Compute the missing periodic component marked with a \rinline{missing.symbol}.
\item Which period would you recommend allocating the most network
  bandwidth to? Explain your answer.
\end{enumerate}

\begin{workingbox}
\begin{enumerate}
\item The trend is a 3-point moving average on the sqrt scale, so is
\begin{center}
\rinline{paste("(", paste(adat, collapse="+"), ")/3", sep="")} $=$ \rinline{sum(adat)/3}
\end{center}
\xmark{4}

\item The seasonal component should sum to zero so the missing component is \rinline{ansS}
\xmark{2}

\item Period two has the largest periodic component, so it should be
  allocated the most bandwidth. \xmark{2}
\end{enumerate}
\end{workingbox}

]>)



%------------------------------------------------------------------

m4_define(_graphs_q4,<[

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

  The top \rinline{n} technology companies have following Twitter relationships.

  \begin{center}
    % begin.rcode echo=FALSE,results="asis", fig=TRUE, fig.width=4, fig.height=4
    par(mar = c(0,0,0,0))
    plot(g, layout=layout.fruchterman.reingold, vertex.size = 35, vertex.label = LETTERS[1:n])
    % end.rcode 
  \end{center}

  Using this graph:
  \begin{enumerate}
  \item Construct the adjacency list.
  \item Tabulate the degree distribution and state if the graph is
    more similar to a Erdös-Renyi graph or Barabasi-Albert graph.
  \item Calculate the graph diameter.
  \item Identify which vertex has the greatest degree centrality and explain why.
  \end{enumerate}
  
  \begin{workingbox}
    \marknote{For each of these four questions, give 1 mark for the correct
      working and 1 mark for the right answer.}
    
    \begin{enumerate}
    \item The adjacency list is:
      \begin{center}
        \begin{minipage}{0.5\textwidth}
          % begin.rcode echo=FALSE,results="verbatim", message=FALSE
          print(as.matrix(get.edgelist(g)))
          % end.rcode 
        \end{minipage}
        \xmark{2}
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
      Exponential, the graph is a Barabasi-Albert graph. \xmark{1}

    \item The diameter is the longest shortest path. For this graph, the diameter is 
      \rinline{diameter(g)}. \xmark{2}
      
    \item The degree for each vertex is:
      \begin{center}
        \begin{tabular}{\rinline{paste(rep('c',n),collapse='')}}
          \rinline{paste(names(d),collapse=" & ")} \\
          \rinline{paste(d,collapse=" & ")}
        \end{tabular}
      \end{center}
        The vertex with greatest degree has the greatest degree centrality score.
        The most central vertex is vertex \rinline{names(which(close == min(close)))}.
        \xmark{2}
      
    \end{enumerate}
    
  \end{workingbox}
]>)



m4_define(_clustering_q4,<[
  
  \squestion
  
  % begin.rcode echo=FALSE,results="hide", message=FALSE
  
  set.seed(1)
  $@

  rownames(X) = LETTERS[1:nrow(X)]
  d = dist(X, method = "binary")
  D = as.matrix(d)
  D.missing = D
  z = D[missing[1],missing[2]]
  D.missing[missing[1],missing[2]] = NA
  D.missing[missing[2],missing[1]] = NA
  h = hclust(d, method="single")
  min.dist = function(D) {
    a = dim(D)[1]
    row = which.min(apply(D + diag(rep(10,a)), 2, min))
    col = apply(D + diag(rep(10,a)), 2, which.min)[row]
    return(c(row,col))
  }
  
  merge.rows = function(D, pos) {
    a = D[pos[2],,drop=FALSE]
    A = D[-pos[2],,drop=FALSE]
    A[pos[1],] = apply(rbind(A[pos[1],,drop=FALSE],a),2,min)
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

  The Death Star was destroyed before Grand Moff Tarkin could cluster
  the locations of the rebel bases. Unfortunately Darth Vader has
  found you on his way to meet the Emperor and wants you to complete
  the analysis on the way.
    
  The location of $\rinline{nrow(X)}$ rebel bases 
  (\rinline{paste(LETTERS[1:nrow(X)], collapse=", ")})
  are given below:
  % begin.rcode echo=FALSE,results="verbatim"
  print(X)
  % end.rcode 
  The incomplete distance matrix produced by the Death Star is:
  % begin.rcode echo=FALSE,results="verbatim"
  print(D.missing)
  % end.rcode 
  \begin{enumerate}
  \item Which metric was used to compute the distances?
  \item Compute the missing distance (marked ``NA'').
  \item Using single linkage clustering, calculate the three
    distance matrices, showing the distance between 3 clusters, 2
    clusters and 1 cluster.
  \item Sketch the dendrogram of the hierarchical clustering.
  \end{enumerate}
  
  \begin{workingbox}
    
    \begin{enumerate}
    \item Binary distance was used. \xmark{2}
    \item The missing distance is \rinline{z}. \xmark{2}
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
    \end{enumerate} \xmark{2}
    \item The dendrogram:
    \begin{center}
    % begin.rcode echo=FALSE,results="asis", fig=TRUE, fig.width=3, fig.height=3
    plot(h)
    % end.rcode 
    Take note of the clusters formed and the height of the vertical lines. \xmark{2}
    \end{center}
      

  
  \end{workingbox}
]>)


m4_define(_link_analysis_q4,<[

  %%%% Mean, standard deviation, mode, median, quantiles

  \squestion
  
  % begin.rcode echo=FALSE,results="hide"
  require("igraph")
  suppressWarnings(require("xtable", quietly=TRUE))

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
  

  The following graph shows the information flow between 
  \rinline{N} major cities.
  \begin{center}
    % begin.rcode echo=FALSE,results="asis", fig=TRUE, fig.width=4, fig.height=4
    par(mar = c(0,0,0,0))
    plot(g, layout=layout.fruchterman.reingold, vertex.size = 30, vertex.label = LETTERS[1:N])
    % end.rcode 
  \end{center}
  
  \begin{enumerate}
    \item Compute the stationary distribution of the graph.
    \item Explain why the graph is ergodic and what change must be
      made to make it non-ergodic.
  \item Construct the probability transition matrix of your new non-ergodic graph.
  \item Show that the stationary distribution found above is not the stationary distribution of the new non-ergodic graph.
  \end{enumerate}
  
  \begin{workingbox}

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

    \item The graph is ergodic since there is a path from each vertex
      to every other vertex. To make the graph non-ergodic, we must
      change one of the edges (add direction, or remove), so that
      there is not a path between at lest one pair of vertices. \xmark{2}
        
    \begin{enumerate}
    \item The probability transition matrix of the original graph is:
      \begin{center}
        \begin{minipage}{0.5\textwidth}
          % begin.rcode echo=FALSE,results="verbatim"
          print(T)
          % end.rcode 
        \end{minipage}
      \end{center}
      This must be adjusted to reflect the change in in the graph. \xmark{2}
      
    \item The student must show that $\vec{p} \ne T\vec{p}$ \xmark{2}
      
    \end{enumerate}
    
    
  
  \end{workingbox}

]>)




m4_define(_sentiment_q4,<[
  
  \squestion
  
  % begin.rcode echo=FALSE,results="hide", message=FALSE
  
  $@

  D = as.matrix(dist(X))
  M = cmdscale(D)
  #rownames(D) = paste("Page", 1:n)
  #colnames(D) = paste("Page", 1:n)
  
  l = rep("negative",n)
  l[positive] = "positive"
  names(l) = c(1:n)
  classify = c(5,6,7,8)
  l.removed = l
  l.removed[classify] = "-"
  classified = (1:n)[-classify]
  
  knn = function(x, D, l, k) {
    names(which.max(table(l[as.numeric(names(sort(D[x,])[1:k]))])))
  }

  k1.class = sapply(classify, knn, D[,classified], l, k1)
  k2.class = sapply(classify, knn, D[,classified], l, k2)
  
  eval = function(k.class, pn.class) {
    mean(k.class[which(l[classify] == pn.class)] == pn.class)
  }
  k1p = eval(k1.class,"positive")
  k2p = eval(k2.class,"positive")
  k1n = eval(k1.class,"negative")
  k2n = eval(k2.class,"negative")
  
  % end.rcode 

  The histograms of a set of \rinline{nrow(X)} tweeted images were compared
  using a distance metric and the following distance table was
  obtained.
  % begin.rcode echo=FALSE,results="verbatim"
  print(D)
  % end.rcode 
  with the following Multi-dimensional Scaling projection:
  % begin.rcode echo=FALSE,results="verbatim"
  print(M)
  % end.rcode 
  A user study provided the sentiment class for a subset of the images:
  % begin.rcode echo=FALSE,results="verbatim"
  print(l.removed[-classify])
  % end.rcode 
  \begin{enumerate}
  \item Use the $k$ nearest neighbour classifier to classify the
    sentiment of images that are missing classification (images
    \rinline{classify}), using $k = \rinline{k1}$. 
  \item Use the $k$ nearest neighbour classifier to classify the
    sentiment of images that are missing classification (images
    \rinline{classify}), using $k = \rinline{k2}$. 
  \item Given that the true class of images \rinline{classify} is 
    \rinline{l[classify]}, plot the Receiver operating characteristic
    for $k = \rinline{k1}$ and $k = \rinline{k2}$. Make sure to
    clearly label the plot.
  \item From these results, which of using $k = \rinline{k1}$ or
    $k = \rinline{k2}$ would you use in practice (if any)? Which is
    more similar to random guessing? Explain your reasoning.
  \end{enumerate}
  
  \begin{workingbox}
    
    \begin{enumerate}
    \item The classification of images \rinline{classify} for $k =
      \rinline{k1}$ is \rinline{k1.class}         \xmark{2}
    \item The classification of images \rinline{classify} for $k =
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
    \item We would choose the method on the upper left of the digonal
      (if there is no method, choose none). The method that is closest
      to the diagonal is more similar to random guessing. \xmark{1}
    \end{enumerate}
    
  \end{workingbox}
]>)



m4_define(_text_index_q3,<[

  %%%% construct text index, weight and query
  
  \squestion
  
  % begin.rcode echo=FALSE,results="hide", message=FALSE
  $@


  rownames(A) = paste("D",1:nrow(A), sep="")
  colnames(A) = paste("T",1:ncol(A), sep="")
  
  IDF = log(nrow(A)/apply(A > 0,2,sum))
  TF = log(A + 1)
  TF.IDF = TF %*% diag(IDF)

  #expected.probability = c(0.1, 0.2, 0.7)
  p = dbinom(A[1,], size = sum(A[1,]), prob = expected.probability)
  w = -log(p)

  d.norm = sqrt(sum(w^2))
  qv = as.numeric(colnames(A) %in% query)
  q.norm = sqrt(sum(qv^2))
  ds = (w %*% qv)/(d.norm * q.norm)

  % end.rcode 

  A set of similar tweets (\rinline{rownames(A)} containing the terms
  \rinline{colnames(A)}) has been preprocessed and converted into the
  following term frequency index:
  % begin.rcode echo=FALSE,results="verbatim"
  print(A)
  % end.rcode 

    
  \begin{enumerate}

  \item Provide one reason for and one reason against using stop
    word removal when preprocessing text for a search engine.
    
  \item Compute the Divergence from Randomness weight (using the
    Binomial Distribution) of each term in the first document, given that the expected proportion of each term is \rinline{paste(paste(colnames(A), ":", sep=""), expected.probability, collapse=", ")}.
    
  \item Use cosine similarity to compute the document score for document 1
    using the query containing the terms ``\rinline{query}''.
    
  \item A colleague tells you to include each word five times in the
    query to get better results (e.g. search for
    \rinline{paste(rep(query, each=5), collapse = " ")}). Explain why
    this is a good or bad idea.

  \end{enumerate}
  
  \begin{workingbox}
    \begin{enumerate}
    \item Reason for: removing words make the search process more
      efficient. Reason against: some stop words may be important
      query terms, depending on the text collection.       \xmark{2}
    \item The Binomial probabilities of the first document are
      \rinline{p}. The weights are \rinline{w}.
      \xmark{2}
    \item The norm of the weighted document vector is \rinline{d.norm}.
      The norm of the query vector \rinline{q.norm}.
      Therefore the documents score is \rinline{ds}.
      \xmark{2}
    \item Increasing the number of query terms only changes the query
      vector. Since the query vector is normalised, multiplying the
      query vector by five will not make a difference to the document
      score.  \xmark{2}
    \end{enumerate}
    
    
  \end{workingbox}
]>)


m4_define(_Trend_q3,<[
\squestion

Supreme Chancellor Valorum wants the assistance of the Jedi Knights to
break the blockade of Naboo, and has decided to examine their
popularity in the community. The table below shows the square root of
the number of tweets for each month containing the word Jedi.

\begin{center}
% begin.rcode echo=FALSE, results="asis"

$@
#set.seed(56235)
require("xtable")
set.seed(SEED)
y = sqrt(rpois(length(trend), trend))
MONTHS = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
n = length(y)
names(y) = MONTHS[1:n]
x = 1:n
mod = lm(y ~ x)
yhat = predict(mod)
ssx = sum((x - mean(x))^2)
ssxy = sum((x - mean(x))*(y - mean(y)))
ssy = sum((y - mean(y))^2)
b = ssxy/ssx
RSS = sum((y - yhat)^2)
s = sqrt(RSS/(n-2))
t = (b)/(s/sqrt(ssx))
ytab = as.data.frame(t(y))
rownames(ytab) = c("Tweet Count")
print(xtable(ytab), floating=FALSE)
% end.rcode
\end{center}

Our task is to fit the simple linear regression $y = a + bx$, where
$y$ is the square root of the number of tweets in the month containing
the word Jedi, and $x$ is the month as a number (Jan = 1, Feb = 2,
\ldots, Dec = 12). We have found that $SS_{x} = \rinline{ssx}$, 
$SS_{xy} = \rinline{ssxy}$, and $SS_{y} = \rinline{ssy}$.

\begin{enumerate}
\item Compute the sample gradient $b$ of the of the simple linear regression.
\item Compute the residual sum of squares ($RSS$), given that the fitted square root tweet counts are $\hat{y} = \rinline{paste(format(yhat), collapse=", ")}$.
\item Compute the $t$ test statistic, testing the Null Hypothesis that $\beta$, the gradient of the model is zero.
\item What can we conclude from the above $t$ test statistic in terms of tweets about Jedis?
\end{enumerate}

\begin{workingbox}
\begin{enumerate}
\item The sample gradient $b = SS_{xy}/SS_x = \rinline{b}$ \xmark{2}
\item $RSS = {\sum {(y_i - \hat{y}_i)}^2} = \rinline{RSS}$ \xmark{2}
\item $t = (b - 0)/(s/\sqrt{SS_x})$, $s = \sqrt{RSS/(n-2)} = \rinline{s}$, $t = \rinline{t}$ \xmark{2}
\item If the magnitude of the $t$ value is small, then we have no evidence of a change in tweet counts about Jedis as the months increase. If the $t$ value is large, and $b > 0$, then the tweets counts are increasing over the months. If $b < 0$ then the tweet counts are decreasing.
  \xmark{2}
\end{enumerate}
\end{workingbox}

]>)



m4_define(_BACI_q3,<[
\squestion

The Jedi council are planning on providing all Jedis with double ended
lightsabers, but are concerned that the public may mistake Jedis
carrying these special lightsaber weapons as evil. To examine public
sentiment, the number of tweets containing the words ``evil jedi'' were
counted for three days before and after changing the weapons. As a
control the number of tweets containing the words ``jedi council'' were
counted before and after the changeover.

Below is the square root of the tweet counts:
\begin{center}
% begin.rcode echo=FALSE, results="asis"
require(xtable, quietly=TRUE)

$@

X = factor(rep(c("Evil Jedi","Jedi Council"), 2*c(n,n)))
Z = factor(rep(rep(c("Before","After"), c(n,n)),2), levels=c("Before","After"))

set.seed(SEED)
Y = rpois(4*n, rep(mu, rep(n,4)))

tab=tapply(format(sqrt(Y)), list(X,Z), FUN=paste, collapse=", ")
print(xtable(tab, align="|l|c|c|"), floating=FALSE, hline.after=c(-1,0,1,2))

cat("\\hspace{1cm}")
xtab = xtabs(sqrt(Y)~X+Z)
#print(xtable(xtab, align="|l|r|r|", digits=2), floating=FALSE, hline.after=c(-1,0,1,2))
a = aov(sqrt(Y)~X*Z)
RSS = sum(a$residuals^2)
AI = xtab[1,2]
BI = xtab[1,1]
AC = xtab[2,2]
BC = xtab[2,1]
contrast = (BC+AI) - (AC+BI)
effect = contrast/(2*n)

SSi = contrast^2/(4*n)
MSE = RSS/(4*(n-1))
F = SSi/MSE
SSba = as.data.frame(anova(a))["Z","Sum Sq"]
SSci = as.data.frame(anova(a))["X","Sum Sq"]
SSint = as.data.frame(anova(a))["X:Z","Sum Sq"]
SStot = sum((sqrt(Y) - mean(sqrt(Y)))^2)
% end.rcode
\end{center}



\begin{enumerate}
\item Calculate the \emph{effect} for the interaction between the two
  sets of tweets and time.
\item What does the value of the interaction effect tell us about the
  tweet counts?
\item Given that $SS_{Total} = \rinline{SStot}$, $SS_{BA} = \rinline{SSba}$ and $SS_{CI} = \rinline{SSci}$, find the $F$-statistic for the interaction
  between the square root of the tweet count and time, and state
  its degrees of freedom.
\item List two problems with the data collection process.
\end{enumerate}


\begin{workingbox}
\begin{enumerate}
\item The contrast for the interaction between tweet count and time
  given as $(BC+AI) - (AC+BI)$ = (\rinline{BC} + \rinline{AI}) -
  (\rinline{AC} + \rinline{BI}) = \rinline{contrast}. The effect is $contrast/(2n) = \rinline{effect}$ \xmark{2}
\item The interaction effect $\gamma$ is the sample mean increase of
  the square root tweet count containing "Evil Jedi" after changing to
  double ended lightsabers. \xmark{2}
\item The $F$-statistic is $SS_{\text{int}}/MS_{E} =
  \rinline{SSi/MSE}$ on 1 and \rinline{4*(n-1)} degrees of freedom,
  where $SS_{\text{int}} = \text{contrast}_{int}^2/4n = \rinline{SSint}$, $SS_E = SS_{Total} - SS_{BA} - SS_{CI} - SS_{Int} = \rinline{RSS}$, $MS_E = SS_E/(4(n-1)) = \rinline{MSE}$. \xmark{2}
\item The tweet count of tweets containing "evil jedi" may not be
  effected by lightsaber use or may be effected by other factors. The
  tweet count of tweets containing "jedi council" may be effected by
  lightsaber use and so may not be useful as a control. \xmark{2}
\end{enumerate}
\end{workingbox} 


]>)




m4_define(_SimpleExposure_q3,<[
\squestion

% begin.rcode echo=FALSE, results="hide", message=FALSE

$@


set.seed(SEED)

X = rbind(
t(rmultinom(1, 38, pA)),
t(rmultinom(1, 25, pB)),
t(rmultinom(1, 25, pC)))


page = paste("Page", LETTERS[1:3])
page = c("Reference", "No Reference")
dimnames(X) = list(continent, page)
Y = cbind(X, Total = rowSums(X))
Y = rbind(Y, Total=colSums(Y))
Z = X
E = (rowSums(Z) %o% colSums(Z))/sum(Z)

x = sum((Z - E)^2/E)
df = (nrow(Z) - 1)*(ncol(Z) - 1)
% end.rcode


TweetMe have obtained the count of tweets containing references to their company from three different countries.
\begin{center}
  % begin.rcode echo=FALSE,results="verbatim"
  print(Y)
  % end.rcode
\end{center}

\begin{enumerate}
\item Identify any problems with using a $\chi^2$ (Chi-squared) test
  with this data and transform the data to avoid them.
\item Find expected counts for each entry in the table
  assuming city of origin and Web page are independent.
\item Calculate a $\chi^2$ statistic for testing whether a tweets reference to TweetMe is independent of the country of origin, and state its degrees of freedom.
\item State the Null and Alternative hypotheses of the test. Provide a
  conclusion to the test based on your $\chi^2$ statistic.
\end{enumerate}

  \begin{workingbox}
    \begin{enumerate}
    \item The $\chi^2$ test requires that all cells are 5 or
      more. All cells are at least 5, so no transform is needed. \xmark{2}
    \item The expected counts are:
      % begin.rcode echo=FALSE,results="verbatim"
      print(E)
      % end.rcode 
      \xmark{2}
    \item The test statistic is $\chi^2 = \rinline{x}$ with df = \rinline{df}.
      \xmark{2}
    \item The Null Hypothesis is that reference to TweetMe is
      independent of country. The Alternative is that there is an
      assication to country. If the test statistic small, there is no
      evidence of a relationship. If the test statistic is large, then
      there is an association. \xmark{2}
    \end{enumerate}
  \end{workingbox}


]>)




m4_define(_SimpleExposure_q4,<[
\squestion

% begin.rcode echo=FALSE, results="hide", message=FALSE

$@


set.seed(SEED)

X = rbind(
t(rmultinom(1, 100, pA)),
t(rmultinom(1, 80, pB)),
t(rmultinom(1, 70, pC)),
t(rmultinom(1, 50, pD)))


age = c("17-25", "26-35","36-45","46-55")
nation = c("Australian", "Not Australian")
rownames(X) = age
colnames(X) = nation
X = t(X)
df = (nrow(X) - 1)*(ncol(X) - 1)

rchisq = rchisq(1000, df = df)
% end.rcode

FunkyFones want to determine if the age of people interested in
working for them is dependent on their nationality. They obtained the following data, containing the number of visits from people of a given nationality and age group,  from their LinkedIn page.
\begin{center}
  % begin.rcode echo=FALSE,results="verbatim"
  print(X)
  % end.rcode
\end{center}

\begin{enumerate}
\item State the Null and alternative hypothesis of the test that needs to be conducted.

\item State which test statistic should be used for the test and provide its equation.

\item Describe the randomisation process to obtain the distribution of
  the test statistic.
  
\item Given the following randomisation distribution and sample test
  statistic value of \rinline{chisqs}, what is the conclusion of the
  test?

\begin{center}
  % begin.rcode echo=FALSE,results="asis", fig=TRUE, fig.width=4, fig.height=4
  hist(rchisq, main = "Randomisation distribution", xlab = "Randomised statistic")
  % end.rcode
\end{center}

\end{enumerate}

  \begin{workingbox}

\begin{enumerate}
\item $H_0$: Age and Nationality are independent. \xmark{1}
  $H_A$: They are not independent. \xmark{1}

\item The $\chi^2$ statistic should be used to compare the sample to the expected independent values. \xmark{2}
  
\item The randomisation distribution shows the distribution of $\chi^2$
  when age and nationality are independent. If we shuffle the ages,
  construct the table, and compute the $\chi^2$ value, we obtain one
  random $\chi^2$ when $H_0$ is true. By repeating this many times we
  obtain the randomisation distribution. \xmark{2}
  
\item If the the test statistic could be sampled from the randomisation distribution, then we can't reject $H_0$. Otherwise, we reject $H_0$. \xmark{2}


\end{enumerate}
    
    
  \end{workingbox}


]>)


m4_define(_text_index_q4,<[

  %%%% construct text index, weight and query
  
  \squestion
  
  % begin.rcode echo=FALSE,results="hide", message=FALSE
  $@


  s = sapply(d,strsplit," ")
  terms = unique(unlist(s))
  ftable = function(x) { table(factor(x, levels = terms)) }
  A = t(sapply(s, ftable))
  rownames(A) = c("D1","D2","D3")
  
  IDF = log(nrow(A)/apply(A > 0,2,sum))
  TF = log(A + 1)
  TF.IDF = TF %*% diag(IDF)


  qs = strsplit(query ," ")
  qt = ftable(qs[[1]])

  qw = log(qt + 1)*IDF
  
  ip = TF.IDF[1,] %*% qw
  qwn = sqrt(sum(qw^2))
  dwn = sqrt(sum(TF.IDF[1,]^2))
  
  s1 = ip/(qwn*dwn)

  % end.rcode 

  Tweets have started to appear from unknown sources, using an alien
  language. The three most recent tweets are:
  \begin{itemize}
  \item \rinline{d[[1]]}
  \item \rinline{d[[2]]}
  \item \rinline{d[[3]]}
  \end{itemize}

    
  \begin{enumerate}

  \item Should we perform stop word removal and/or stemming on these three tweets?
    
  \item Construct the document term frequency matrix.
   
  \item Compute the TF-IDF weights for the first tweet.

  \item Given the query ``\rinline{query}'', compute the document score for the first tweet,
    using the Cosine similarity between the TF-IDF weighted first tweet
    and TF-IDF weighted query.

  \end{enumerate}
  
  \begin{workingbox}
    \begin{enumerate}
    \item Stop words and stemming is specific to the language. Since
      we are unfamiliar with the alien language, we should not remove
      stop words or stem. \xmark{2}
      
    \item   
      % begin.rcode echo=FALSE,results="verbatim"
      print(A)
      % end.rcode 
      \xmark{2}

    \item IDF = [\rinline{IDF}], TF = [\rinline{TF[1,]}], TF-IDF = [\rinline{TF.IDF[1,]}] \xmark{2}

    \item The weighted query vector is [\rinline{qw}]. The Cosine similarity is $qd_1/(\|q\|\|d\|)$.
      $qd_1 = \rinline{ip}$, $\|q\| = \rinline{qwn}$, $\|d_1\| = \rinline{dwn}$. So the document score for $d_1$ is \rinline{s1}. \xmark{2}
    \end{enumerate}
    
    
  \end{workingbox}
]>)



m4_define(_graphs_q5,<[

  \squestion
  
  % begin.rcode echo=FALSE, results="hide", message=FALSE
  require("igraph")
  $@
  #g = graph.formula(A-B, A-C, A-D, B-D)
  A = as.matrix(get.adjacency(g))
  d = degree(g)
  close = 1/closeness(g)
  n = length(d)
  V(g)$color = "white"
  V(g)$label.color = "black"
  E(g)$color = "black"
  names(d) = LETTERS[1:n]
  close = 1/closeness(g)
  % end.rcode 

  The aliens have also transmitted the following matrix, that may represent a graph.
  % begin.rcode echo=FALSE,results="verbatim"
  A
  % end.rcode 

  Using this adjacency matrix:
  \begin{enumerate}
  \item Draw the graph that is represented by the adjacency matrix.
  \item Tabulate the degree distribution and state if the graph is
    more similar to a Erdös-Renyi graph or Barabasi-Albert graph.
  \item Compute the closeness centrality score for each vertex, and
    identify which vertex is the most central according to closeness
    centrality.
  \item After viewing the graph, a colleague has suggested that the
    graph may represent the constellation (pattern) of stars that the aliens
    live near. Why is this not likely to be true.
  \end{enumerate}
  
  \begin{workingbox}
    \marknote{For each of these four questions, give 1 mark for the correct
      working and 1 mark for the right answer.}
    
    \begin{enumerate}
    \item The graph is:
      \begin{center}
        % begin.rcode echo=FALSE,results="asis", fig=TRUE, fig.width=4, fig.height=4
        par(mar = c(0,0,0,0))
        plot(g, layout=layout.fruchterman.reingold, vertex.size = 35, vertex.label = LETTERS[1:n])
        % end.rcode 
      \end{center}
    \item The degree distribution is:
      \begin{center}
        \begin{minipage}{0.5\textwidth}
          % begin.rcode echo=FALSE,results="verbatim"
          table(factor(degree(g), c(0,1,2,3,4,5)))
          % end.rcode 
        \end{minipage}
        \xmark{2}
      \end{center}
      If the distribution is similar to Poisson, the graph is a
      Erdös-Renyi graph. If the distribution is similar to
      Exponential, the graph is a Barabasi-Albert graph. \xmark{2}

    \item The closeness centrality score for each vertex is [\rinline{close}]. The most central vertex is vertex \rinline{names(which(close == max(close)))} with a score of \rinline{max(close)}. \xmark{2}
      
    \item Star constellations have a specific pattern, allowing is to
      identify them. The graph we drew can be drawn in many different
      ways, as long and the edges are connected to the correct
      vertices. So even if the graph represented a constellation, it
      is not likely that we drew the constellation correctly. \xmark{2}
      
    \end{enumerate}
    
  \end{workingbox}
]>)


m4_define(_clustering_q5,<[
  
  \squestion
  
  % begin.rcode echo=FALSE,results="hide", message=FALSE
  
  set.seed(1)
  $@

  rownames(X) = LETTERS[1:nrow(X)]
  d = dist(X, method = "manhattan")
  D = as.matrix(d)
  D.missing = D
  z = D[missing[1],missing[2]]
  D.missing[missing[1],missing[2]] = NA
  D.missing[missing[2],missing[1]] = NA

  M = cmdscale(D,2)
  
  h = hclust(d, method="single")
  min.dist = function(D) {
    a = dim(D)[1]
    row = which.min(apply(D + diag(rep(10,a)), 2, min))
    col = apply(D + diag(rep(10,a)), 2, which.min)[row]
    return(c(row,col))
  }
  
  merge.rows = function(D, pos) {
    a = D[pos[2],,drop=FALSE]
    A = D[-pos[2],,drop=FALSE]
    A[pos[1],] = apply(rbind(A[pos[1],,drop=FALSE],a),2,min)
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

  A colleague has provided the following data containing four objects:
  % begin.rcode echo=FALSE,results="verbatim"
  print(X)
  % end.rcode 
  and the partially completed distance matrix of the four objects:
  % begin.rcode echo=FALSE,results="verbatim"
  print(D.missing)
  % end.rcode 
  \begin{enumerate}
  \item Which metric was used to compute the distances?
  \item Compute the missing distance (marked ``NA'').
  \item Your colleague wants to use k-means clustering on the four
    objects to obtain two clusters. Explain why k-means should not be
    applied directly the the four objects, and how to transform the
    data into an appropriate form for k-means clustering.
  \item After transforming the data, we have obtained the four points below:
    \begin{center}
      % begin.rcode echo=FALSE,results="asis", fig=TRUE, fig.width=3, fig.height=3
      plot(M)
      % end.rcode 
    \end{center}
    Your colleague wants to initialise the two k-means cluster
    centres to $c_1 = [\rinline{c1}]$ and $c_2 = [\rinline{c2}]$.
    Which objects do you predict will belong to each cluster $c_1$
    and $c_2$ after performing the k-means clustering?
  \end{enumerate}
  
  \begin{workingbox}
    
    \begin{enumerate}
    \item Manhattan distance. \xmark{2}
    \item The missing distance is \rinline{z}. \xmark{2}
    \item k-means clusters based on Euclidean distance, but the object
      distances are measured using Manhattan distance. We must
      transform the objects to a Euclidean distance space using
      Multi-Dimensional scaling before using k-means. \xmark{2}
    \item After one iteration all points belong to $c_1$ and none to
      $c_2$, therefore depending on the algorithm, $c_2$ will either
      disappear, or not move, so all points will belong to cluster
      $c_1$. Further iterations will not change the clusters. \xmark{2}
    \end{enumerate}
  
  \end{workingbox}
]>)





m4_define(_link_analysis_q5,<[

  %%%% Mean, standard deviation, mode, median, quantiles

  \squestion
  
  % begin.rcode echo=FALSE,results="hide"
  require("igraph")
  $@

  V(g)$color = "white"
  V(g)$label.color = "black"
  E(g)$color = "black"
  N = length(V(g))
  alpha = 0.8
  A = t(as.matrix(get.adjacency(g)))
  T = A %*% diag(1/apply(A,2,sum))
  p = degree(g, mode="out")
  p = p/sum(p)

  % end.rcode 
  

  Using the following directed graph:
  
  \begin{center}
    % begin.rcode echo=FALSE,results="asis", fig=TRUE, fig.width=4, fig.height=4
    par(mar = c(0,0,0,0))
    plot(g, layout=layout.fruchterman.reingold, vertex.size = 35, vertex.label = LETTERS[1:N])
    % end.rcode 
  \end{center}
  
  \begin{enumerate}
  \item Construct the probability transition matrix.
  \item State if the graph is ergodic and why or why not.
  \item Construct the random surfer probability transition matrix using $\lambda = 1$.
  \item Compute the stationary distribution.
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
      
    \item The random surfer probability transition matrix using $\lambda = 1$ is $\lambda T + (1 - \lambda)B = T$ when $\lambda = 1$. So the random surfer probability transition matrix is:
      
      \begin{center}
      \begin{minipage}{0.5\textwidth}
      % begin.rcode echo=FALSE,results="verbatim"
      print(T)
      % end.rcode 
      \end{minipage}
      \xmark{2}
      \end{center}
    \item We can see that even though the graph is directional, all
      edges are bidirectional, which is equivalent to
      undirectional. Therefore the stationary distribution is proportional
      to the degree of each vertex.
      % begin.rcode echo=FALSE,results="verbatim"
      print(p)
      % end.rcode 
      \xmark{2}
      
    \end{enumerate}
    
    
  
  \end{workingbox}
]>)




m4_define(_sentiment_q5,<[

  \squestion
  
  % begin.rcode echo=FALSE,results="hide",message=FALSE
  require("tm")
  $@
  

  tweetWords = strsplit(tweet, " ")[[1]]

  a = c("happy","calm","furious","mean")
  
  tweetTable = table(factor(tweetWords, levels = a))
  
  names(p) = a
  names(n) = a
  ## P(T|S)
  X = rbind(p,n)
  rownames(X) = c("Positive","Negative")
  ## P(S)
  ps = 0.6
  ## P(S|T) = P(T|S)P(S)/P(T)

  ## P(T) = sum(P(T|S)*P(S))

  
  pTS = diag(c(ps,1-ps)) %*% X
  pT = colSums(pTS)

  pTweet = prod(pT^tweetTable)
  pTweetP = prod(p^tweetTable)
  pTweetN = prod(n^tweetTable)
  
  pSPTweet = pTweetP*ps/pTweet
  pSNTweet = pTweetN*(1-ps)/pTweet
  
  llr = log(pSPTweet/pSNTweet)
  
  % end.rcode 

  
  The Angry Tweet Company has provided the following term probabilities given positive or negative sentiment (e.g. $P(\text{term = happy}\vert \text{sentiment = Pos}) = 0.1$).
  % begin.rcode echo=FALSE,results="verbatim"
  print(X)
  % end.rcode 
  We want to compute the sentiment of a chosen tweet containing the
  words ``furious happy!'' using a Naive Bayes Classifier.  Given that
  the probability of a randomly chosen tweet having Positive sentiment
  is $P(\text{sentiment = Pos}) = \rinline{ps}$:
  \begin{enumerate} 
  \item Compute the probability of the chosen tweet given that the tweet has
    positive sentiment $P(\text{tweet}\vert \text{sentiment = Pos})$,
    and the probability given that the tweet has negative \\ sentiment
    $P(\text{tweet}\vert \text{sentiment = Neg})$.
  \item Compute $P(\text{term})$ for all four terms, then compute the
    probability of the chosen tweet $P(\text{tweet})$.
  \item Compute the probability that the tweet has positive sentiment
    $P(\text{sentiment = Pos}\vert \text{tweet})$, and
    the probability that the tweet has negative sentiment
    $P(\text{sentiment = Neg}\vert \text{tweet})$.
  \item Compute the log likelihood ratio of the tweet's sentiment and
    predict the sentiment based on the result.
  \end{enumerate}
  
  \begin{workingbox}
  \begin{enumerate} 
  \item $P(\text{tweet}\vert \text{sentiment = Pos}) = {\prod_{\text{term in tweet}} P(\text{term}\vert \text{sentiment = Pos})}$ = \rinline{pTweetP}. \\
      $P(\text{tweet}\vert \text{sentiment = Neg}) = {\prod_{\text{term in tweet}} P(\text{term}\vert \text{sentiment = Neg})}$ = \rinline{pTweetN}. \xmark{2}

  \item $P(\text{term}) = {\sum_{\text{sentiment}} P(\text{term}\vert \text{sentiment})P(\text{sentiment})}$ \\
    The probabilities $P(\text{term}\vert \text{sentiment})P(\text{sentiment})$ are:
    % begin.rcode echo=FALSE,results="verbatim"
    print(pTS)
    % end.rcode 
    The final probabilities $P(\text{term})$ are:
    % begin.rcode echo=FALSE,results="verbatim"
    print(pT)
    % end.rcode 
    $P(\text{tweet}) = {\prod_{\text{terms in tweet}} P(\text{term})}$ = \rinline{pTweet}.\xmark{2}

    \item 
      $P(\text{sentiment = Pos}\vert \text{tweet}) = 
      P(\text{tweet}\vert \text{sentiment = Pos})P(\text{sentiment = Pos})/P(\text{tweet})$ = $\rinline{pTweetP}\times\rinline{ps}/\rinline{pTweet}$ = \rinline{pSPTweet}. \\
      $P(\text{sentiment = Neg}\vert \text{tweet}) = 
      P(\text{tweet}\vert \text{sentiment = Neg})P(\text{sentiment = Neg})/P(\text{tweet})$ = $\rinline{pTweetN}\times\rinline{1-ps}/\rinline{pTweet}$ = \rinline{pSNTweet}. \xmark{2}
    \item The log likelihood ratio is 
      $\log{(P(\text{sentiment = Pos}\vert \text{tweet})/P(\text{sentiment = Neg}\vert \text{tweet}))}$ = \rinline{llr}. If \rinline{llr} > 0, then the tweet has positive sentiment, otherwise it has negative sentiment. \xmark{2}
    \end{enumerate}
  
    
  \end{workingbox}
]>)




m4_define(_Trend_q5,<[
\squestion



Apple have recorded the number of tweets per season over three years,
containing the hashtag \#appleproblems, to gauge the trend in negative
sentiment towards the company.  They want to use this information to
work out which time of year they should begin their advertising
campaign.
\begin{center}
  % begin.rcode echo=FALSE,results="verbatim"
$@
#set.seed(56235)
#require("xtable")
set.seed(SEED)
xx = rpois(length(trend), trend + s)
tmp = matrix(xx, ncol=4, byrow=TRUE)
dnames = list(paste("Year",1:3), c("Summer","Autumn","Winter","Spring"))
dimnames(tmp) = dnames
#print(xtable(tmp), floating=FALSE)
print(tmp)
% end.rcode
\end{center}

The square root of the tweet counts are presented below split into
their estimated trend and periodic components.

\begin{center}
{\bf Trend}\\[1ex]
% begin.rcode echo=FALSE, results="verbatim"
d = stats::decompose(ts(sqrt(xx), freq=4))
et = as.numeric(d$trend)
etc = formatC(et, format="f", digits=2)
tmp = matrix(et, ncol=4, byrow=TRUE)
dimnames(tmp) = dnames

tmp[2,2] = NA #missing.symbol
adat = round(sqrt(xx)[4:8],3)
#print(xtable(tmp), floating=FALSE, sanitize.text.function=function(x) x)
print(tmp)
% end.rcode

{\bf Periodic}\\[1ex]
% begin.rcode echo=FALSE, results="verbatim"
ss = d$figure
#ssc = formatC(ss, format="f", digits=3)
tmp = matrix(ss, ncol=4, byrow=TRUE)
dimnames(tmp) = list("Periodic", dnames[[2]])

tmp[1,2] = NA #missing.symbol
ansS = -sum(as.numeric(tmp[1,-2]))
#print(xtable(tmp), floating=FALSE, sanitize.text.function=function(x) x)
print(tmp)
% end.rcode
\end{center}

% begin.rcode echo=FALSE, results="verbatim"
m = lm(et ~ I(1:length(et)))
grad = coef(m)[2]
% end.rcode


\begin{enumerate}
\item Compute the missing trend component for Autumn of Year 2.
\item Compute the missing periodic component for Autumn.
\item Which Season should Apple begin its advertising campaign and why?
\item It was found that the trend component can be closely modelled
  using a linear regression with gradient \rinline{grad}. Using this
  gradient, and the trend and periodic data, how many tweets do we
  predict contain "\#appleproblems" in the Winter of Year 3?
\end{enumerate}

\begin{workingbox}
\begin{enumerate}
\item The trend is a 4-point moving average on the sqrt scale, so is
\begin{center}
(\rinline{adat[1]}/2 + \rinline{paste(adat[2:4], collapse="+")} + \rinline{adat[5]}/2)/3 = \rinline{(adat %*% c(0.5,1,1,1,0.5))/4}
\end{center}
\xmark{2}

\item The seasonal component should sum to zero so the missing component is \rinline{ansS}
\xmark{2}

\item Autumn has the largest periodic component, meaning that month has the most tweets containing "\#appleproblems". Starting the advertising campaign in Autumn might be best to increase everyone's positive sentiment. \xmark{2}

\item Given that the gradient is \rinline{grad} and that the trend component of Autumn in Year 3 is \rinline{et[10]}, we predict that the trend for Winter in Year 3 is \rinline{et[10] + grad}. Adding in the periodic effect gives \rinline{et[10] + grad + ss[3]}. Finally, we square it to remove the square root transformation, giving \rinline{(et[10] + grad + ss[3])^2} expected tweets in Winter of Year 3. \xmark{2}
\end{enumerate}
\end{workingbox}

]>)
