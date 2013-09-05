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

