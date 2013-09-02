

questions = list(
    list(
      type =  'NUM',
      known.parameters =  "p x n",
      question = 'The probability of finding the word "test" in a document collection is `r p`. Use Divergence from Random weighting with the binomial distribution to compute the weight of the word "test", in a document of length `r n`, where "test" appears `r x` times.',
      answer = '`r -log(dbinom(x,n,p))`',
      tolerance = "0.1"
    ),
    list(
      type =  'NUM',
      known.parameters =  "fdt ft N",
      question = 'Compute the TF-DF weight of the word "castle" in document d, given that the word appears in document d `r fdt` times, and the word "castle" appears in `r ft` of the `r N` documents in the collection.',
      answer = '`r log(fdt + 1)*log(N/ft)`',
      tolerance = "0.1"
    ),
    list(
      type =  'MC',
      known.parameters =  "q d",
      question = 'A query has been converted into the following vector `r paste(c("[", q, "]"), collapse=" ")`. Which of the following document vectors is most similar to the given query vector (using Cosine similarity)?',
      choices = list(
            '\"`r paste(c("[", d[1,], "]"), collapse=" ")`\"',
            '\"`r paste(c("[", d[2,], "]"), collapse=" ")`\"',
            '\"`r paste(c("[", d[3,], "]"), collapse=" ")`\"',
            '\"`r paste(c("[", d[4,], "]"), collapse=" ")`\"'),
      correct_answer = '`r L = apply(d, 1, function(x) sqrt(sum(x^2))); nd = diag(1/L) %*% d; which.max(nd %*% q)`'
    ),
    list(
      type =  'MC',
      known.parameters =  "",
      question = 'If stemming is applied to a document collection containing M unique words, the number of unique words in the result will be:',
      choices = list(
            'greater than M unique words',
            'greater than or equal to M unique words',
            'less than or equal to M unique words',
            'less than M unique words'),
      correct_answer = '3'
    ),
    list(
      type =  'MC',
      known.parameters =  "",
      question = 'If stop word removal is applied to a document collection containing M unique words, the number of unique words in the result will be:',
      choices = list(
            'greater than M unique words',
            'greater than or equal to M unique words',
            'less than or equal to M unique words',
            'less than M unique words'),
      correct_answer = '3'
    )

)



yaml.out(questions[[1]], "x = 3; n = 50; p = 0.1")
yaml.out(questions[[1]], "x = 1; n = 40; p = 0.2")
yaml.out(questions[[1]], "x = 2; n = 70; p = 0.1")
yaml.out(questions[[1]], "x = 4; n = 60; p = 0.2")

yaml.out(questions[[2]], "fdt = 3; N = 80; ft = 2")
yaml.out(questions[[2]], "fdt = 1; N = 60; ft = 4")
yaml.out(questions[[2]], "fdt = 4; N = 50; ft = 5")
yaml.out(questions[[2]], "fdt = 2; N = 70; ft = 3")

yaml.out(questions[[3]], "q = c(0,1,1,0,0); d = rbind(c(0,0,2,1,1),c(1,1,1,0,0),c(1,0,0,5,1),c(0,1,0,0,1))")
yaml.out(questions[[3]], "q = c(1,0,1,0,0); d = rbind(c(0,0,2,1,1),c(1,1,1,0,0),c(1,0,0,5,1),c(0,1,0,0,1))")
yaml.out(questions[[3]], "q = c(0,0,1,1,0); d = rbind(c(0,0,2,1,1),c(1,1,1,0,0),c(1,0,0,5,1),c(0,1,0,0,1))")
yaml.out(questions[[3]], "q = c(0,0,0,1,1); d = rbind(c(0,0,2,1,1),c(1,1,1,0,0),c(1,0,0,5,1),c(0,1,0,0,1))")

yaml.out(questions[[4]], "")
yaml.out(questions[[5]], "")



