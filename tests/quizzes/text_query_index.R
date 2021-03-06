

questions = list(
    list(
      type =  'NUM',
      known.parameters =  "p k n alpha",
      question = 'The probability P(t|d) for word t in document d is `r p`, and the probability P(t|C) for word t in the document collection C is `r k`. Given that the document is `r n` words long, compute the Dirichlet Smoothed probability of word t in document d using alpha = `r alpha`. Provide your answer using at least two decimal places.',
      answer = '`r lambda = n/(n + alpha); d = lambda*p + (1-lambda)*k; d`',
      tolerance = "0.02"
    ),
    list(
      type =  'NUM',
      known.parameters =  "fdt ft N",
      question = 'Compute the TF-IDF weight of the word "castle" in document d, given that the word "castle" appears in document d `r fdt` times, and the word "castle" appears in `r ft` of the `r N` documents in the collection.',
      answer = '`r log(fdt + 1)*log(N/ft)`',
      tolerance = "0.1"
    ),
    list(
      type =  'MC',
      known.parameters =  "q d",
      question = 'A query has been converted into the following vector `r paste(c("[", q, "]"), collapse=" ")`. Which of the following document vectors is most similar to the given query vector, using Cosine similarity? (Do not apply TF-IDF weights, use the given vector values).',
      choices = list(
            '`r paste(c("[", d[1,], "]"), collapse=" ")`',
            '`r paste(c("[", d[2,], "]"), collapse=" ")`',
            '`r paste(c("[", d[3,], "]"), collapse=" ")`',
            '`r paste(c("[", d[4,], "]"), collapse=" ")`'),
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
    ),
    list(
      type =  'MC',
      known.parameters =  "",
      question = 'The most likely stem of the word "writing" is:',
      choices = list(
            'writer',
            'write',
            'ing',
            'written'),
      correct_answer = '2'
    ),
    list(
      type =  'MC',
      known.parameters =  "",
      question = 'The most likely stem of the word "teacher" is:',
      choices = list(
            'teaching',
            'taught',
            'teach',
            'learn'),
      correct_answer = '3'
    ),
    list(
      type =  'MC',
      known.parameters =  "",
      question = 'The most likely stem of the word "bounce" is:',
      choices = list(
            'bounce',
            'bouncing',
            'bouncer',
            'boundary'),
      correct_answer = '1'
    ),
    list(
      type =  'MC',
      known.parameters =  "",
      question = 'Which of the following words is most likely to be a stop word:',
      choices = list(
            'early',
            'morning',        
            'after',
            'noon'),
      correct_answer = '3'
    ),
    list(
      type =  'MC',
      known.parameters =  "",
      question = 'Which of the following words is most likely to be a stop word:',
      choices = list(
            'jane',
            'is',        
            'extremely',
            'happy'),
      correct_answer = '2'
    ),
    list(
      type =  'MC',
      known.parameters =  "",
      question = 'Which of the following words is least likely to be a stop word:',
      choices = list(
            'and',
            'potato',        
            'this',
            'every'),
      correct_answer = '2'
    ),
    list(
      type =  'MC',
      known.parameters =  "",
      question = 'Which of the following words is least likely to be a stop word:',
      choices = list(
            'to',
            'be',        
            'a',
            'rat'),
      correct_answer = '4'
    )

)


yaml.out(questions[[1]], "p = 0.2; k = 0.1; n = 80; alpha = 10")
yaml.out(questions[[1]], "p = 0.2; k = 0.1; n = 70; alpha = 1")
yaml.out(questions[[1]], "p = 0.1; k = 0.2; n = 60; alpha = 5")
yaml.out(questions[[1]], "p = 0.1; k = 0.2; n = 50; alpha = 15")
yaml.out(questions[[1]], "p = 0.05; k = 0.2; n = 50; alpha = 1")
yaml.out(questions[[1]], "p = 0.05; k = 0.2; n = 60; alpha = 10")
yaml.out(questions[[1]], "p = 0.3; k = 0.1; n = 70; alpha = 5")
yaml.out(questions[[1]], "p = 0.3; k = 0.1; n = 80; alpha = 15")

yaml.out(questions[[2]], "fdt = 3; N = 90; ft = 2")
yaml.out(questions[[2]], "fdt = 1; N = 50; ft = 4")
yaml.out(questions[[2]], "fdt = 4; N = 60; ft = 5")
yaml.out(questions[[2]], "fdt = 2; N = 80; ft = 3")
yaml.out(questions[[2]], "fdt = 3; N = 80; ft = 6")
yaml.out(questions[[2]], "fdt = 1; N = 60; ft = 7")
yaml.out(questions[[2]], "fdt = 4; N = 50; ft = 8")
yaml.out(questions[[2]], "fdt = 2; N = 70; ft = 9")

yaml.out(questions[[3]], "q = c(0,1,1,0,0); d = rbind(c(0,0,2,1,1),c(1,1,1,0,0),c(1,0,0,5,1),c(0,1,0,0,1))")
yaml.out(questions[[3]], "q = c(1,0,1,0,0); d = rbind(c(0,0,2,1,1),c(1,1,1,0,0),c(1,0,0,5,1),c(0,1,0,0,1))")
yaml.out(questions[[3]], "q = c(0,0,1,1,0); d = rbind(c(0,0,2,1,1),c(1,1,1,0,0),c(1,0,0,5,1),c(0,1,0,0,1))")
yaml.out(questions[[3]], "q = c(0,0,0,1,1); d = rbind(c(0,0,2,1,1),c(1,1,1,0,0),c(1,0,0,5,1),c(0,1,0,0,1))")
yaml.out(questions[[3]], "q = c(1,1,0,0,0); d = rbind(c(0,0,2,1,1),c(1,1,1,0,0),c(1,0,0,5,1),c(0,1,0,0,1))")
yaml.out(questions[[3]], "q = c(1,0,0,1,0); d = rbind(c(0,0,2,1,1),c(1,1,1,0,0),c(1,0,0,5,1),c(0,1,0,0,1))")
yaml.out(questions[[3]], "q = c(0,0,0,0,1); d = rbind(c(0,0,2,1,1),c(1,1,1,0,0),c(1,0,0,5,1),c(0,1,0,0,1))")
yaml.out(questions[[3]], "q = c(0,0,1,0,1); d = rbind(c(0,0,2,1,1),c(1,1,1,0,0),c(1,0,0,5,1),c(0,1,0,0,1))")

yaml.out(questions[[4]])
yaml.out(questions[[5]])
yaml.out(questions[[6]])
yaml.out(questions[[7]])
yaml.out(questions[[8]])
yaml.out(questions[[9]])
yaml.out(questions[[10]])
yaml.out(questions[[11]])
yaml.out(questions[[12]])


