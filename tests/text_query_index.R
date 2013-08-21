

questions = list(
    list(
      type =  'NUM',
      known.parameters =  "p x n",
      question = 'The probability of finding the word "test" in a document collection is `r p`. Compute the weight of the word "test" in a document of length `r n`, where "test" appears `r x` times, given that using Divergence from Random weighting with the binomial distribution.',
      answer = '`r -log(dbinom(x,n,p))`',
      tolerance = "0.1"
    )
)




yaml.out(questions[[1]], "x = 3; n = 40; p = 0.1")
yaml.out(questions[[1]], "x = 5; n = 60; p = 0.2")




