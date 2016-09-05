

questions = list(
    list(
      type =  'NUM',
      known.parameters =  "x y",
      question = 'Compute the Euclidean distance between vectors [`r x`] and [`r y`].',
      answer = '`r sqrt(sum((x - y)^2))`',
      tolerance = "0.02"
    ),
    list(
      type =  'NUM',
      known.parameters =  "x y",
      question = 'Compute the Maximum distance between vectors [`r x`] and [`r y`].',
      answer = '`r max(abs(x - y))`',
      tolerance = "0.02"
    ),
    list(
      type =  'NUM',
      known.parameters =  "x y",
      question = 'Compute the Manhattan distance between vectors [`r x`] and [`r y`].',
      answer = '`r sum(abs(x - y))`',
      tolerance = "0.02"
    ),
    list(
      type =  'NUM',
      known.parameters =  "x y",
      question = 'Compute the Binary distance between vectors [`r x`] and [`r y`].',
      answer = '`r 1 - sum(x & y)/sum(x | y)`',
      tolerance = "0.02"
    )
)


yaml.out(questions[[1]], "x = c(1,0,3); y = c(0,1,1)")
yaml.out(questions[[2]], "x = c(1,0,3); y = c(0,1,1)")
yaml.out(questions[[3]], "x = c(1,0,3); y = c(0,1,1)")
yaml.out(questions[[4]], "x = c(1,0,3); y = c(0,1,1)")

yaml.out(questions[[1]], "x = c(2,1,0); y = c(8,5,0)")
yaml.out(questions[[2]], "x = c(2,1,0); y = c(8,5,0)")
yaml.out(questions[[3]], "x = c(2,1,0); y = c(8,5,0)")
yaml.out(questions[[4]], "x = c(2,1,0); y = c(8,5,0)")

yaml.out(questions[[1]], "x = c(1,1,1); y = c(3,4,1)")
yaml.out(questions[[2]], "x = c(1,1,1); y = c(3,4,1)")
yaml.out(questions[[3]], "x = c(1,1,1); y = c(3,4,1)")
yaml.out(questions[[4]], "x = c(1,1,1); y = c(3,4,1)")

yaml.out(questions[[1]], "x = c(0,0,3); y = c(4,1,1)")
yaml.out(questions[[2]], "x = c(0,0,3); y = c(4,1,1)")
yaml.out(questions[[3]], "x = c(0,0,3); y = c(4,1,1)")
yaml.out(questions[[4]], "x = c(0,0,3); y = c(4,1,1)")


