## Make sure image URL directory is not readable, only executable!

#cat('`r image_path = "graphs_intro"; image_url = "http://www.scem.uws.edu.au/~lapark/300958/images/graphs_intro"`')


questions = list(
    list(
      type =  'MC',
      known.parameters =  'D correct testStat alpha `r require("xtable");`',
      question = '<p>A hypothesis test for independence has produced a randomisation distribution with the quantiles in the below table. If the test statistic is `r testStat` and alpha = `r alpha`, what is the outcome of the test?</p> `r gsub("\\n","",c(print(xtable(D, digits=3), type="html", comment=FALSE)))`',
      choices = list(
        'Reject the Null Hypothesis',
        'Accept the Null Hypothesis',
        'Do not reject the Null Hypothesis',
        'Accept the Alternative Hypothesis'),
      correct_answer = '`r correct`'
    ),
    list(
      type =  'MC',
      known.parameters =  'lower upper',
      question = '<p>A 95% confidence interval for a proportion is (`r lower`, `r upper`). From this we can infer:</p>',
      choices = list(
          'The population proportion is `r (lower + upper)/2`',
          'The population proportion is equal to the sample proportion',
          'The population proportion is between `r lower` and `r upper`',
          'The population proportion is likely to be between `r lower` and `r upper`'
          ),
      correct_answer = '4'
    ),
    list(
      type =  'MC',
      known.parameters =  'determine correct',
      question = '<p>Using the Facebook data, if we want to determine `r determine`, which one of the following statements is true?</p>',
      choices = list(
        'A chi squared test for independence is appropriate.',
        'A chi squared test for preference is appropriate',
        'A confidence interval for a proportion is appropriate',
        'A chi squared test and confidence interval for a proportion are not appropriate.'),
      correct_answer = '`r correct`'
    )
  )

yaml.out(questions[[1]], 'D = data.frame(Quantile = c(0.01, 0.025, 0.5, 0.1, 0.9, 0.95, 0.975, 0.99), Value = c(5,6,7,8,21,22,23,24)); testStat = 23.5; alpha = 0.05; correct = 1')
yaml.out(questions[[1]], 'D = data.frame(Quantile = c(0.01, 0.025, 0.5, 0.1, 0.9, 0.95, 0.975, 0.99), Value = c(5,6,7,8,21,22,23,24)); testStat = 20.5; alpha = 0.05; correct = 3')
yaml.out(questions[[1]], 'D = data.frame(Quantile = c(0.01, 0.025, 0.5, 0.1, 0.9, 0.95, 0.975, 0.99), Value = c(5,6,7,8,21,22,23,24)); testStat = 6.5; alpha = 0.05; correct = 3')
yaml.out(questions[[1]], 'D = data.frame(Quantile = c(0.01, 0.025, 0.5, 0.1, 0.9, 0.95, 0.975, 0.99), Value = c(5,6,7,8,21,22,23,24)); testStat = 8.5; alpha = 0.05; correct = 3')
yaml.out(questions[[1]], 'D = data.frame(Quantile = c(0.01, 0.025, 0.5, 0.1, 0.9, 0.95, 0.975, 0.99), Value = c(30,35,40,50,90,95,100,110)); testStat = 92; alpha = 0.05; correct = 3')
yaml.out(questions[[1]], 'D = data.frame(Quantile = c(0.01, 0.025, 0.5, 0.1, 0.9, 0.95, 0.975, 0.99), Value = c(30,35,40,50,90,95,100,110)); testStat = 32; alpha = 0.05; correct = 1')
yaml.out(questions[[1]], 'D = data.frame(Quantile = c(0.01, 0.025, 0.5, 0.1, 0.9, 0.95, 0.975, 0.99), Value = c(30,35,40,50,90,95,100,110)); testStat = 102; alpha = 0.05; correct = 1')
yaml.out(questions[[1]], 'D = data.frame(Quantile = c(0.01, 0.025, 0.5, 0.1, 0.9, 0.95, 0.975, 0.99), Value = c(30,35,40,50,90,95,100,110)); testStat = 52; alpha = 0.05; correct = 3')

yaml.out(questions[[2]], 'lower = 0.23; upper = 0.34')
yaml.out(questions[[2]], 'lower = 0.11; upper = 0.97')
yaml.out(questions[[2]], 'lower = 0.45; upper = 0.47')
yaml.out(questions[[2]], 'lower = 0.75; upper = 0.89')
yaml.out(questions[[2]], 'lower = 0.51; upper = 0.66')
yaml.out(questions[[2]], 'lower = 0.62; upper = 0.88')
yaml.out(questions[[2]], 'lower = 0.37; upper = 0.48')
yaml.out(questions[[2]], 'lower = 0.03; upper = 0.05')


yaml.out(questions[[3]], 'determine = "if there is a dependence between the persons country of origin and their age"; correct = 1')
yaml.out(questions[[3]], 'determine = "the proportion of Males that have liked the page"; correct = 3')
yaml.out(questions[[3]], 'determine = "if the page is viewed by people of all ages, with no preference for a given age"; correct = 2')
yaml.out(questions[[3]], 'determine = "the number of likes our page will receive tomorrow"; correct = 4')









