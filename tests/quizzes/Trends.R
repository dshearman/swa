questions = list(
  list(
    type =  'NUM',
    known.parameters =  'X Y',
    question = '<p>Compute the simplyfied Twitter chi-squared statistic (without the second term) given that the tweets in the before period are `r X` and in the current period `r Y`</p>',
    answer = '`r (Y-X)^2/X`',
    tolerance = "0.05"
  ),
  list(
    type =  'NUM',
    known.parameters =  'SSX SSXY RSS n',
    question = '<p>When conducting a linear regression, on the square root of `r n` twitter counts, the following summary statistics were obtained. SSX = `r SSX`, SSXY = `r SSXY` and RSS = `r RSS`. Calculate the t-statistic for the test of the slope (&beta;) being zero.</p>',
    answer = '`r (SSXY/SSX)/sqrt(RSS/((n-2)*SSX))`',
    tolerance = "0.05"
  ),
  list(
    type =  'NUM',
    known.parameters =  'y',
    question = '<p>It is beleived that there is a seasonal cycle of period 4 in a set of count data. The following
    observations are the square roots of part of the data. Calculate the 4 point moving average for the 
    third observation.</p> <p>`r y`</p>',
    answer = '`r (0.5*(y[1]+y[5])+sum(y[2:4]))/4`',
    tolerance = "0.05"
  ),
  list(
    type =  'NUM',
    known.parameters =  's',
    question = '<p>It is beleived that there is a seasonal cycle of period 4 in a set of count data. The following
    are three of the seasonal components. Calculate the fourth component.</p> <p>`r s`</p>',
    answer = '`r  -sum(s)`',
    tolerance = "0.05"
      ),
  list(
    type =  'NUM',
    known.parameters =  'a b `r suppressWarnings(require("xtable")); day = 1:30; count = a +b*day + rnorm(30, mean = 0, sd = 10); m = lm(count ~ day)`',
    question = '<p>We have used Simple Linear Regression to test if a linear trend exists in our data of day vs tweet count. Given the following output from the R function lm, what is the p value when testing the Null hypothesis that the gradient is zero, versus the Alternative hypothesis that the gradient is not zero? Provide your answer to two decimal places.</p> `r gsub("\\n","",c(print(xtable(summary(m)$coefficients, digits = 4), type="html", comment=FALSE)))`',
    answer = '`r round((summary(m))$coefficients[2,4], digits = 3)`',
    tolerance = "0.01"
      )    
)


set.seed(1726)
for(i in 1:16) yaml.out(questions[[1]], 'X=rpois(1,357); Y=rpois(1,422)')

set.one = function(n) {
  x = runif(n)
  y = 1.7*x + rnorm(length(x))
  SSX = sum((x-mean(x))^2)
  SSXY = sum((x-mean(x))*(y-mean(y)))
  fit = lm(y~x)
  RSS = sum(residuals(fit)^2)
  paste(c("n", "SSX", "SSXY", "RSS"),
  c(formatC(n, format="d"), formatC(c(SSX, SSXY, RSS), format="f", digits=3)),
        sep="=", collapse=";")
}


set.seed(8873)
for(n in c(20:30)) yaml.out(questions[[2]], set.one(n))

count = c(135, 145, 133, 102, 105, 108, 128, 144, 149, 130, 107, 106, 83, 117, 
          123, 104, 116, 127, 75, 128, 136, 145, 120, 127, 110, 109, 122, 136, 125, 
          143, 177, 142, 134, 153, 173, 151, 174, 168, 158, 156, 128, 156, 140, 133, 
          132)
Y = sqrt(count)

for(i in c(5,10,14,23,21,28,31,34,37,40)) {
  str = paste("y=c(", paste(formatC(Y[i:(i+5)], format="f", digits=2), collapse=","),")")
  yaml.out(questions[[3]], str)
}

set.seed(78865)
for(i in 1:10) yaml.out(questions[[4]], "s=round(rnorm(3),3)")


yaml.out(questions[[5]], ' a = 10; b = 0.001')
yaml.out(questions[[5]], ' a = 10; b = 0.005')
yaml.out(questions[[5]], ' a = 10; b = 0.007')
yaml.out(questions[[5]], ' a = 10; b = 0.01')
yaml.out(questions[[5]], ' a = 10; b = 0.05')
yaml.out(questions[[5]], ' a = 10; b = 0.07')
yaml.out(questions[[5]], ' a = 10; b = 0.1')
yaml.out(questions[[5]], ' a = 10; b = 0.2')
yaml.out(questions[[5]], ' a = 10; b = 0.5')
yaml.out(questions[[5]], ' a = 10; b = 1')

