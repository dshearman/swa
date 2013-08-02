Factorial <- function(n) {
  fac <- 1
  for(i in 1:n) fac <- fac * i
  return(fac)
}

Factorial(5)



RFactorial <- function(n) {
  if(n==1) return(n)
  else return(n*RFactorial(n-1))
}

RFactorial(5)
