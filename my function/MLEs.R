# MLES of Weibull
# function that return b, c
# b: scale parameter
# c: shape parameter
mle.weil <- function(x){
  y = log(x); n = length(x)
  c0 = sqrt(6)/pi*sd(y)
  s1 = mean(y)
  z = x^c0
  s2 = sum(z); s3 = sum(y %*% z); s4 = sum(y^2 %*% z)
  f = 1/c0 + s1 - s3/s2
  c0 = c0 + f/(1/c0^2 + (s2*s4 - s3^2)/s2^2)
  while ( f >= 1e-3){
    s1 = mean(y)
    z = x^c0
    s2 = sum(z); s3 = sum(y %*% z); s4 = sum(y^2 %*% z)
    f = 1/c0 + s1 - s3/s2
    c0 = c0 + f/(1/c0^2 + (s2*s4 - s3^2)/s2^2)
  }
  return(c((1/n*s2)^(1/c0),c0)) # to return the MLEs of b, c, respectively.
}

# function that estimate the biases of the MLES of the parameter of a gamma distribution
mle.gamma <- function(x){
  n = length(x); l = 1 # number of loops
  s = log(mean(x))- sum(log(x))/n
  a0 = (3-s+sqrt((s-3)^2+24*s))/(12*s)
  a1 = a0 - (log(a0)-digamma(a0)-s)/(1/a0-trigamma(a0))
  while(abs(a1 - a0)>= 1e-7 || l <= 30){
    a0 = a1
    a1 = a0 - (log(a0)-digamma(a0)-s)/(1/a0-trigamma(a0))
    l = l + 1
  }
  a.hat = a1
  b.hat = mean(x)/a.hat
  return(c(a.hat,b.hat))
}
