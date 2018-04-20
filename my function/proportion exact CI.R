# Here, k is the number of units in the sample of size n that has the attribute of interest
# N is the sie of the population; n is the sample size
exact.ci <- function(n,N,k){
  alpha = 0.05/2
  if (k==0){
    ML =0; MU = ci.prop.upp(k,N,n)
  } else if (k==n){
    MU = N
    ML = ci.prop.low(k,N,n)
  } else{
    ML = ci.prop.low(k,N,n)
    MU = ci.prop.upp(k,N,n)
  }
  return(c(ML/N,MU/N))
}
ci.prop.low <- function(k, N,n){
  alpha = 0.05/2
  L = floor(N*k/n)
  for (ML in 0:L){
    prob = 1- phyper(k-1, ML, N-ML, n)
    if (prob> alpha) break
  }
  return(ML)
}
ci.prop.upp <- function(k, N,n){
  alpha = 0.05/2
  U = ceiling(N*k/n)
  for (MU in rev(U:N)){
    prob = phyper(k, MU, N-MU, n)
    if (prob> alpha) break
  }
  return(MU)
}
# example
n = 1200; N = 1800000; k= 552
exact.ci(n,N,k)