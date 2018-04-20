# Assume that we have two samples are from normal populations.
# Function to compute 95% CI for ??1 ??? ??2 based on the Welch approximate degrees of freedom method:
welch.ci <- function(x1,x2){
  n1 = length(x1); n2 = length(x2)
  f = (var(x1)/n1+var(x2)/n2)^2/(var(x1)^2/n1^2/(n1-1)+var(x2)^2/n2^2/(n2-1))
  margin = qt(1-0.05/2,f)*sqrt(var(x1)/n1+var(x2)/n2)
  Welch.ci = c(mean(x1)-mean(x2)-margin, mean(x1)-mean(x2) + margin)
  return(Welch.ci)
}
x1 = c(2.64,2.89,1.89,1.46,2.47,1.75,2.39,0.69,1.05,0.51)
x2 = c(3.04,2.89,5.04,2.74,5.16,2.96,1.06,2.54,3.57,3.39,3.28,4.29)