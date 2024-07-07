library(tidyverse)
# Brunner-Munzel


# solving
k1<- 1; k2 <- 3; 

K<-k1+k2
coef_ <- numeric(K)
coef_k1 <- choose(k1, 0:k1)*((-1)^(0:k1))/gamma(k1) 
coef_k1

n<-0
for(n in 0:(k2-1))
coef_[(n+1):(n+1+length(coef_k1)-1)] <- 
  coef_[(n+1):(n+1+length(coef_k1)-1)]+gamma(k1+n)/gamma(n+1)*coef_k1

coef_[1]<-coef_[1]-0.5
coef_

roots<-polyroot(coef_)

root<-roots[(abs(Im(roots)) < 10^-6) & (Re(roots)>0)]
length(root)

Re(root)



func_Erlang_WMW <- function(k1, k2, eps=10^-6){
  K<-k1+k2
  coef_ <- numeric(K)
  coef_k1 <- choose(k1, 0:k1)*((-1)^(0:k1))/gamma(k1) 
  for(n in 0:(k2-1))
    coef_[(n+1):(n+1+length(coef_k1)-1)] <- coef_[(n+1):(n+1+length(coef_k1)-1)]+gamma(k1+n)/gamma(n+1)*coef_k1
  coef_[1]<-coef_[1]-0.5 # converting to polyfunction = 0
  roots<-polyroot(coef_)
  root<-roots[(abs(Im(roots)) < eps) & (0<Re(roots)) & (Re(roots)<1)]
  if(length(root)==1){
    r_root <- Re(root)
  } 
  else 
    warning("Some roots are found.")
  return(list(r_root=r_root, roots=roots, coef_=coef_, k1=k1, k2=k2))
}



k1<- 1; k2 <- 10; 
results<-func_Erlang_WMW(k1=k1, k2=k2)
results


lambda1 <- 1-results$r_root;
lambda2 <- results$r_root;


N<-10000000
dfx <- rgamma(N, shape=k1, rate = lambda1)
dfy <- rgamma(N, shape=k2, rate = lambda2)
mean(dfx<dfy)



ggplot(data = data.frame(X=c(0, k1/lambda1+k1/(lambda1^2)*2)), aes(x=X)) +
  stat_function(fun=function(x) dgamma(x, shape=k1, rate = lambda1), aes(color="X")) +
  stat_function(fun=function(x) dgamma(x, shape=k2, rate = lambda2), aes(color="Y"))





library(ggplot2)

p <- ggplot(data=data.frame(X=c(0,k1/lambda1+k1/(lambda1^2)*2)), aes(x=X))
p <- p + stat_function(fun=function(x) dgamma(x, shape = k1, rate = lambda1))
p