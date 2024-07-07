library(ggplot2)

#install.packages("exactRankTests")
#library(exactRankTests)


#install.packages("DescTools")
#if (!require("remotes")) install.packages("remotes")
#remotes::install_github("AndriSignorell/DescTools")

#install.packages("rQCC")
#library(rQCC)

#install.packages("exactRankTests")
#library(exactRankTests)

polyroot(c(+1/2,-2,1))
polyroot(c(-1/2, 3,-3,1))



coef_<-1-sqrt(2)/2
theta2<-coef_
theta1<-1-coef_
theta2/(theta1+theta2)



Nsim <- 100000
#k1<-1; k2<-2
#theta1<-2+sqrt(2); theta2<-0+sqrt(2); 
k1<-1; k2<-3
coef_<-0.2062995; theta2<-coef_; theta1<-1-coef_
k1*theta1; k2*theta2 #mean
theta1/theta2 #std.ratio
df_pval <-c(); df_est <-c()
pb <- txtProgressBar(min = 1, max = Nsim, style = 3)
N<-1000
for(i in 1:Nsim){
  dfx <- rgamma(N, shape=k1, scale = theta1)
  dfy <- rgamma(N, shape=k2, scale = theta2)
  #dfx <- rnorm(N, mean=-0.32, sd=0.55)
  #dfy <- rnorm(N, mean=-0.05, sd=0.55)
  
  #df_est <- c(df_est, pairwiseCI() )
  
  df_pval <- c(df_pval, wilcox.test(x=dfx, y=dfy)$p.value)
  
  setTxtProgressBar(pb, i) 
}

hist(df_pval)
mean(df_pval<0.05)

ggplot(data = data.frame(X=c(0, 10)), aes(x=X)) +
  stat_function(fun=function(x) dgamma(x, shape=k1, scale = theta1), aes(color="X")) +
  stat_function(fun=function(x) dgamma(x, shape=k2, scale = theta2), aes(color="Y"))



N<-100000
dfx <- rgamma(N, shape=k1, scale = theta1)
dfy <- rgamma(N, shape=k2, scale = theta2)
mean(dfx<dfy)
theta1/theta2












Nsim <- 100000

df_pval <-c()
pb <- txtProgressBar(min = 1, max = Nsim, style = 3)
N<-114
for(i in 1:Nsim){
  dfx <- rnorm(N, mean=-0.22, sd=0.57)
  dfy <- rnorm(N, mean=    0, sd=0.57)
  
  df_pval <- c(df_pval, wilcox.exact(x=dfx, y=dfy, paired = T)$p.value)
  
  setTxtProgressBar(pb, i) 
}

mean(df_pval<0.05)





