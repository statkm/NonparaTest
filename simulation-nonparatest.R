library(tidyverse)
library(ggplot2)

#install.packages("exactRankTests")
#library(exactRankTests)

#Brunner-Munzel
#install.packages("lawstat")
library(lawstat)

#install.packages("DescTools")
#if (!require("remotes")) install.packages("remotes")
#remotes::install_github("AndriSignorell/DescTools")

#install.packages("rQCC")
#library(rQCC)

#install.packages("exactRankTests")
#library(exactRankTests)

#Erlang distribution

k1<- 3; k2 <- 20; 
results<-func_Erlang_WMW(k1=k1, k2=k2)
lambda1 <- 1-results$r_root;
lambda2 <- results$r_root;

#plot of density
xmax <- max(k1/lambda1+sqrt(k1/(lambda1^2))*3,
            k2/lambda2+sqrt(k2/(lambda2^2))*3)
ggplot(data = data.frame(X=c(0, xmax)), aes(x=X)) +
  stat_function(fun=function(x) dgamma(x, shape=k1, rate = lambda1), aes(color="X")) +
  stat_function(fun=function(x) dgamma(x, shape=k2, rate = lambda2), aes(color="Y"))


cat(" k: ", k1, k2, "\n",
    "lambda: ", lambda1, lambda2, "\n", "\n",
    "mean: ", k1/lambda1, k2/lambda2, ", ratio:", k1/lambda1/(k2/lambda2), "\n",
    "std: ", (sqrt(k1)/lambda1), (sqrt(k2)/lambda2), ", ratio:", (sqrt(k1)/lambda1)/(sqrt(k2)/lambda2), "\n",
    "skw: ", 2/sqrt(k1), 2/sqrt(k2), ", ratio:", 2/sqrt(k1)/(2/sqrt(k2)), "\n",
    "kur: ", 6/k1, (6/k2), ", ratio:", 6/k1/(6/k2), "\n",
    "\n")

Nsim <- 100000
df_pval <-c(); df_pval2 <-c(); df_pval3<-c(); df_pval4<-c()
df_est <-c()
pb <- txtProgressBar(min = 1, max = Nsim, style = 3)
N<-500
for(i in 1:Nsim){
  dfx <- rgamma(N, shape=k1, rate = lambda1)
  dfy <- rgamma(N, shape=k2, rate = lambda2)
  #dfx <- rgamma(N, shape=k1, scale = theta1)
  #dfy <- rgamma(N, shape=k2, scale = theta2)
  #dfx <- rnorm(N, mean=-0.32, sd=0.55)
  #dfy <- rnorm(N, mean=-0.05, sd=0.55)
  
  #df_est <- c(df_est, pairwiseCI() )
  df_pval <- c(df_pval, wilcox.test(x=dfx, y=dfy)$p.value)
  df_pval2 <- c(df_pval2, brunner.munzel.test(x=dfx, y=dfy)$p.value)
  df_pval3 <- c(df_pval3, t.test(x=dfx, y=dfy, var.equal = TRUE)$p.value)
  df_pval4 <- c(df_pval4, t.test(x=dfx, y=dfy, var.equal = FALSE)$p.value)
 
  
  setTxtProgressBar(pb, i) 
}

dat_results <-rbind(
  data.frame(value=df_pval, test="WMW"),
  data.frame(value=df_pval2, test="BM"),
  data.frame(value=df_pval3, test="T"),
  data.frame(value=df_pval4, test="Welch")
)

dat_results %>%
  ggplot(aes(x=value, fill=test, y = ..density..))+
  geom_histogram(alpha=0.4, position="identity")

dat_results %>%
  group_by(test) %>%
  summarize(Nsim=n(), k1=k1, k2=k2, lambda1=lambda1, lambda2=lambda2, pval=mean(value), alpha=mean(value<0.05)
            )



N<-100000
dfx <- rgamma(N, shape=k1, rate = lambda1)
dfy <- rgamma(N, shape=k2, rate = lambda2)
mean(dfx<dfy)














# Exact test


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





