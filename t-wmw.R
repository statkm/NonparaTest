library(ggplot2)

#gamma
#https://en.wikipedia.org/wiki/Gamma_distribution
# same skewness but different std
beta_<-3 # skewness
k_ <-(2/beta_)^2
std.ratio <- 1.1
# mean
k_*1; k_*1*std.ratio

ggplot(data = data.frame(X=c(0, 5)), aes(x=X)) +
  stat_function(fun=function(x) dgamma(x, shape = k_, scale = 1), aes(color="X")) +
  stat_function(fun=function(x) dgamma(x, shape = k_, scale = 1*std.ratio), aes(color="Y"))

# same mean but different std
mean_ <- 1
std.ratio <- 1.1
theta2_ <- 1
theta_ <- std.ratio^2*theta2_
k_ <- mean_/theta_
k2_ <- mean_/theta2_

ggplot(data = data.frame(X=c(0, 5)), aes(x=X)) +
  stat_function(fun=function(x) dgamma(x, shape = k_, scale = theta_), aes(color="X")) +
  stat_function(fun=function(x) dgamma(x, shape = k2_, scale = theta2_), aes(color="Y"))

# skewness
2/sqrt(k_); 2/sqrt(k2_); 2/sqrt(k_)/(2/sqrt(k2_))











#Erlang distribution with WMW effect is 0.5
k_cand <- c(1, 2, 5, 10, 20)
k1_cand <- k_cand
k2_cand <- k_cand
lambda_cand <- c(0.1, 1, 5, 10) # lambda1+lambda2

N<-10000

col_<-c("k1", "k2", "lambda1", "lambda2", "mean1", "mean2", "mean.diff", "mean.ratio", 
        "std1","std2", "std.ratio", "skew1", "skew2", "skew.ratio", "kur1", "kur2", "kur.ratio")
n <- length(col_)
df_cand<-data.frame(matrix(rep(NA, n), nrow=1))[numeric(0), ]


df_cand

for(i in 1:length(k_cand)){
  for(j in i:length(k_cand)){
    for(l in 1:length(lambda_cand)){
      k1<-k1_cand[i]; k2<-k2_cand[j]
      results<-func_Erlang_WMW(k1=k1, k2=k2)
      
      lambda1 <- (1-results$r_root)*lambda_cand[l];
      lambda2 <- results$r_root*lambda_cand[l];
      
      #dfx <- rgamma(N, shape=k1, rate = lambda1)
      #dfy <- rgamma(N, shape=k2, rate = lambda2)
      #cat(k1, k2, lambda1, lambda2, results$r_root, mean(dfx<dfy), "\n") #P(X<Y)
      #plot of density
      xmax <- max(k1/lambda1+sqrt(k1/(lambda1^2))*2,
                  k2/lambda2+sqrt(k2/(lambda2^2))*2)
      g<-ggplot(data = data.frame(X=c(0, xmax)), aes(x=X)) +
        stat_function(fun=function(x) dgamma(x, shape=k1, rate = lambda1), aes(color="X")) +
        stat_function(fun=function(x) dgamma(x, shape=k2, rate = lambda2), aes(color="Y"))
      
      
      
      
      ggsave(plot=g, filename=paste0("pic/plot_", k1, "_", k2, "_", round(lambda1,1), "_", round(lambda2, 1), ".png"))
      
      df_cand <- rbind(df_cand,
      c(k1, k2, lambda1, lambda2, k1/lambda1, k2/lambda2, k1/lambda1-k2/lambda2, k1/lambda1/(k2/lambda2), 
          (sqrt(k1)/lambda1), (sqrt(k2)/lambda2), (sqrt(k1)/lambda1)/(sqrt(k2)/lambda2), 
          2/sqrt(k1), 2/sqrt(k2), 2/sqrt(k1)/(2/sqrt(k2)), 
          6/k1, (6/k2), 6/k1/(6/k2))
      )
      
    }
  }
}

names(df_cand)<-col_
df_cand















#log normal
#https://en.wikipedia.org/wiki/Log-normal_distribution
fun_ln_skew <- function(mu, sigma){
  sqrt(exp(sigma^2)-1)*(exp(sigma^2)+2)
}

fun_ln_skew(0, 1)

beta_<-3 # skewness
polyroot(c(-4-beta_^2, 0, 3, 1)) # 1.103803 for 1; 1.355301 for 2; 1.668685 for 3
ggplot(data = data.frame(sigma=c(0, 1)), aes(x=sigma)) +
  stat_function(fun=function(x) fun_ln_skew(0, x))

sigma_<-sqrt(log(1.668685))

std.ratio <- 1.1
log(std.ratio)/2

ggplot(data = data.frame(X=c(0, 5)), aes(x=X)) +
  stat_function(fun=function(x) dlnorm(x, meanlog = 0, sdlog = sigma_), aes(color="X")) +
  stat_function(fun=function(x) dlnorm(x, meanlog = 0+log(std.ratio)/2, sdlog = sigma_), aes(color="Y"))

exp(0)
exp(0+log(std.ratio)/2)

exp(0-sigma_^2)
exp(0+log(std.ratio)/2-sigma_^2)
