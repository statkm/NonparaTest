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
