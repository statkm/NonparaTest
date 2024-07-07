#Erlang distribution with WMW effect is 0.5
k_cand <- c(1, 2, 5, 10, 20)
k1_cand <- k_cand
k2_cand <- k_cand
lambda_cand <- c(0.1, 1, 5, 10) # lambda1+lambda2

Nsim <- 100000
N<-500

col_<-c("k1", "k2", "lambda1", "lambda2", "mean1", "mean2", "mean.diff", "mean.ratio", 
        "std1","std2", "std.ratio", "skew1", "skew2", "skew.ratio", "kur1", "kur2", "kur.ratio")
n <- length(col_)
df_cand<-data.frame(matrix(rep(NA, n), nrow=1))[numeric(0), ]


for(i in 1:length(k_cand)){
  for(j in i:length(k_cand)){
    for(l in 1:length(lambda_cand)){
      cat("sim: ", i, "-", j, "-", l, "\n")
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
      
      ggsave(plot=g, filename=paste0("results/plot_", k1, "_", k2, "_", round(lambda1,2), "_", round(lambda2, 2), ".png"))
      
      df_cand <- rbind(df_cand,
                       c(k1, k2, lambda1, lambda2, k1/lambda1, k2/lambda2, k1/lambda1-k2/lambda2, k1/lambda1/(k2/lambda2), 
                         (sqrt(k1)/lambda1), (sqrt(k2)/lambda2), (sqrt(k1)/lambda1)/(sqrt(k2)/lambda2), 
                         2/sqrt(k1), 2/sqrt(k2), 2/sqrt(k1)/(2/sqrt(k2)), 
                         6/k1, (6/k2), 6/k1/(6/k2))
      )
      
      
      #simulation

      df_pval <-c(); df_pval2 <-c(); df_pval3<-c(); df_pval4<-c()

      for(isim in 1:Nsim){
        dfx <- rgamma(N, shape=k1, rate = lambda1)
        dfy <- rgamma(N, shape=k2, rate = lambda2)

        df_pval <- c(df_pval, wilcox.test(x=dfx, y=dfy)$p.value)
        df_pval2 <- c(df_pval2, brunner.munzel.test(x=dfx, y=dfy)$p.value)
        df_pval3 <- c(df_pval3, t.test(x=dfx, y=dfy, var.equal = TRUE)$p.value)
        df_pval4 <- c(df_pval4, t.test(x=dfx, y=dfy, var.equal = FALSE)$p.value)
        
      }
      
      dat_results <-rbind(
        data.frame(value=df_pval, test="WMW"),
        data.frame(value=df_pval2, test="BM"),
        data.frame(value=df_pval3, test="T"),
        data.frame(value=df_pval4, test="Welch")
      )
      
      gg<-dat_results %>%
        ggplot(aes(x=value, fill=test, y = ..density..))+
        geom_histogram(alpha=0.4, position="identity")
      ggsave(plot=gg, filename=paste0("results/plot_", k1, "_", k2, "_", round(lambda1,2), "_", round(lambda2, 2), "_results.png"))
      
      dat_results %>%
        group_by(test) %>%
        summarize(Nsim=n(), k1=k1, k2=k2, lambda1=lambda1, lambda2=lambda2, pval=mean(value), alpha=mean(value<0.05)
        ) %>%
        write.csv(., file = paste0("results/table_", k1, "_", k2, "_", round(lambda1,2), "_", round(lambda2, 2), "_results.csv"))
      
    }
  }
}

names(df_cand)<-col_
df_cand %>%
  write.csv(., file = paste0("results/table_simulation_setting.csv"))


