#---第2部Stan入門編---第5章基本的な回帰とモデルのチェック---5.1重回帰
#---Jiro Nagao

#---予測区間グラフ作成

#---2024-10-04-Friday
#---2024-10-16-Wednesday

#-------------------------------------------------------------------------------
#setwd("C:/Users/njiro/OneDrive/O_DRV/WD/R_WD/lsn/matsu/MLR5")
setwd("C:/Users/njiro_o08316b/OneDrive/O_DRV/WD/R_WD/lsn/matsu//MLR5")

rm(list = ls())
wd <- getwd()

#-------------------------------------------------------------------------------
#---パッケージ読込
library(tidyverse)
library(ggplot2)
library(GGally)
library(ellipse)

library(gridExtra)
library(rstan)
library(ggmcmc)
library(patchwork)

#-------------------------------------------------------------------------------
indt <- paste0(wd, "/input")
outdt <- paste0(wd, "/output")
mdl <- paste0(wd, "/stan_model")
gra <- paste0(wd, "/graph")

#-------------------------------------------------------------------------------
#---予測値計算関数
expct_f <- function(X_new, N_mcmc, N_X, b1, b2, b3, sigma){
  
  #---枠作成
  base1 <- as.data.frame(matrix(nrow = N_mcmc, ncol = N_X))
  base2 <- as.data.frame(matrix(nrow = N_mcmc, ncol = N_X))
  
  mcmc1 <- as.data.frame(matrix(nrow = N_mcmc, ncol = N_X))
  mcmc2 <- as.data.frame(matrix(nrow = N_mcmc, ncol = N_X))
  
  for(i in 1:N_X){
    base1[,i] <- b1 + b2 * 0 + b3 * X_new[i]
    base2[,i] <- b1 + b2 * 1 + b3 * X_new[i]
    
    mcmc1[,i] <- rnorm(n = N_mcmc, mean = base1[,i], sd = sigma)
    mcmc2[,i] <- rnorm(n = N_mcmc, mean = base2[,i], sd = sigma)

  }
  expct <- list(base1, base2, mcmc1, mcmc2)
  return(expct)
}

#-------------------------------------------------------------------------------
#---信頼区間推定関数
cal_conf <- function(X_new, df, probs){
  qua <- apply(df, MARGIN = 2, FUN = quantile, prob = probs)
  d_est <- data.frame(X = X_new, t(qua), check.names = FALSE)
  return(d_est)
}

#-------------------------------------------------------------------------------
#---描画関数
gr <- function(d_est, df){
  p <- ggplot()
  p <- p + theme_bw(base_size = 18)
  p <- p + geom_ribbon(
    data = d_est,
    mapping = aes(
      x = X * 200,
      ymin = `2.5%`,
      ymax = `97.5%`,
      group = as.factor(A),
      fill = as.factor(A)
    ),
    alpha = 1/6
  )
  p <- p + geom_ribbon(
    data = d_est,
    mapping = aes(
      x = X * 200,
      ymin = `25%`,
      ymax = `75%`,
      group = as.factor(A),
      fill = as.factor(A)
    ),
    alpha = 2/6
  )
  p <- p + geom_line(
    data = d_est,
    mapping = aes(
      x = X * 200,
      y = `50%`,
      group = as.factor(A),
      colour = as.factor(A)
    ),
    linewidth = 1
  )
  p <- p + geom_point(
    data = as.data.frame(df),
    mapping = aes(
      x = Score,
      y = Y,
      group = as.factor(A),
      colour = as.factor(A),
      shape = as.factor(A)
    ),
    size = 3
  )
  p <- p + coord_cartesian(xlim=c(50, 200), ylim=c(0, 0.5))
  p <- p + scale_y_continuous(breaks=seq(from=0, to=0.5, by=0.2))
  p <- p + labs(x = "Score", y = "Y")
  
  return(p)
}

#-------------------------------------------------------------------------------
#---グラフ記録関数
wr_gr <- function(plot, path, filename){
  pdf(
    file = paste0(
      path,
      "/",
      filename
    ),
    height = 6,
    width  = 8
  )
  
  print(plot)
  
  dev.off()
}


#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
#---main program
#-------------------------------------------------------------------------------
#---イメージ読込
ipnm1 <- outdt
ifnm1 <- "MLR5_result.RData"
load(file = paste0(ipnm1,"/",ifnm1))

#-------------------------------------------------------------------------------
#---予測のために生成したX値
N_mcmc <- length(ms$lp__)        #---mcmcサンプルの数
N_Score <- 100                   #---生成X値の数
Score_new <- seq(min(data$Score), max(data$Score), length = N_Score)

#---パラメータの取込
b1 <- ms$b1
b2 <- ms$b2
b3 <- ms$b3
sigma <- ms$sigma

#-------------------------------------------------------------------------------
#---予測値計算
set.seed(1234)
expct <- expct_f(Score_new, N_mcmc, N_Score, b1, b2, b3, sigma)

base1 <- expct[[1]]　　　　　#---回帰直線の期待値(A = 0)
base2 <- expct[[2]]　　　　　#---回帰直線の期待値(A = 1)
mcmc1 <- expct[[3]]　　　　　#---データの予測値(A = 0)
mcmc2 <- expct[[4]]　　　　　#---データの予測値(A = 1)

#-------------------------------------------------------------------------------
#---信頼区間＆予測区間の推定
probs <-c(0.025, 0.25, 0.50, 0.75, 0.975)  #---信頼区間の水準
d_base1 <- cal_conf(Score_new, base1, probs)　　　#---回帰直線の信頼区間
d_base2 <- cal_conf(Score_new, base2, probs)　　　#---回帰直線の信頼区間
d_mcmc1 <- cal_conf(Score_new, mcmc1, probs)　　　#---データの予測区間
d_mcmc2 <- cal_conf(Score_new, mcmc2, probs)　　　#---データの予測区間

d_base1_1 <- d_base1 |> 
  mutate(A = 0)
d_base2_1 <- d_base2 |> 
  mutate(A = 1)

d_mcmc1_1 <- d_mcmc1 |> 
  mutate(A = 0)
d_mcmc2_1 <- d_mcmc2 |> 
  mutate(A = 1)

d_base <- rbind(d_base1_1, d_base2_1)
d_mcmc <- rbind(d_mcmc1_1, d_mcmc2_1)
#head(d_base)
#head(d_mcmc)

#-------------------------------------------------------------------------------
#---グラフ描画
p_base <- gr(d_base, df)　　　#---回帰直線と信頼区間
p_mcmc <- gr(d_mcmc, df)　　　#---回帰直線と予測区間
p <- grid.arrange(p_base, p_mcmc)
p

#-------------------------------------------------------------------------------
#---図の記録
#opnm1 <- gra
#ofnm1 <- "fig5_2.pdf"
#wr_gr(plot = p, path = opnm1, filename = ofnm1)



