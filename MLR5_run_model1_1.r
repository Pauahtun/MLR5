#---第2部Stan入門編---第5章基本的な回帰とモデルのチェック---5.1重回帰
#---Jiro Nagao

#---モデルラン

#---2024-10-01-Tuesday

#-------------------------------------------------------------------------------
setwd("C:/Users/njiro/OneDrive/O_DRV/WD/R_WD/lsn/matsu/MLR5")
rm(list = ls())
#wd <- getwd()

#-------------------------------------------------------------------------------
#---パッケージ読込
library(tidyverse)
library(ggplot2)
#library(GGally)
#library(ellipse)

#library(gridExtra)
library(rstan)
#library(ggmcmc)
#library(patchwork)

#-------------------------------------------------------------------------------
indt <- paste0(wd, "/input")
outdt <- paste0(wd, "/output")
mdl <- paste0(wd, "/stan_model")
gra <- paste0(wd, "/graph")

#-------------------------------------------------------------------------------
#---初期設定関数
setting <- function(){
  set <- data.frame(
    seed = 123,
    chain = 3,
    iter = 1000,
    warmup = 200,
    thin =2
  )
  return(set)
}

#-------------------------------------------------------------------------------
#---csv読込関数
rd_csv <- function(path, filename){
  df <- read.csv(
    file = paste0(
      path,
      "/",
      filename
    ),
    header = TRUE
  )
  return(df)
}

#-------------------------------------------------------------------------------
#---stan出力記録関数
wr_stan <- function(path, filename){
  save.image(
    file = paste0(
      path,
      "/",
      filename
    )
  )
}

#-------------------------------------------------------------------------------
#---stanコンパイル関数
cmpl_stan <- function(path, filename){
  df <- stan_model(
    file = paste0(
      path,
      "/",
      filename
    )
  )
  return(df)
}

#-------------------------------------------------------------------------------
#---パラメータリスト設定関数
set_par <- function(){
  par <- c("b1", "b2", "b3", "sigma")
  return(par)
}

#-------------------------------------------------------------------------------
#---パラメータ初期設定関数
set_parini <- function(){
  par_ini <- list(
    b1 = runif(1, -10, 10),
    b2 = runif(1, 0, 10),
    b3 = runif(1, 0, 10),
    sigma = 10
  )
  return(par_ini)
}

#-------------------------------------------------------------------------------
#---stanサンプリング関数
smp_stan <- function(model, data, pars, parameter_list, seed, chain, iter, warmup, thin){
  fit <- sampling(
    model,
    data = data,
    pars = pars,
    #init = function(){parameter_list},
    seed = seed,
    chain = chain,
    iter = iter,
    warmup = warmup,
    thin = thin
  )
  return(fit)
}


#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
#---main program
#-------------------------------------------------------------------------------
#---初期設定
set <- setting()
pars <- set_par()

#---csv読込
ifnm1 <- "data-attendance-1.txt"
df <- rd_csv(indt, ifnm1)

#-------------------------------------------------------------------------------
#df$A <- as.factor(df$A)   #---Aを因子化
#N_col <- ncol(df)　　　　 #---データ数

data <- list(
  N = nrow(df),
  A = df$A,
  Score = df$Score/200,
  Y = df$Y
)

#-------------------------------------------------------------------------------
#---stanモデルの実行
#---stanファイルのコンパイル
ifnm2 <- "model5_3.stan"
stanmodel <- cmpl_stan(mdl, ifnm2)

#---サンプリング
fit <- smp_stan(
  stanmodel, 
  data = data, 
  pars = pars, 
  #parameter_list = par_ini, 

    chain = set$chain, 
  seed = set$seed, 
  iter = set$iter, 
  warmup = set$warmup, 
  thin = set$thin
)

#---サンプル抽出
ms <- rstan::extract(fit)

#-------------------------------------------------------------------------------
#---stan結果出力
ofnm1 <- "MLR5_result.RData"
wr_stan(outdt, ofnm1)
