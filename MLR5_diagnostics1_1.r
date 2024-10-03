#---第2部Stan入門編---第5章基本的な回帰とモデルのチェック---5.1重回帰
#---Jiro Nagao

#---トレースプロット

#---2024-10-02-Wednesday

#-------------------------------------------------------------------------------
setwd("C:/Users/njiro/OneDrive/O_DRV/WD/R_WD/lsn/matsu/MLR5")
rm(list = ls())
wd <- getwd()

#-------------------------------------------------------------------------------
#---パッケージ読込
library(tidyverse)
library(ggplot2)
#library(GGally)
#library(ellipse)

#library(gridExtra)
library(rstan)
library(ggmcmc)
#library(patchwork)

#-------------------------------------------------------------------------------
indt <- paste0(wd, "/input")
outdt <- paste0(wd, "/output")
mdl <- paste0(wd, "/stan_model")
gra <- paste0(wd, "/graph")

#-------------------------------------------------------------------------------
#---パラメータ表出力関数
wr_par <- function(df0, pathname, filename){
  write.table(
    df0,
    file = paste0(
      pathname,
      "/",
      filename
    ),
    sep = "\t",　　　　#---タブ区切
    quote = FALSE,     #---引用符除去
    col.name = NA
  )
}

#-------------------------------------------------------------------------------
#---収束診断関数1
mcmc_diag1 <- function(fit, pathname, filename){
  ggmcmc(
    ggs(
      fit,
      inc_warmup = TRUE,
      stan_include_auxilia = TRUE
    ),
    file = paste0(
      pathname,
      "/",
      filename
    ),
    plot = "traceplot"
  )
}

#-------------------------------------------------------------------------------
#---収束診断関数2---出力限定version
mcmc_diag2 <- function(fit, pathname, filename){
  ggmcmc(
    ggs(fit),
    file = paste0(
      pathname,
      "/",
      filename
    ),
    plot = c(
      "traceplot",
      "density",
      "running",
      "autocorrelation"
    )
  )
}

#-------------------------------------------------------------------------------
#---収束診断関数3---全出力version
mcmc_diag3 <- function(fit, pathname, filename){
  ggmcmc(
    ggs(fit),
    file = paste0(
      pathname,
      "/",
      filename
    )
  )
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
#---パラメータ表
df <- data.frame(summary(fit)$summary)

#-------------------------------------------------------------------------------
#---パラメータ表出力
opnm1 <- outdt
#ofnm1 <- "MLR5_par_smr.txt"
#wr_par(df, opnm1, ofnm1)

#-------------------------------------------------------------------------------
#---収束診断関数1
#ofnm2 <- "MLR5_traceplot.pdf"
#mcmc_diag1(fit, opnm1, ofnm2)

#-------------------------------------------------------------------------------
#---収束診2---出力限定version
ofnm3 <- "MLR5_diag1.pdf"
mcmc_diag2(fit, opnm1, ofnm3)

#-------------------------------------------------------------------------------
#---収束診3---全出力version
#ofnm4 <- "MLR5_diag2.pdf"
#mcmc_diag3(fit, opnm1, ofnm4)



