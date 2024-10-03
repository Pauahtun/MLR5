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
df

#-------------------------------------------------------------------------------
a <- ggs(fit, inc_warmup = TRUE, stan_include_auxiliar = TRUE)
p <- ggmcmc(a, plot = "traceplot")
plot(a)



