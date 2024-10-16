#---第2部Stan入門編---第5章基本的な回帰とモデルのチェック---5.1重回帰
#---Jiro Nagao

#---実測値と予測値の関係

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

#-------------------------------------------------------------------------------
indt <- paste0(wd, "/input")
outdt <- paste0(wd, "/output")
mdl <- paste0(wd, "/stan_model")
gra <- paste0(wd, "/graph")

#-------------------------------------------------------------------------------
#---予測値推定関数
est_prd <- function(data, y_prd, probs){
  qua <- apply(
    X      = y_prd,
    MARGIN = 2,
    FUN    = quantile,
    probs  = probs
  )
  d_est <- data.frame(data, t(qua), check.names=FALSE)
  d_est$Score <- d_est$Score / 200
  d_est$A <- as.factor(d_est$A)
  
  return(d_est)
}

#-------------------------------------------------------------------------------
#---グラフ描画関数
gr <- function(data){
  p <- ggplot(
    data = data, 
    mapping = aes(
      x = Y, 
      y = `50%`, 
      ymin = `10%`, 
      ymax = `90%`, 
      shape = A, 
      fill = A
    )
  )
  p <- p + theme_bw(base_size = 18)
  p <- p + theme(
    legend.key.height = grid::unit(2.5, 'line')
  )
  p <- p + coord_fixed(
    ratio = 1, 
    xlim = c(0, 0.5), 
    ylim = c(0, 0.5)
  )
  p <- p + geom_pointrange(
    size = 0.5, 
    color = 'grey5'
  )
  p <- p + geom_abline(
    mapping = aes(
      slope = 1, 
      intercept = 0
    ), 
    color = 'black', 
    alpha = 3/5, 
    linetype = '31'
  )
  p <- p + scale_shape_manual(values = c(21, 24))
  p <- p + scale_fill_manual(values = c('white', 'grey70'))
  p <- p + labs(x = 'Observed', y = 'Predicted')
  p <- p + scale_x_continuous(breaks = seq(from = 0, to = 0.5, by = 0.1))
  p <- p + scale_y_continuous(breaks = seq(from = 0, to = 0.5, by = 0.1))
  
  return(p)
}

#-------------------------------------------------------------------------------
#---グラフ記録関数
wr_gr <- function(graph, pathname, filename){
  pdf(
    file = paste0(
      pathname,
      "/",
      filename
    ),
    height = 4,
    width = 5
  )
  
  print(graph)
  
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
#ms

#-------------------------------------------------------------------------------
#---初期設定
prb <- c(0.1, 0.5, 0.9)
y_prd <- ms$y_pred

#-------------------------------------------------------------------------------
#---予測値不確実性推定
d_est <- est_prd(df, y_prd, prb)
#head(d_est)

#-------------------------------------------------------------------------------
#---グラフ描画
p <- gr(d_est)
p

#-------------------------------------------------------------------------------
#---グラフ記録
#opnm1 <- gra
#ofnm1 <- "fig5_3.pdf"
#r_gr(p, opnm1, ofnm1)

