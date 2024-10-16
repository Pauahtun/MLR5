#---第2部Stan入門編---第5章基本的な回帰とモデルのチェック---5.1重回帰
#---Jiro Nagao

#---誤差分布

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
#library(GGally)
#library(ellipse)

library(gridExtra)
#library(rstan)
#library(ggmcmc)

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
#---ロング化関数
long_f <- function(df0){
  df1 <- df0 |> 
    pivot_longer(
      cols     = everything(),
      names_to = "parameter"
    ) |> 
    mutate(
      PersonID = readr::parse_number(parameter)
    )
  
  return(df1)
}

#-------------------------------------------------------------------------------
#---密度最大値取得関数
get_maxdns <- function(df0){
  df1 <- df0 |> 
    apply(                              
      MARGIN = 2,                       #---列毎に計算
      FUN    = function(X){
        dens 　<- density(X)　　　　　　#---観測値を密度に変換
        mode_i <- which.max(dens$y)     #---最大密度
        mode_x <- dens$x[mode_i]　　　  #---最大密度のx値
        mode_y <- dens$y[mode_i]　　　  #---最大密度のy値
        mode 　<- c(mode_x, mode_y)     #---最大密度の座標(x,y)
        
        return(mode)
      }
    )
  return(df1)
}

#-------------------------------------------------------------------------------
#---グラフ描画関数
gr <- function(data){
  p <- ggplot()
  p <- p + theme_bw(base_size = 18)
  p <- p + geom_line(
    data = data,
    mapping = aes(
      x = value,
      group = PersonID
    ),
    stat = "density",
    colour = "black",
    alpha = 2/6
  )
  p <- p + geom_segment(
    data = d_mode1, 
    mapping = aes(
      x = X, 
      xend = X, 
      y = Y, 
      yend = 0
    ), 
    color = "black", 
    linetype = "dashed", 
    alpha = 2/6
  )
  p <- p + geom_rug(
    data = d_mode1, 
    mapping = aes(
      x = X
    ), 
    sides = 'b'
  )
  p <- p + labs(
    x = "value", 
    y = "density"
  )
  
  return(p)
}

#-------------------------------------------------------------------------------
#---グラフ描画2
gr2 <- function(d_mode, df, s_MAP){
  p <- ggplot(
    data = d_mode,
    mapping = aes(
      x = X
    )
  )
  p <- p + theme_bw(base_size = 18)
  p <- p + geom_histogram(
    binwidth = bw, 
    color = "black", 
    fill = "white"
  )
  p <- p + geom_density(
    mapping = aes(
      y = after_stat(count) * bw
    ), 
    alpha = 3/6, 
    color = "black", 
    fill = "gray20"
  )
  p <- p + geom_rug(
    sides = "b"
  )
  p <- p + stat_function(
    fun = function(x) nrow(df) * bw * dnorm(x, mean = 0, sd = s_MAP), linetype = "dashed"
  )
  p <- p + labs(
    x = "value", 
    y = "density"
  )
  p <- p + xlim(
    range(density(d_mode$X)$x)
  )
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
    height = 6,
    width = 8
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
Y <- df$Y        　　　　　　#---観測値
mu <- ms$mu　　　　　　　　　#---期待値のmcmcサンプル
N_mcmc <- length(ms$lp__)　　#---mcmcサンプル数

#-------------------------------------------------------------------------------
#---各人の観測値と各人の予測値（分布）の誤差を計算
noise_mcmc <- data.frame(t(replicate(N_mcmc, Y)) - mu, check.names = FALSE)

#-------------------------------------------------------------------------------
#---各人の誤差の最大密度値（モード）を取得
d_mode0 <- get_maxdns(noise_mcmc)
d_mode1 <- d_mode0 |>                  #---行列倒置，データフレーム化，列名入力
  t() |> 
  data.frame() |> 
  magrittr::set_colnames(c('X', 'Y'))
#head(d_mode1)

#-------------------------------------------------------------------------------
#---誤差データのロング化
d_est <- long_f(noise_mcmc)
#head(d_est)

#-------------------------------------------------------------------------------
#---グラフ描画
p <- gr(d_est)
#p

#-------------------------------------------------------------------------------
bw = 0.01
s_dens <- density(ms$s)
s_MAP <- s_dens$x[which.max(s_dens$y)]

p2 <- gr2(d_mode1, df, s_MAP)
#p2

g <- grid.arrange(p, p2, nrow = 2)
g

#-------------------------------------------------------------------------------
#---グラフ記録
#opnm1 <- gra
#ofnm1 <- "fig5_4.pdf"
#wr_gr(g, opnm1, ofnm1)

#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
#ggsave(file='output/fig5-4-left.png', plot=p, dpi=300, w=4, h=3)
#ggsave(file='output/fig5-4-right.png', plot=p, dpi=300, w=4, h=3)
