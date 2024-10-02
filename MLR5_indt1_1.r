#---第2部Stan入門編---第5章基本的な回帰とモデルのチェック---5.1重回帰
#---Jiro Nagao

#---相関展望

#---2024-09-09-Monday
#---2024-09-12-Thursday
#---2024-09-13-Friday
#---2024-10-01-Tuesday

#-------------------------------------------------------------------------------
setwd("C:/Users/njiro/OneDrive/O_DRV/WD/R_WD/lsn/matsu/MLR5")
rm(list = ls())
wd <- getwd()

#-------------------------------------------------------------------------------
#---パッケージ読込
library(tidyverse)
library(ggplot2)
library(GGally)
library(ellipse)

#library(gridExtra)
#library(rstan)
#library(ggmcmc)
#library(patchwork)

#-------------------------------------------------------------------------------
indt <- paste0(wd, "/input")
outdt <- paste0(wd, "/output")
mdl <- paste0(wd, "/stan_model")
gra <- paste0(wd, "/graph")

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
#---1変数グラフ描画関数（対角線）
single_v_gr <- function(df, i){
  
  x <- df[,i]                       #---処理する列（データ）を指定
  df0 <- data.frame(x, A = df$A)
  df1 <- data.frame(
    x = -Inf, 
    y = Inf, 
    label = colnames(df)[i]
  )
  
  p <- ggplot(
    data = df0,
    mapping = aes(x)    #---対角は1変数
  )
  p <- p + theme_bw(base_size = 14)
  p <- p + theme(
    axis.text.x = element_text(  #---x軸目盛の設定
      angle = 40,
      vjust = 1,
      hjust = 1
    )
  )
  
  if(class(x) == "factor"){          #---xが離散変数の場合
    p <- p + geom_bar(　　　　　　　 #---棒グラフ表記
      mapping = aes(fill = A),
      colour = "grey5"
      
    )
  }else{　　　　　　　　　　　　　　 #---xが連続変数の場合
    bw <- (max(x)-min(x))/10
    p <- p + geom_histogram(　　　　 #---ヒストグラム表記
      binwidth = bw,
      mapping = aes(fill = A),
      colour = "grey5"
    )
    p <- p + geom_line(              #---確率曲線追加
      mapping = aes(
        y = after_stat(count) * bw
      ),
      stat = "density"
    )
  }
  
  p <- p + geom_label(　　　　　　　#---各変数のラベル
    data = df1,
    mapping = aes(
      x = x,
      y = y,
      label = label
    ),
    vjust = 1,
    hjust = 0
  )
  p <- p + scale_fill_manual(　　　#---色の設定
    values = alpha(
      c("white", "grey40"),
      0.5
    )
  )
}

#-------------------------------------------------------------------------------
#---相関係数グラフ作成（上側）
cor_gra <- function(df, i, j, zcolat, zcolre){
  
  x <- as.numeric(df[, i])
  y <- as.numeric(df[, j])
  r <- cor(x, y, method = "spearman", use = "pairwise.complete.obs")  　　　　#---相関係数の計算
  zcol <- lattice::level.colors(r, at = zcolat, col.regions = grey(zcolre))
  textcol <- ifelse(abs(r) < 0.4, "grey20", "white")                            #---相関係数（正負は関係なく）が0.4未満=黒色, 0.4以上=白色
  ell <- ellipse::ellipse(
    r,
    level = 0.95,
    type = "l",
    npoints = 50,
    scale = c(0.2, 0.2),
    centre = c(0.5, 0.5)
  )
  
  #---グラフ描画
  p <- ggplot(
    data = data.frame(ell),
    mapping = aes(x = x, y = y)
  )
  p <- p + theme_bw()
  p <- p + theme(
    plot.background = element_blank(),
    panel.border = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.ticks = element_blank()
  )
  p <- p + geom_polygon(
    fill = zcol, 
    colour = zcol
  )
  p <- p + geom_text(
    data = NULL,
    x = 0.5,
    y = 0.5,
    label = 100 * round(r, 2),
    size = 6,
    col = textcol
  )
  return(p)
}

#-------------------------------------------------------------------------------
#---2変数グラフ描画関数（下部）
two_v_gr <- function(df, i, j){
  
  x <- df[,j]
  y <- df[,i]
  df2 <- data.frame(x, y, gr = df$A)
  
  p <- ggplot(
    data = df2,
    mapping = aes(x = x, y = y, fill = gr, shape = gr)
  )  
  p <- p + theme_bw(base_size = 14)
  p <- p + theme(
    axis.text.x = element_text(
      angle = 40,
      vjust = 1,
      hjust = 1
    )
  )
  
  if(class(x) == "factor"){   #---カテゴリ変数ならボックス＋揺らぎデータ
    p <- p + geom_boxplot(
      mapping = aes(
        group = x
      ),
      alpha = 3/6,
      outlier.shape = NA,
      fill = "white"
    )
    p <- p + geom_point(
      position = position_jitter(  #---高さはそのまま，幅に揺らぎを与える
        w = 0.4,
        h = 0
      ),
      size = 2
    )
  }else{　　　　　　　　　　　#---連続変数なら散布図
    p <- p + geom_point(
      size = 2
    )
  }
  
  p <- p + scale_shape_manual(
    values = c(21, 24)          #---散布図のshapeは21番と24番
  )
  p <- p + scale_fill_manual(
    values = alpha(
      c("white", "grey40"), 　#---散布図のfillはwhiteとgrey40
      0.5
    )
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
    w = 6,
    h = 6
  )
  print(
    graph,
    left = 0.3,
    bottom = 0.3
  )
  dev.off()
  
}


#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
#---main program
#-------------------------------------------------------------------------------
#---初期設定
#set <- setting()

#---csv読込
ifnm1 <- "data-attendance-1.txt"
df <- rd_csv(indt, ifnm1)

#-------------------------------------------------------------------------------
df$A <- as.factor(df$A)   #---Aを因子化
N_col <- ncol(df)　　　　 #---データ数

#-------------------------------------------------------------------------------
#---相関図枠作成
ggp <- ggpairs(df, upper = "blank", diag = "blank", lower = "blank")

#-------------------------------------------------------------------------------
zcolat <- seq(-1, 1, length = 81)
zcolre <- c(zcolat[1:40] + 1, rev(zcolat[41:81]))

#-------------------------------------------------------------------------------
#---1変数グラフ描画（対角）
for(i in 1:N_col){
  p <- single_v_gr(df, i)
  ggp <- putPlot(ggp, p, i, i)
}

#-------------------------------------------------------------------------------
#---相関係数描画（上部）
for (i in 1:(N_col-1)) {
  for (j in (i+1):N_col) {
    p <- cor_gra(df, i, j, zcolat, zcolre)
    ggp <- putPlot(ggp, p, i, j)
  }
}

#-------------------------------------------------------------------------------
#---2変数グラフ描画（下部）
for (j in 1:(N_col-1)) {
  for (i in (j+1):N_col) {
    p <- two_v_gr(df, i, j)
    ggp <- putPlot(ggp, p, i, j)
  }
}
ggp

#-------------------------------------------------------------------------------
#---グラフ記録
#ofnm1 <- "fig5_1.pdf"
#wr_gr(ggp, gra, ofnm1)



