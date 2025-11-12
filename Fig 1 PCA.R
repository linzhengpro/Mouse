library(grid)
library(gridExtra)
library(ggplot2)
library(cowplot)
source("pca.r")#载入使用
df<-read.csv("pca3.csv" )
pca.ncg<-.pca(data = df[,2:4], ##
              is.log = FALSE)


.scatter.density.pc(pcs = pca.ncg$sing.val$v[, 1:3], ##展示几个
                    pc.var = pca.ncg$variation,
                    strokeColor = 'gray30',
                    strokeSize = .2,
                    pointSize = 6,
                    alpha = .6,
                    title = "A", ##标题名
                    group.name = "Stage", # legned name
                    group=df$classification, # 选择分组
                    color=c("#614099","#E5086A","#FABB6E","#FC8002","#ADDB88","#369F2D","#FAC7B3"
                            ,"#EE4431","#B9181A","#CEDFEF","#92C2DD","#4995C6"
                            ,"#1663A9","#B4B4D5","#8481BA",
                            "#B0A875")) -> p#颜色


do.call(
  gridExtra::grid.arrange,
  c(p,ncol=4,nrow=1))


.scatter.density.pc(pcs = pca.ncg$sing.val$v[, 1:3], ##
                    pc.var = pca.ncg$variation,
                    strokeColor = 'gray30',
                    strokeSize = .2,
                    pointSize = 4,
                    alpha = .6,
                    title = "A", ##    
                    group.name = "Stage", # legned name
                    group=df$classification, #
                    color=c("#FABB6E","#FC8002","#ADDB88","#369F2D","#FAC7B3"
                            ,"#EE4431","#B9181A","#CEDFEF","#92C2DD","#4995C6"
                            ,"#1663A9","#B4B4D5","#8481BA","#614099","#E5086A",
                            "#B0A875")) -> p

do.call(
  gridExtra::grid.arrange,
  c(p,ncol=2,nrow=2))




.scatter.density.pc(pcs = pca.ncg$sing.val$v[, 1:3], ##
                    pc.var = pca.ncg$variation,
                    strokeColor = 'gray30',
                    strokeSize = .2,
                    pointSize = 4,
                    alpha = .6,
                    title = "A", ##    
                    group.name = "Stage", # legned name
                    group=df$classification, #
                    color=c("#614099","#E5086A","#FABB6E","#FC8002","#ADDB88","#369F2D","#FAC7B3"
                            ,"#EE4431","#B9181A","#CEDFEF","#92C2DD","#4995C6"
                            ,"#1663A9","#B4B4D5","#8481BA",
                            "#B0A875")) -> p
do.call(
  gridExtra::grid.arrange,
  c(p,ncol=2,nrow=2))

