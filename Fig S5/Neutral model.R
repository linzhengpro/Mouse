install.packages("minpack.lm")
library(minpack.lm)

spp <- read.csv('otu.txt', head = TRUE, stringsAsFactors = FALSE, row.names = 1, sep = "\t")




# 将数据进行转置
spp <- t(spp)

# 计算总相对丰度的平均值
N <- mean(apply(spp, 1, sum))

# 计算每个物种的平均相对丰度
p.m <- apply(spp, 2, mean)

# 去除平均值为0的物种
p.m <- p.m[p.m != 0]

# 计算每个物种的相对丰度
p <- p.m / N

# 将原始数据二值化，表示物种的存在与否
spp.bi <- 1 * (spp > 0)

# 计算每个物种的出现频率
freq <- apply(spp.bi, 2, mean)

# 去除频率为0的物种
freq <- freq[freq != 0]

# 合并相对丰度和频率数据
C <- merge(p, freq, by = 0)

# 按照频率排序
C <- C[order(C[, 2]),]

# 将结果转为数据框
C <- as.data.frame(C)

# 去除包含0的行
C.0 <- C[!(apply(C, 1, function(y) any(y == 0))),]

# 提取相对丰度和频率的数据
p <- C.0[, 2]
freq <- C.0[, 3]

# 为数据命名
names(p) <- C.0[, 1]
names(freq) <- C.0[, 1]

# 计算d的值
d = 1/N

## 使用非线性最小二乘法（NLS）拟合模型参数 m（或 Nm）
m.fit <- nlsLM(freq ~ pbeta(d, N * m * p, N * m * (1 - p), lower.tail = FALSE), start = list(m = 0.1))

# 输出拟合结果
m.fit

# 计算 m 的置信区间
m.ci <- confint(m.fit, 'm', level = 0.95)

library(Hmisc)

# 计算预测的频率值
freq.pred <- pbeta(d, N * coef(m.fit) * p, N * coef(m.fit) * (1 - p), lower.tail = FALSE)

# 使用 Wilson 方法计算置信区间
pred.ci <- binconf(freq.pred * nrow(spp), nrow(spp), alpha = 0.05, method = "wilson", return.df = TRUE)

# 计算 R2 值
Rsqr <- 1 - (sum((freq - freq.pred)^2)) / (sum((freq - mean(freq))^2))
Rsqr

# 定义数据框 bacnlsALL，包含 p、freq、freq.pred、以及 pred.ci 的数据
bacnlsALL <- data.frame(p, freq, freq.pred, pred.ci[, 2:3])

# 定义点的颜色
inter.col <- rep('black', nrow(bacnlsALL))
inter.col[bacnlsALL$freq <= bacnlsALL$Lower] <- '#00a4ac'  # 定义低于置信区间的点的颜色
inter.col[bacnlsALL$freq >= bacnlsALL$Upper] <- '#e06a5d'  # 定义高于置信区间的点的颜色
# 创建颜色向量
inter.col <- rep('black', nrow(bacnlsALL))
inter.col[bacnlsALL$freq <= bacnlsALL$Lower] <- '#00a4ac'
inter.col[bacnlsALL$freq >= bacnlsALL$Upper] <- '#e06a5d'
write.csv(bacnlsALL, file = "bacnlsALL_with_colors.csv", row.names = FALSE)
# 将颜色信息作为新列添加到原始数据框
bacnlsALL$point_color <- inter.col
# 加载 grid 包
library(grid)

# 创建新页面
grid.newpage()

# 设置视图
pushViewport(viewport(h = 0.6, w = 0.6))

# 设置数据视图范围
pushViewport(dataViewport(xData = range(log10(bacnlsALL$p)), yData = c(0, 1.02), extension = c(0.02, 0)))

# 绘制矩形
grid.rect()

# 绘制散点图
grid.points(log10(bacnlsALL$p), bacnlsALL$freq, pch = 20, gp = gpar(col = inter.col, cex = 0.7))

# 添加 y 轴和 x 轴
grid.yaxis()
grid.xaxis()

# 绘制预测的曲线
grid.lines(log10(bacnlsALL$p), bacnlsALL$freq.pred, gp = gpar(col = '#0a71b4', lwd = 2), default = 'native')

# 绘制置信区间
grid.lines(log10(bacnlsALL$p), bacnlsALL$Lower, gp = gpar(col = '#0a71b4', lwd = 2, lty = 2), default = 'native')
grid.lines(log10(bacnlsALL$p), bacnlsALL$Upper, gp = gpar(col = '#0a71b4', lwd = 2, lty = 2), default = 'native')

# 添加文字标签
grid.text(y = unit(0, 'npc') - unit(2.5, 'lines'), label = 'Mean Relative Abundance (log10)', gp = gpar(fontface = 2))
grid.text(x = unit(0, 'npc') - unit(3, 'lines'), label = 'Frequency of Occurrence', gp = gpar(fontface = 2), rot = 90)

# 定义绘制文字的函数
draw.text <- function(just, i, j) {
  grid.text(paste("Rsqr=", round(Rsqr, 3), "\n", "Nm=", round(coef(m.fit) * N)), x = x[j], y = y[i], just = just)
}

# 添加文字标签
x <- unit(1:4/5, "npc")
y <- unit(1:4/5, "npc")
draw.text(c("centre", "bottom"), 4, 1)










#加载R包和设置工作路径
library(Hmisc)
library(minpack.lm)
library(stats4)
library(grid)

otu<-read.csv('otu.csv',head=T,row.names=1,)#读取工作路径下名为my_OTU的CSV数据
#应用非线性最小二乘法生成OTU出现频率与其相对丰度之间的最佳拟合
r_mean <- mean(apply(otu, 1, sum))#计算行均值
n_mean <- apply(otu, 2, mean)#计算列均值
n_mean <- n_mean[n_mean != 0]
p <- n_mean/r_mean
spp.bi <- 1*(spp>0)
freq <- apply(spp.bi, 2, mean)
freq <- freq[freq != 0]
C <- merge(p, freq, by=0)
C <- C[order(C[,2]),]
C <- as.data.frame(C)
C.0 <- C[!(apply(C, 1, function(y) any(y == 0))),]
p <- C.0[,2]
freq <- C.0[,3]
names(p) <- C.0[,1]
names(freq) <- C.0[,1]
d = 1/N
#下面使用非线性最小二乘法进行拟合
m.fit <- nlsLM(freq ~ pbeta(d, N*m*p, N*m*(1 -p), lower.tail=FALSE),start=list(m=0.1))
m.fit  #获取 m 值
m.ci <- confint(m.fit, 'm', level=0.95)#获取拟合置信区间
freq.pred <- pbeta(d, N*coef(m.fit)*p, N*coef(m.fit)*(1 -p), lower.tail=FALSE)
pred.ci <- binconf(freq.pred*nrow(spp), nrow(spp), alpha=0.05, method="wilson", return.df=TRUE)
Rsqr <- 1 - (sum((freq - freq.pred)^2))/(sum((freq - mean(freq))^2))
Rsqr #获取模型R方值
#上面有几个值值得关注
#freq 代表出现频率的观测值
#freq.pred 代表中性模型拟合值

bacnlsALL <-data.frame(p,freq,freq.pred,pred.ci[,2:3])#组合数据，然后分别设定各自点的颜色
inter.col<-rep('black',nrow(bacnlsALL))#中性模型颜色
inter.col[bacnlsALL$freq <= bacnlsALL$Lower]<-'#A52A2A'#出现频率低于中性群落模型预测的部分
inter.col[bacnlsALL$freq >= bacnlsALL$Upper]<-'#29A6A6'#出现频率高于中性群落模型预测的部分
grid.newpage()
pushViewport(viewport(h=0.6,w=0.6))
pushViewport(dataViewport(xData=range(log10(bacnlsALL$p)), yData=c(0,1.02),extension=c(0.02,0)))
grid.rect()
grid.points(log10(bacnlsALL$p), bacnlsALL$freq,pch=20,gp=gpar(col=inter.col,cex=0.7))
grid.yaxis()
grid.xaxis()
grid.lines(log10(bacnlsALL$p),bacnlsALL$freq.pred,gp=gpar(col='blue',lwd=2),default='native')

grid.lines(log10(bacnlsALL$p),bacnlsALL$Lower ,gp=gpar(col='blue',lwd=2,lty=2),default='native') 
grid.lines(log10(bacnlsALL$p),bacnlsALL$Upper,gp=gpar(col='blue',lwd=2,lty=2),default='native')  
grid.text(y=unit(0,'npc')-unit(2.5,'lines'),label='Mean Relative Abundance (log10)', gp=gpar(fontface=2)) 
grid.text(x=unit(0,'npc')-unit(3,'lines'),label='Frequency of Occurance',gp=gpar(fontface=2),rot=90) 
draw.text <- function(just, i, j) {
  grid.text(paste("Rsqr=",round(Rsqr,3),"\n","Nm=",round(coef(m.fit)*N)), x=x[j], y=y[i], just=just)
}
x <- unit(1:4/5, "npc")
y <- unit(1:4/5, "npc")
draw.text(c("centre", "bottom"), 4, 1)


