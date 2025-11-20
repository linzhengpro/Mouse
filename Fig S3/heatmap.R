library(corrmorant)
library(ggplot2)
library(cols4all)

#测试数据：
dt <- drosera #数据集展示了三种非洲茅属植物的叶和叶柄大小
head(dt)
table(dt$species)
table(dt$variety)

dt <- read.csv("about.csv",header=T)

p <- ggcorrm(dt,
             corr_method = c('pearson')) + #分析方法选择,可选'pearson','kendall' or 'spearman'
  lotri(geom_point()) + #下三角区域添加散点图
  lotri(geom_smooth(method = 'lm')) + #下三角区域添加拟合曲线
  utri_corrtext(corr_size = TRUE) + #上三角区域添加相关性系数
  dia_names(y_pos = 0.15) + #对角线区域添加分类标签
  dia_histogram(lower = 0.3, upper = 0.98, color = 'grey30') #对角线区域添加直方图
p


p <- ggcorrm(dt,
             corr_method = c('pearson')) + #分析方法选择,可选'pearson','kendall' or 'spearman'
  lotri(geom_point()) + #下三角区域添加散点图
  lotri(geom_smooth(method = 'lm')) + #下三角区域添加拟合曲线
  utri_corrtext(corr_size = TRUE) + #上三角区域添加相关性系数
  dia_names(y_pos = 0.15) + #对角线区域添加分类标签
  dia_histogram(lower = 0.3, upper = 0.98, color = 'grey30') #对角线区域添加直方图
p



#颜色自定义+区域图表更换：
p1 <- ggcorrm(data = dt,
              corr_method = c('pearson')) +
  lotri(geom_point(alpha = 0.3)) +
  utri_corrtext(corr_size = T) + 
  lotri(geom_smooth(method = 'lm', fill = '#ADDB88', color = '#369F2D')) +
  utri_heatcircle(alpha = 0.5, color = 3) +
  
  dia_density(lower = 0.3, upper = 0.98, fill = '#4995C6', color = '#4995C6', alpha = 0.3) +
  scale_fill_corr(aesthetics = c("fill", "color"), option = 'C')
p1
