library(corrplot) # 这个包是今天可视化的主角
library(RColorBrewer) # 用来配色的
data("Boston",package = "ISLR2") #利用Boston数据集 ，注意需要安装ISLR2这个包
colnames(Boston) = stringr::str_to_title(colnames(Boston))
cor(Boston) -> corr_data

corr_data <- read.csv("cor.csv", header = TRUE, row.names = 1)
corr_data <- as.matrix(corr_data)
custom_colors <- colorRampPalette(c("#76C131","#C388FE"))(200)

corrplot(corr = corr_data, 
         method = 'color',
         col = custom_colors,
         outline = TRUE,
         tl.col = "black",
         is.corr = FALSE)


corrplot(corr = corr_data, 
         col = custom_colors,
         outline = TRUE,
         tl.col = "black",
         is.corr = FALSE)

