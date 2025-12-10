
#导入表达量数据；
exp <- read.csv("otu.csv",row.names = 1)
#导入理化信息数据；
chem <- read.table("env.xls",header = T,row.names = 1)
chem<-read.table(file='env.csv',header=TRUE,row.names= 1,sep=',')
#预览数据；
head(exp)
head(chem)



#对数据框进行转置；
expt <- t(exp)
chemt <- t(chem)
#预览转置后的数据；
expt[1:9,1:6]
chemt[1:9,1:10]


library(psych)
#计算基因表达量之间的pearson相关性；
ct1 <- corr.test(expt,chemt,method = "pearson")
#提取相关性系数矩阵；
r1 <- ct1$r
#提取pvalue值矩阵；
p1 <- round(ct1$p,3)

#预览转置后的相关性系数矩阵和pvalue矩阵；
r2 <- t(r1)
p2 <- t(p1)
#使用显著性星号标记进行替换；
p2[p2>=0 & p2 < 0.001] <- "***"
p2[p2>=0.001 & p2 < 0.01] <- "**"
p2[p2>=0.01 & p2 < 0.05] <- "*"
p2[p2>=0.05 & p2 <= 1] <- ""


#载入pheatmap包；
library(pheatmap)
#自定义颜色；
mycol<-colorRampPalette(c("#0f86a9", "white", "#ed8b10"))(200)
#绘制热图；
pheatmap(r2,scale = "none",
         border_color ="white",
         number_color="white",
         fontsize_number=14,
         fontsize_row=8,
         fontsize_col=9,
         cellwidth=15,
         cellheight=15,
         cluster_rows=F,
         cluster_cols=F,
         color = mycol,
         display_numbers = p2,
         show_rownames=T)

# 导出相关性系数矩阵(r2)到CSV文件
write.csv(r2, file = "correlation_coefficients.csv", quote = FALSE)

# 如果需要同时导出p值矩阵(p2)
write.csv(p2, file = "correlation_pvalues.csv", quote = FALSE)

