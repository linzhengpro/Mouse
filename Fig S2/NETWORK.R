# 检测包，是则跳过，没有则安装
if (!requireNamespace("psych", quietly=TRUE))
  install.packages("psych")
if (!requireNamespace("reshape2", quietly=TRUE))
  install.packages("reshape2")

# 加载包
library(psych)
library(reshape2)

# 导入数据(txt)，可在RStudio右上角手动Import Dataset，设置如下
# OTU/Ev文件：heading=Yes, Row names=first column
# Taxonomy文件：heading=Yes, Row names=automatic

# 或使用如下命令导入数据：
setwd("C:/Users/dell/Desktop/网络图+热图数据/环境因子-微生物互作网络图")

Ev <- read.table("gene.txt", sep="\t", header=T)
OTU <- read.table("otu.txt", sep="\t", header=T, row.names=1)

# 导入节点注释文件
tax <- read.table("ZS1.txt", sep="\t", header=T)
names(tax)[1] <- "Id"

#数据预处理

# 转置数据格式
################################################################################################
# # 情形1（默认）：两数据Ev-OTU表格时:
#Ev=t(Ev)
OTU=t(OTU)

# # 情形2：单数据OTU-OTU表格时：
# OTU=t(OTU)
################################################################################################
#设定分析阈值

#结果不理想时可反复修改这些阈值

# 若OTU数目太多，极大影响计算速度，而且结果不具有可读性
# 按丰度值的百分比进行筛选, 默认保留相对丰度>0.05%的OTU
abundance=0.05

# 筛选
OTU <- OTU[,colSums(OTU)/sum(OTU)>=(abundance/100)]

# 网络分析的关联阈值
r.cutoff=0.6
p.cutoff=0.05



# 计算r、p

# 情形1：两数据Ev-OTU表格时，默认
occor=corr.test(OTU, Ev,
                use="pairwise",
                method="p", # 可选pearson/kendall
                adjust="fdr",
                alpha=0.05)

# 情形2：单OTU-OTU
# occor=corr.test(OTU,
#     use="pairwise",
#     method="spearman",
#     adjust="fdr",
#     alpha=0.05)

# 获取相关矩阵及边数据

# 提取相关性矩阵的r、p值
r_matrix=occor$r
p_matrix=occor$p

# 确定物种间存在相互作用关系的阈值，将相关性R矩阵内不符合的数据转换为0
r_matrix[p_matrix>p.cutoff|abs(r_matrix)<r.cutoff]=0

# 转换数据为长格式形式，方便下游分析
p_value=melt(p_matrix)
r_value=melt(r_matrix)

#将r、p两表合并
r_value=cbind(r_value, p_value$value)

# 删除含r_value=0的行
r_value=subset(r_value, r_value[,3]!=0)

# 删除含r_value=NA的行
r_value=na.omit(r_value)

# 对r表格增补绝对值、正负型等信息
abs=abs(r_value$value)

linktype=r_value$value
linktype[linktype>0]=1
linktype[linktype<0]=-1

r_value=cbind(r_value, abs, linktype)

# 重命名r、p表头
names(r_value) <- c("Source","Target","r_value","p_value", "abs_value", "linktype")
names(p_value) <- c("Source","Target","p_value")

# 输出结果为csv文件

write.csv(r_value,file="1.边数据.csv", row.names=FALSE)
write.csv(r_matrix, file="4.corr_matrix.csv")
write.csv(r_value,file="5.r_value.csv", row.names=FALSE)
write.csv(p_value,file="6.p_value.csv", row.names=FALSE)

# 获取节点数据
# 从边文件提取节点并去除重复
node_OTU <- as.data.frame(as.data.frame(r_value[,1])[!duplicated(as.data.frame(r_value[,1])), ])
node_Ev <- as.data.frame(as.data.frame(r_value[,2])[!duplicated(as.data.frame(r_value[,2])), ])

names(node_OTU)="Id"
names(node_Ev)="Id"

# OTU ID和Ev ID合并成节点索引表，用于检索注释信息
list <- rbind(node_Ev, node_OTU)
write.csv(list,file="3.node_list.csv", row.names=FALSE)

# 筛选节点对应的注释信息
list=subset(tax,Id %in% list$Id)
filtered_tax <- subset(tax, Id %in% list$Id)
# 复制一列当节点Label
list$Label <- list$Id

# 输出结果为csv文件
write.csv(list,file="2.节点数据.csv", row.names=FALSE)
