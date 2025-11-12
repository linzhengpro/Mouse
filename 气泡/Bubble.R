# 创建示例数据
set.seed(12)
gene_data <- data.frame(
  Gene = factor(rep(paste0("Gene",1:10), each = 5),levels = paste0("Gene",1:10)),
  CellType = rep(c("Type1", "Type2", "Type3","Type4","Type5"), times = 10),
  Expression = rnorm(50, mean = 0, sd = 1))

rm(list = ls())

gene_data <- read.csv("BOBO2.csv", header = TRUE, colClasses = c("factor", "character", "numeric", "numeric"))
# 定义颜色映射函数
library(ggplot2)
library(ggprism)
library(RColorBrewer)
library(colorRamp2)

colors <- colorRampPalette(c("#76C131","#C388FE"))(100)
values <- seq(-2.5, 2.5, length.out = 101)[-101]
col_fun <- colorRamp2(values, colors)

# 使用 ggplot2 创建气泡图
ggplot(gene_data, aes(x = Gene, y = Term, size = abs(p.value), color = log2)) +
  geom_point(alpha = 0.9) +
  scale_size_continuous(range = c(3, 10)) +
  scale_color_gradientn(colors = col_fun(values)) +
  ggprism::theme_prism(border = T)+
  labs(title = "Gene Expression Across Cell Types", x = "Sample", y = "Cell Type")+
  theme(axis.text.x = element_text(angle=90, hjust=1, vjust=.5))






# Read the data
gene_data <- read.csv("BOBO2.csv", header = TRUE, colClasses = c("factor", "character", "numeric", "numeric"))

# Define the order for the Gene and Term columns
gene_order <- c("Creatinine", "PEP", "Adenosine", "cAMP", "AMP", "ADP", "GDP" ,"ATP",
                "GTP", "NADH", "NADP", "FAD", "R5P", "2-O", "cGMP", "GMP")
term_order <- c("CK/MPS")  # Since you have only one cell type, this can stay the same

# Convert Gene and Term to factors with specified levels
gene_data$Gene <- factor(gene_data$Gene, levels = gene_order)
gene_data$Term <- factor(gene_data$Term, levels = term_order)

# Define color mapping function
library(ggplot2)
library(ggprism)
library(RColorBrewer)
library(circlize)

colors <- colorRampPalette(c("#76C131","#C388FE"))(100)
values <- seq(-2.5, 2.5, length.out = 101)[-101]
col_fun <- colorRamp2(values, colors)

# Create bubble plot using ggplot2
ggplot(gene_data, aes(x = Gene, y = Term, size = abs(p.value), color = log2)) +
  geom_point(alpha = 0.9) +
  scale_size_continuous(range = c(3, 10)) +
  scale_color_gradientn(colors = col_fun(values)) +
  ggprism::theme_prism(border = T)+
  labs(title = "Gene Expression Across Cell Types", x = "Sample", y = "Cell Type")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))
