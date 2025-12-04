# 高分文章复现系列2-棒棒糖图
# date: 2024-03-22
# author: 研趣生物-小白
# 微信：InfoStudio01


# 程序包安装---
if(!"openxlsx" %in% installed.packages()){install.packages('openxlsx')}
if(!"ggplot2" %in% installed.packages()){install.packages('ggplot2')}

# 加载程序包----
library(openxlsx)
library(ggplot2) 

# 数据读取----
data <- read.xlsx("data.xlsx",check.names = F)

# 细胞类型因子化，固定排序----
data$celltype <- factor(data$celltype, levels = data$celltype)

# 颜色配置----
col_set =c("GO:0006952" = "#ffb25f",
           "GO:0007186" = "#ffb25f",
           "GO:0015693" = "#ffb25f",
           "GO:0043093" = "#ffb25f",
           "GO:0019439" = "#ffb25f",
           "GO:0051302" = "#ffb25f",
           "GO:0005892" = "#f67e6f",
           "GO:0032580" = "#f67e6f",
           "GO:0005634" = "#f67e6f",
           "GO:0005126" = "#c4dbbc",
           "GO:0005267" = "#c4dbbc",
           "GO:0016702" = "#c4dbbc",
           "GO:0008376" = "#c4dbbc",
           "GO:0008990" = "#c4dbbc",
           "GO:0015095" = "#c4dbbc",
           "GO:0009020" = "#c4dbbc",
           "GO:0015385" = "#c4dbbc",
           "GO:0008773" = "#c4dbbc",
           "GO:0016833" = "#c4dbbc",
           "GO:0008373" = "#c4dbbc")

# 开始绘图----
p <- ggplot(data,
            aes(x = celltype, y = r)) +
  # 棒棒图的连线
  geom_segment(aes(x = celltype, xend = celltype, y = 0, yend = r),  
               linetype = "solid", # 实线
               size = 1, # 连线的粗细 
               color = "gray40" # 连线的颜色
               ) + 
  # y轴0刻度的水平线
  geom_hline( 
    yintercept = 0,  # 水平线位置
    linetype = "dashed",  # 虚线
    size = 1, # 连线的粗细 
    colour="gray40" # 连线的颜色
    ) +
  # 绘制点
  geom_point(aes(color = celltype),  
             color = col_set,
             size = 17) +   
  # 添加相关性R值标签
  geom_text(aes(label = r), 
            color = ifelse(data$r != 0.96, "black", 'red'), 
            
            size = 5) +
  # 添加相关性P值标签,这一步最精华的一点是，根据r值正负调整水平移动的位置。
  geom_text(aes(label = p),
            hjust = ifelse(data$r >= 0, 1.5, -0.5),
            vjust = -0.5,
            angle = 90,
            fontface = 'italic',
            color = ifelse(data$p != 'p<0.001', "black", 'red'), 
            size = 5) +
  # y轴刻度设置
  scale_y_continuous( #设置y轴
    limits = c(-4, 5),
    breaks = c(-4, 0, 2, -2,5),
    labels = c(-4, 0, 2, -2,5)) +
  # 坐标title和图形title设置
  labs(
    y = "Log2FC",
    title = paste0("GO plot")
    ) +
  theme_classic() +
  theme(
    plot.title = element_text(size = 20, hjust = 0.5),
    axis.text.x = element_text(size = 16, angle = 45, hjust=1, face = 'bold'), 
    axis.text.y = element_text(size = 16, face = 'bold'),
    axis.title.x = element_blank(),
    axis.title.y = element_text(size = 20, face = 'bold'),
    plot.margin = unit(c(1, 1, 1, 1), "cm")
  ) 
p
