
library(ggpubr)

outFile <- "ggballoonplot2.pdf"
data <- read.table(inputFile, header = T, sep = "\t", check.names = F, row.names = 1)
data <- read.csv('气泡图.csv',row.names = 1, check.names = F)
pdf(file = outFile, width = 9, height = 8)
ggballoonplot(data,
              fill = "value",
              size = "value",
              size.range = c(5, 15)) + 
  
  #可以自己修改配色哦
  scale_fill_gradient2(
    low = "#2E75B6",
    mid = "white",
    high = "#FF0000",
    midpoint = median(as.matrix(data))  
  ) +  
  
  theme_minimal(base_size = 12) + 
  
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
    axis.title = element_text(face = "bold"),
    legend.position = "right",
    legend.key.height = unit(1.5, "cm"),
    plot.title = element_text(hjust = 0.5, size = 16)
  ) +
  
  # 标签
  labs(
    title = "Value Distribution",
    x = "Genes",
    y = "CancerType",
    fill = "Value Scale"
  ) +
  
  guides(size = "none")

dev.off()

