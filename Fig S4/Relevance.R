
#读取数据
library(ggplot2)
library(ggthemes)
df1 <- read.csv("top200H.csv",header = T)
df2 <- read.csv("top200H.csv",header = T)
#计算相关性
library(psych)
cor.result<-corr.test(df1,df2,method = "pearson")
cor.result$p
cor.result$r



r.cor<-data.frame(cor.result$r) #根据两组数据来定
p.cor<-data.frame(cor.result$p) #根据两组数据来定
r.cor[p.cor>0.05] <- 0#相关性不显著的相关系数为0



###将数据转换为长数据格式，进行合并并添加连接属性
r.cor$from = rownames(r.cor)
p.cor$from = rownames(p.cor)
library(tidyr) # 加载 tidyr 库

p_value <-  p.cor %>% 
  gather(key = "to", value = "p", -from) %>%
  data.frame()
edg<- r.cor %>% 
  gather(key = "to", value = "r", -from) %>%
  data.frame() %>%
  left_join(p_value, by=c("from","to")) %>%
  mutate(
    linecolor = ifelse(r > 0,"positive","negative"),
    linesize = abs(r)) %>%
  filter(linesize>0)# 可以选择过滤掉较低的相关性以简化网络



# 导出边文件
write.csv(edg, "edges.csv", row.names = FALSE)

# 导出节点文件
write.csv(node, "nodes.csv", row.names = FALSE)


