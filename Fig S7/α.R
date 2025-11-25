
rm(list=ls())#clear Global Environment
#加载包
library(ggplot2)
library(ggpubr)
library(ggsignif)
library(ggprism)
library(vegan)
library(picante)
library(dplyr)
library(RColorBrewer)



##导入数据，所需是数据行名为样本名、列名为OTUxxx的数据表
df <- read.table("otu.txt",header = T, row.names = 1, check.names = F)
#使用vegan包计算多样性指数
Shannon <- diversity(df, index = "shannon", MARGIN = 2, base = exp(1))
Simpson <- diversity(df, index = "simpson", MARGIN = 2, base =  exp(1))
Richness <- specnumber(df, MARGIN = 2)#spe.rich =sobs
###将以上多样性指数统计成表格
index <- as.data.frame(cbind(Shannon, Simpson, Richness))
tdf <- t(df)#转置表格
tdf<-ceiling(as.data.frame(t(df)))
#计算obs，chao，ace指数
obs_chao_ace <- t(estimateR(tdf))
obs_chao_ace <- obs_chao_ace[rownames(index),]#统一行名
#将obs，chao，ace指数与前面指数计算结果进行合并
index$Chao <- obs_chao_ace[,2]
index$ACE <- obs_chao_ace[,4]
index$obs <- obs_chao_ace[,1]
#计算Pielou及覆盖度
index$Pielou <- Shannon / log(Richness, 2)
index$Goods_coverage <- 1 - colSums(df ==1) / colSums(df)
#导出表格
write.table(cbind(sample=c(rownames(index)),index),'diversity.index.txt', row.names = F, sep = '\t', quote = F)

#读入文件
index <- read.delim('diversity.index.txt', header = T, row.names = 1)
##figure:take shannon for example
index$samples <- rownames(index)#将样本名写到文件中
#读入分组文件
groups <- read.delim('group.txt',header = T, stringsAsFactors = F)
colnames(groups)[1:2] <- c('samples','group')#改列名
#合并分组信息与多样性指数
df2 <- merge(index,groups,by = 'samples')



#Shannon
p1 <- ggplot(df2,aes(x=group,y=Shannon))+#指定数据
  stat_boxplot(geom = "errorbar", width=0.1,size=0.8)+#添加误差线,注意位置，放到最后则这条线不会被箱体覆盖
  geom_boxplot(aes(fill=group), #绘制箱线图函数
               outlier.colour="white",size=0.8)+#异常点去除
  theme(panel.background =element_blank(), #背景
        axis.line=element_line(),#坐标轴的线设为显示
        plot.title = element_text(size=14))+#图例位置
  # scale_fill_manual(values=c("#ffc000","#a68dc8","blue"))+#指定颜色
  geom_jitter(width = 0.2)+#添加抖动点
  geom_signif(comparisons = list(c("A","B"),
                                 c("A","C"),
                                 c("A","D"),
                                 c("B","C"),
                                 c("B","D"),
                                 c("C","D")),# 设置需要比较的组
              map_signif_level = T, #是否使用星号显示
              test = t.test, ##计算方法
              y_position = c(3.35,3.5,3.75,4.5,4.75,5),#图中横线位置 设置
              tip_length = c(c(0,0),
                             c(0,0),
                             c(0,0),
                             c(0,0),
                             c(0,0),
                             c(0,0)),#横线下方的竖线设置
              size=0.8,color="black")+
  theme_prism(palette = "candy_bright",
              base_fontface = "plain", # 字体样式，可选 bold, plain, italic
              base_family = "serif", # 字体格式，可选 serif, sans, mono, Arial等
              base_size = 16,  # 图形的字体大小
              base_line_size = 0.8, # 坐标轴的粗细
              axis_text_angle = 45)+ # 可选值有 0，45，90，270
  scale_fill_prism(palette = "candy_bright")+
  theme(legend.position = 'none')#去除图例
p1

#Simpson
p2 <- ggplot(df2,aes(x=group,y=Simpson))+#指定数据
  stat_boxplot(geom = "errorbar", width=0.1,size=0.8)+#添加误差线,注意位置，放到最后则这条线不会被箱体覆盖
  geom_boxplot(aes(fill=group), #绘制箱线图函数
               outlier.colour="white",size=0.8)+#异常点去除
  theme(panel.background =element_blank(), #背景
        axis.line=element_line(),#坐标轴的线设为显示
        plot.title = element_text(size=14))+#图例位置
  # scale_fill_manual(values=c("#ffc000","#a68dc8","blue"))+#指定颜色
  geom_jitter(width = 0.2)+#添加抖动点
  geom_signif(comparisons = list(c("A","B"),
                                 c("A","C"),
                                 c("A","D"),
                                 c("B","C"),
                                 c("B","D"),
                                 c("C","D")),# 设置需要比较的组
              map_signif_level = T, #是否使用星号显示
              test = t.test, ##计算方法
              y_position = c(0.75,0.775,0.8,1,1.05,1.125),#图中横线位置 设置
              tip_length = c(c(0,0),
                             c(0,0),
                             c(0,0),
                             c(0,0),
                             c(0,0),
                             c(0,0)),#横线下方的竖线设置
              size=0.8,color="black")+
  theme_prism(palette = "candy_bright",
              base_fontface = "plain", # 字体样式，可选 bold, plain, italic
              base_family = "serif", # 字体格式，可选 serif, sans, mono, Arial等
              base_size = 16,  # 图形的字体大小
              base_line_size = 0.8, # 坐标轴的粗细
              axis_text_angle = 45)+ # 可选值有 0，45，90，270
  scale_fill_prism(palette = "candy_bright")+
  theme(legend.position = 'none')#去除图例
p2

#Ace
p3 <- ggplot(df2,aes(x=group,y=ACE))+#指定数据
  stat_boxplot(geom = "errorbar", width=0.1,size=0.8)+#添加误差线,注意位置，放到最后则这条线不会被箱体覆盖
  geom_boxplot(aes(fill=group), #绘制箱线图函数
               outlier.colour="white",size=0.8)+#异常点去除
  theme(panel.background =element_blank(), #背景
        axis.line=element_line(),#坐标轴的线设为显示
        plot.title = element_text(size=14))+#图例位置
  # scale_fill_manual(values=c("#ffc000","#a68dc8","blue"))+#指定颜色
  geom_jitter(width = 0.2)+#添加抖动点
  geom_signif(comparisons = list(c("A","B"),
                                 c("A","C"),
                                 c("A","D"),
                                 c("B","C"),
                                 c("B","D"),
                                 c("C","D")),# 设置需要比较的组
              map_signif_level = T, #是否使用星号显示
              test = t.test, ##计算方法
              y_position = c(500,520,540,560,580,600),#图中横线位置 设置
              tip_length = c(c(0,0),
                             c(0,0),
                             c(0,0),
                             c(0,0),
                             c(0,0),
                             c(0,0)),#横线下方的竖线设置
              size=0.8,color="black")+
  theme_prism(palette = "candy_bright",
              base_fontface = "plain", # 字体样式，可选 bold, plain, italic
              base_family = "serif", # 字体格式，可选 serif, sans, mono, Arial等
              base_size = 16,  # 图形的字体大小
              base_line_size = 0.8, # 坐标轴的粗细
              axis_text_angle = 45)+ # 可选值有 0，45，90，270
  scale_fill_prism(palette = "candy_bright")+
  theme(legend.position = 'none')#去除图例
p3

#Chao
p4 <- ggplot(df2,aes(x=group,y=Chao))+#指定数据
  stat_boxplot(geom = "errorbar", width=0.1,size=0.8)+#添加误差线,注意位置，放到最后则这条线不会被箱体覆盖
  geom_boxplot(aes(fill=group), #绘制箱线图函数
               outlier.colour="white",size=0.8)+#异常点去除
  theme(panel.background =element_blank(), #背景
        axis.line=element_line(),#坐标轴的线设为显示
        plot.title = element_text(size=14))+#图例位置
  # scale_fill_manual(values=c("#ffc000","#a68dc8","blue"))+#指定颜色
  geom_jitter(width = 0.2)+#添加抖动点
  geom_signif(comparisons = list(c("A","B"),
                                 c("A","C"),
                                 c("A","D"),
                                 c("B","C"),
                                 c("B","D"),
                                 c("C","D")),# 设置需要比较的组
              map_signif_level = T, #是否使用星号显示
              test = t.test, ##计算方法
              y_position = c(500,520,540,560,580,600),#图中横线位置 设置
              tip_length = c(c(0,0),
                             c(0,0),
                             c(0,0),
                             c(0,0),
                             c(0,0),
                             c(0,0)),#横线下方的竖线设置
              size=0.8,color="black")+
  theme_prism(palette = "candy_bright",
              base_fontface = "plain", # 字体样式，可选 bold, plain, italic
              base_family = "serif", # 字体格式，可选 serif, sans, mono, Arial等
              base_size = 16,  # 图形的字体大小
              base_line_size = 0.8, # 坐标轴的粗细
              axis_text_angle = 45)+ # 可选值有 0，45，90，270
  scale_fill_prism(palette = "candy_bright")+
  theme(legend.position = 'none')#去除图例
p4


library("gridExtra")
library("cowplot")
plot_grid(p1,p2,p3,p4, labels=c('A','B','C','D'), ncol=2, nrow=2)#拼图及标注
ggsave('用来看显著性的.pdf',width=12,height = 12)




#另一个方法
library(ggplot2)
library(ggthemes)
library(tidyverse)
library(agricolae)
library(car)
library(reshape2)

##导入数据
df <- read.table("index.txt",header = T, row.names = 1, check.names = F)
df$group <- factor(df$group,levels = c('KG','SG'))#记得改分组
head(df)

#是否符合正态分布#
qqPlot(lm(df$ACE ~ df$group, data=df), 
       simulate=TRUE, main="Q-Q Plot", lables=FALSE)


##正态检验
shapiro.test(df$ACE)
# Bartlett检验
bartlett.test(df$ACE ~ df$group, data=df)
# Levene检验,对原始数据的正态性不敏感
leveneTest(df$ACE ~ df$group, data=df)


model<-aov(ACE ~ group, data=df)


out <- LSD.test(model,"group", p.adj="none")
grou<- group_by(df,group)
bar_data <- summarise(grou,sd(ACE,na.rm = T))#计算误差
#整理数据
bar_data2 <- merge(bar_data ,out$group,by.x="group",by.y = "row.names",all = F)#合并数据
bar_data2<-bar_data2[order(bar_data2$group),]

#定义label
label<-bar_data2$groups
#绘图
p1<-ggplot(df,aes(group,ACE,fill=group))+
  stat_boxplot(geom = "errorbar", width=0.1)+
  geom_boxplot(position="dodge")+
  ggtitle("ACE")+
  theme_wsj()+
  scale_fill_manual(values=c("#1f77b4", "#aec7e8", "#ff7f0e", "#ffbb78", "#98df8a", "#d62728", "#ff9896", 
                             "#9467bd", "#8c564b", "#c49c94", "#e377c2", "#f7b6d2", "#c7c7c7", "#bcbd22", 
                             "#dbdb8d", "#17becf", "#9edae5", "yellow"))+#指定颜色
  guides(fill=guide_legend(title=NULL)) +
  theme(legend.position = 'none',
        axis.text.x = element_text(angle = 45,vjust = 1,hjust = 1))+
  geom_text(data=bar_data2,aes(x=group,y=ACE+sd(ACE,na.rm=T)+20,label=label))+#添加字母标记
  geom_jitter(alpha=0.8,width = 0.2,size=1)#添加抖动点
p1


#是否符合正态分布#
qqPlot(lm(df$Chao ~ df$group, data=df), 
       simulate=TRUE, main="Q-Q Plot", lables=FALSE)
##正态检验
shapiro.test(df$Chao)
bartlett.test(df$Chao ~ df$group, data=df)
# Levene检验,对原始数据的正态性不敏感
leveneTest(df$Chao ~ df$group, data=df)
######方差检验
model<-aov(Chao ~ group, data=df)
#进行多重比较，不矫正P值
out <- LSD.test(model,"group", p.adj="none")#结果显示：标记字母法out$group
grou<- group_by(df,group)
bar_data <- summarise(grou,sd(Chao,na.rm = T))#计算误差
#整理数据
bar_data2 <- merge(bar_data ,out$group,by.x="group",by.y = "row.names",all = F)#合并数据
bar_data2<-bar_data2[order(bar_data2$group),]

#定义label
label<-bar_data2$groups
#绘图
p2<-ggplot(df,aes(group,Chao,fill=group))+
  stat_boxplot(geom = "errorbar", width=0.1)+
  geom_boxplot(position="dodge")+
  ggtitle("Chao")+
  theme_wsj()+
  scale_fill_manual(values=c("#1f77b4", "#aec7e8", "#ff7f0e", "#ffbb78", "#98df8a", "#d62728", "#ff9896", 
                             "#9467bd", "#8c564b", "#c49c94", "#e377c2", "#f7b6d2", "#c7c7c7", "#bcbd22", 
                             "#dbdb8d", "#17becf", "#9edae5", "yellow"))+#指定颜色
  guides(fill=guide_legend(title=NULL)) +
  theme(legend.position = 'none',
        axis.text.x = element_text(angle = 45,vjust = 1,hjust = 1))+
  geom_text(data=bar_data2,aes(x=group,y=Chao+sd(Chao,na.rm=T)+20,label=label))+#添加字母标记
  geom_jitter(alpha=0.8,width = 0.2,size=1)#添加抖动点
p2


#是否符合正态分布
qqPlot(lm(df$Shannon ~ df$group, data=df), 
       simulate=TRUE, main="Q-Q Plot", lables=FALSE)
##正态检验
shapiro.test(df$Shannon)
bartlett.test(df$Shannon ~ df$group, data=df)
# Levene检验,对原始数据的正态性不敏感
leveneTest(df$Shannon ~ df$group, data=df)
######方差检验
model<-aov(Shannon ~ group, data=df)
#进行多重比较，不矫正P值
out <- LSD.test(model,"group", p.adj="none")
grou<- group_by(df,group)
bar_data <- summarise(grou,sd(Shannon,na.rm = T))#计算误差
#整理数据
bar_data2 <- merge(bar_data ,out$group,by.x="group",by.y = "row.names",all = F)#合并数据
bar_data2<-bar_data2[order(bar_data2$group),]
#定义label
label<-bar_data2$groups
#绘图
p3<-ggplot(df,aes(group,Shannon,fill=group))+
  stat_boxplot(geom = "errorbar", width=0.1)+
  geom_boxplot(position="dodge")+
  ggtitle("Shannon")+
  theme_wsj()+
  scale_fill_manual(values=c("#1f77b4", "#aec7e8", "#ff7f0e", "#ffbb78", "#98df8a", "#d62728", "#ff9896", 
                             "#9467bd", "#8c564b", "#c49c94", "#e377c2", "#f7b6d2", "#c7c7c7", "#bcbd22", 
                             "#dbdb8d", "#17becf", "#9edae5", "yellow"))+#指定颜色
  guides(fill=guide_legend(title=NULL)) +
  theme(legend.position = 'none',
        axis.text.x = element_text(angle = 45,vjust = 1,hjust = 1))+
  geom_text(data=bar_data2,aes(x=group,y=Shannon+0.3,label=label))+#添加字母标记
  geom_jitter(alpha=0.8,width = 0.2,size=1)#添加抖动点
p3


#是否符合正态分布#
qqPlot(lm(df$Simpson ~ df$group, data=df), 
       simulate=TRUE, main="Q-Q Plot", lables=FALSE)
##正态检验
shapiro.test(df$Simpson)
bartlett.test(df$Simpson ~ df$group, data=df)
# Levene检验,对原始数据的正态性不敏感
leveneTest(df$Simpson ~ df$group, data=df)
####方差检验
model<-aov(Simpson ~ group, data=df)
#进行多重比较，不矫正P值
out <- LSD.test(model,"group", p.adj="none")
grou<- group_by(df,group)
bar_data <- summarise(grou,sd(Simpson,na.rm = T))#计算误差
#整理数据
bar_data2 <- merge(bar_data ,out$group,by.x="group",by.y = "row.names",all = F)#合并数据
bar_data2<-bar_data2[order(bar_data2$group),]
#定义label
label<-bar_data2$groups
#绘图
p4<-ggplot(df,aes(group,Simpson,fill=group))+
  stat_boxplot(geom = "errorbar", width=0.1)+
  geom_boxplot(position="dodge")+
  ggtitle("Simpson")+
  theme_wsj()+
  scale_fill_manual(values=c("#1f77b4", "#aec7e8", "#ff7f0e", "#ffbb78", "#98df8a", "#d62728", "#ff9896", 
                             "#9467bd", "#8c564b", "#c49c94", "#e377c2", "#f7b6d2", "#c7c7c7", "#bcbd22", 
                             "#dbdb8d", "#17becf", "#9edae5", "yellow"))+#指定颜色
  guides(fill=guide_legend(title=NULL)) +
  theme(legend.position = 'none',
        axis.text.x = element_text(angle = 45,vjust = 1,hjust = 1))+
  geom_text(data=bar_data2,aes(x=group,y=Simpson+0.05,label=label))+#添加字母标记
  geom_jitter(alpha=0.8,width = 0.2,size=1)#添加抖动点
p4



#拼图
library(patchwork)
(p1+p2)/(p3+p4)
