library(MicrobiotaProcess) # A comprehensive R package for managing and analyzing microbiome and other ecological data within the tidy framework
library(dplyr) # A Grammar of Data Manipulation
library(ggplot2) # Create Elegant Data Visualisations Using the Grammar of Graphics
library(phyloseq) # Handling and analysis of high-throughput microbiome census data
library(ggtree) # an R package for visualization of tree and annotation data



sample <- read.table("sample.txt",check.names = F, row.names = 1, header = 1, sep = "\t")
OTU<- read.table("Total.txt",check.names = F, row.names = 1, header = 1, sep = "\t")
Tax <- read.table("tax.txt",check.names = F, row.names = 1, header = 1)



ps <- phyloseq(sample_data(sample),
               otu_table(as.matrix(OTU), taxa_are_rows=TRUE), 
               tax_table(as.matrix(Tax)))
ps


df <- ps %>% as.MPSE()
df


taxa.tree <- df %>% 
  mp_extract_tree(type="taxatree")
taxa.tree

ggtree(
  taxa.tree,
  linewidth=0.6,
  color = "black",
  size = 0.3) +
  geom_tiplab(size=2, offset=0.1)+
  geom_point(data = td_filter(!isTip),
             fill="white",
             size=2,
             shape=21)+
  geom_hilight( 
    data = td_filter(nodeClass == "Kingdom"),
    mapping = aes(node = node, fill = label))+
  scale_fill_manual(
    values=c("#3be8b0", "#1aafd0", "#6a67ce","#ffb900","#fc636b"),
    guide=guide_legend(keywidth=1, keyheight=1),
    name="Kingdom")

ggtree(
  taxa.tree,
  layout="radial",
  linewidth=0.6,
  color = "black",
  size = 0.3) +
  geom_tiplab(size=3, offset=0.1)+
  geom_point(data = td_filter(!isTip),
             fill="white",
             size=2,
             shape=21)+
  geom_hilight( 
    data = td_filter(nodeClass == "Kingdom"),
    mapping = aes(node = node, fill = label))+
  scale_fill_manual(
    values=c("#3be8b0", "#1aafd0", "#6a67ce","#ffb900","#fc636b","#e5086a","#80a738","#1c85f0"),
    guide=guide_legend(keywidth=1, keyheight=1),
    name="Kingdom")


library(ape)  # 确保加载ape包

# 检查 taxa.tree 是否是 phylo 对象
if (!inherits(taxa.tree, "phylo")) {
  # 如果 taxa.tree 不是 phylo 对象，可能需要转换，具体取决于你的数据结构
  # 假设你已经知道如何将其转换为 phylo
  taxa.tree <- as.phylo(taxa.tree)  # 你需要根据具体情况进行调整
}

# 导出为 .nwk 文件
write.tree(taxa.tree, file="taxa_tree.nwk")
