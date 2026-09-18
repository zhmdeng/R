rm(list = ls())   # 清空环境
# setwd("")       # 设置工作目录（已注释）

# ==================== 加载 R 包 ====================
library(MicrobiotaProcess)  # 微生物组 tidy 分析框架，提供 as.MPSE()、mp_extract_tree()、td_filter()
library(dplyr)              # 数据处理
library(ggplot2)            # 绘图
library(phyloseq)           # 微生物组数据结构
library(ggtree)             # 树可视化，提供 ggtree()、geom_hilight()、geom_tiplab()
library(phyloseq)           # ❌ 重复加载
library(MicrobiotaProcess)  # ❌ 重复加载

# ⚠️ 加载顺序有问题：MicrobiotaProcess 先加载，phyloseq 后加载。
# MicrobiotaProcess 也定义了 tax_table 等 S4 方法，可能覆盖 phyloseq 的版本。
# 建议把 library(phyloseq) 放在最前面，再加载 MicrobiotaProcess。

# ==================== 读取数据 ====================
sample <- read.table("sample.txt", check.names = F, row.names = 1, header = 1, sep = "\t")
# 读取样本元数据：行名是样本名，列是分组等变量
OTU <- read.table("otu.txt", check.names = F, row.names = 1, header = 1, sep = "\t")
# 读取 OTU 丰度表：行=OTU，列=样本
Tax <- read.table("tax.txt", check.names = F, row.names = 1, header = 1, sep = "\t")
# 读取分类表：行=OTU，列=界门纲目科属种

# ==================== 构建 phyloseq 对象 ====================
ps <- phyloseq(
  sample_data(sample),                               # 样本元数据
  otu_table(as.matrix(OTU), taxa_are_rows = TRUE),   # OTU 表，行是 OTU
  phyloseq::tax_table(as.matrix(Tax))                # 分类表（显式指定 phyloseq:: 避免冲突）
)
ps

# ==================== 转为 MPSE 对象 ====================
df <- ps %>% as.MPSE()   # 把 phyloseq 对象转为 MicrobiotaProcess 的 MPSE 对象
df

# ==================== 提取分类树 ====================
taxa.tree <- df %>%
  mp_extract_tree(type = "taxatree")   # 提取分类层级树
taxa.tree

# ==================== 默认布局：矩形树 ====================
ggtree(taxa.tree, linewidth = 0.6, color = "black", size = 0.3) +
  # 用 ggtree 绘制，矩形布局（默认）
  geom_tiplab(size = 2, offset = 0.1) +              # 叶节点标签，字号 2，向右偏移 0.1
  geom_point(data = td_filter(!isTip),               # 只对非叶节点画散点
             fill = "white", size = 2, shape = 21) +  # 白底黑边圆点
  geom_hilight(                                       # 高亮（色块）
    data = td_filter(nodeClass == "Phylum"),          # 只对门级节点高亮
    mapping = aes(node = node, fill = label)) +       # 按节点和标签填充
  scale_fill_manual(                                  # 自定义填充色
    values = c("#3be8b0", "#1aafd0", "#6a67ce", "#ffb900", "#fc636b"),
    guide = guide_legend(keywidth = 1, keyheight = 1),
    name = "Phylum")

# ==================== 门水平：径向树 ====================
ggtree(taxa.tree, layout = "radial", linewidth = 0.6, color = "black", size = 0.3) +
  # layout="radial" 改成径向布局
  geom_tiplab(size = 3, offset = 0.1) +               # 标签字号改为 3
  geom_point(data = td_filter(!isTip), fill = "white", size = 2, shape = 21) +
  geom_hilight(data = td_filter(nodeClass == "Phylum"),  # 高亮门级节点
               mapping = aes(node = node, fill = label)) +
  scale_fill_manual(values = c("#3be8b0", "#1aafd0", "#6a67ce", "#ffb900", "#fc636b"),
                    guide = guide_legend(keywidth = 1, keyheight = 1),
                    name = "Phylum")

# ==================== 纲水平：径向树 ====================
ggtree(taxa.tree, layout = "radial", linewidth = 0.6, color = "black", size = 0.3) +
  geom_tiplab(size = 3, offset = 0.1) +
  geom_point(data = td_filter(!isTip), fill = "white", size = 2, shape = 21) +
  geom_hilight(data = td_filter(nodeClass == "Class"),   # 高亮纲级节点
               mapping = aes(node = node, fill = label)) +
  labs(fill = "Class")                                  # 图例标题改为 Class

# ==================== 又一张门水平径向树 ====================
ggtree(taxa.tree, layout = "radial", linewidth = 0.6, color = "black", size = 0.3) +
  geom_tiplab(size = 3, offset = 0.1) +
  geom_point(data = td_filter(!isTip), fill = "white", size = 2, shape = 21) +
  geom_hilight(data = td_filter(nodeClass == "Phylum"),  # 高亮门级
               mapping = aes(node = node, fill = label)) +
  scale_fill_manual(values = c("#3be8b0", "#1aafd0", "#6a67ce", "#ffb900", "#fc636b"),
                    guide = guide_legend(keywidth = 1, keyheight = 1),
                    name = "Phylum")

# ==================== 门水平：环形树 ====================
ggtree(taxa.tree, layout = "circular", linewidth = 0.6, color = "black", size = 0.3) +
  # layout="circular" 改成环形布局
  geom_tiplab(size = 3, offset = 0.1) +
  geom_point(data = td_filter(!isTip), fill = "white", size = 2, shape = 21) +
  geom_hilight(data = td_filter(nodeClass == "Phylum"),
               mapping = aes(node = node, fill = label)) +
  scale_fill_manual(values = c("#3be8b0", "#1aafd0", "#6a67ce", "#ffb900", "#fc636b"),
                    guide = guide_legend(keywidth = 1, keyheight = 1),
                    name = "Phylum")

# ⚠️ 注释提到"导出图片后可以将背景色块置于底层"，说明 geom_hilight 的色块可能遮住
# 其他元素。如果发现标签或点被色块盖住，需要在绘图时调整图层顺序，或导出到 AI/PS 处理。

# ==================== 保存 ====================
ggsave('图.pdf', dpi = 300)
# ⚠️ 只保存最后一次绘制的图（即 circular 那张），前面所有图都不会被保存