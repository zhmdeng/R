# ========== 加载所需的包 ==========
library(tidyverse)   # 数据处理和绘图（包含 dplyr、tidyr、ggplot2 等）
library(treeio)      # 读取和操作树文件
library(ape)         # 系统发育分析基础包
library(magrittr)    # 提供管道操作符 %>%
library(ggtree)      # 基于 ggplot2 的树可视化包

# ========== 读取并整理 OTU 数据 ==========
otu <- read.delim('otu.xls', row.names = 1) %>%   # 读取 OTU 表，第一列作为行名
  select_if(is.numeric) %>%                        # 只保留数值列（丰度数据）
  rownames_to_column(var = "OTU") %>%              # 将行名转为 OTU 列
  left_join(.,                                     # 左连接：把分类信息合并进来
            read_tsv("otu.xls") %>%                # 再次读取原文件
              select_if(~!is.numeric(.)),          # 只保留非数值列（分类信息）
            by = "OTU") %>%                        # 按 OTU 列连接
  separate(taxonomy,                               # 拆分 taxonomy 列
           into = c("domain","phylum","class","order",
                    "family","genus","species"),   # 分成 7 个分类等级
           sep = ";") %>%                          # 用分号分隔
  mutate_at(vars(c(`domain`:`species`)),           # 对 domain 到 species 的所有列
            ~str_split(., "__", simplify = TRUE)[, 2]) %>%  # 按 "__" 拆分，取第二部分
  column_to_rownames("OTU") %>%                    # 将 OTU 列转回行名
  select(where(is.numeric), phylum) %>%            # 保留数值列和 phylum 列
  head(200)                                        # 只取前 200 行（前 200 个 OTU）

# ========== 构建层次聚类树 ==========
tree <- hclust(dist(otu %>% select(where(is.numeric)),  # 对数值列计算距离矩阵
                    method = "canberra")                # 使用 Canberra 距离
)

# ========== 绘制基础环形树 ==========
ggtree(tree, layout = "circular", branch.length = "none")
# layout="circular"：环形布局
# branch.length="none"：不按枝长比例绘制（所有枝等长）

# ========== 定义函数：绘制分组条带 ==========
draw_strips <- function(p, labels, color) {
  # p：ggtree 绘图对象
  # labels：要添加条带的标签（ASV 名称）向量
  # color：条带颜色
  for (label in labels) {                          # 遍历每个标签
    p <- p + geom_strip(label, label,              # 在同一标签之间画条带
                        extend = 0.5,              # 条带延伸长度
                        color = color,             # 条带颜色
                        offset = 2.1,              # 条带偏移距离
                        barsize = 22,              # 条带宽度
                        alpha = 0.5)               # 透明度
  }
  return(p)                                        # 返回更新后的绘图对象
}

# ========== 提取每个门的 ASV 标签 ==========
df <- otu %>% rownames_to_column(var = "ASV") %>%  # 将行名转为 ASV 列
  select(ASV, phylum)                              # 只保留 ASV 和 phylum 列

# 查看有哪些门（用于确认 phylum 名称）
df %>% pull(phylum) %>% unique()

# 使用 filter 和 pull 提取每个门对应的 ASV 标签
labels_to_group  <- df %>% filter(phylum == "Proteobacteria")     %>% pull(ASV)  # 变形菌门
labels_to_group2 <- df %>% filter(phylum == "Gemmatimonadetes")   %>% pull(ASV)  # 芽单胞菌门
labels_to_group3 <- df %>% filter(phylum == "Actinobacteria")     %>% pull(ASV)  # 放线菌门
labels_to_group4 <- df %>% filter(phylum == "Chloroflexi")        %>% pull(ASV)  # 绿弯菌门
labels_to_group5 <- df %>% filter(phylum == "Acidobacteria")      %>% pull(ASV)  # 酸杆菌门
labels_to_group6 <- df %>% filter(phylum == "Rokubacteria")       %>% pull(ASV)  # Rokubacteria 门

# ========== 创建绘图对象 ==========
p <- ggtree(tree, layout = "circular", branch.length = "none")

# ========== 绘制不同组的条带 ==========
p <- draw_strips(p, labels_to_group,  "#4DBBD5FF")   # 第 1 组：青蓝色
p <- draw_strips(p, labels_to_group2, "#00A087FF")   # 第 2 组：绿色
p <- draw_strips(p, labels_to_group3, "#F39B7FFF")   # 第 3 组：橙色

# ========== 最终绘图 ==========
p + 
  geom_strip("ASV_101653", "ASV_56052",              # 在指定 ASV 之间画条带
             extend = 0.5, color = "yellow",         # 延伸长度、颜色
             offset = 2.1, barsize = 22, alpha = 0.5) +  # 偏移、宽度、透明度
  geom_tiplab(size = 2, color = "black", offset = 3)  # 添加叶节点标签

# ========== 保存图片 ==========
ggsave("ggtree.pdf", width = 5, height = 5)          # 保存为 PDF，尺寸 5×5 英寸