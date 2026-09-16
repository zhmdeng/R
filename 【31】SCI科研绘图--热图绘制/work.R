# ==================== 加载所需的包 ====================
library(tidyverse)   # 数据处理和绘图
library(ggtree)      # 聚类树可视化
library(aplot)       # 用于在图形边缘插入子图
setwd("/Users/mac/Desktop/PowerBI/R_learning/figures/【31】SCI科研绘图--热图绘制/")
# ==================== 定义颜色 ====================
color <- c("grey70", "#709AE1FF")   # 定义两种颜色：灰色和蓝色

# ==================== 绘制热图 ====================
heatmap <- read_tsv("data.txt") %>%              # 读取数据
  select(-1) %>%                                 # 删除第 1 列（假设是索引列）
  pivot_longer(-Gene) %>%                        # 宽转长：除 Gene 外的列变成长表
  mutate(type = case_when(value == 0 ~ "Absence",       # value = 0 → Absence
                          value == 1 ~ "presence")) %>% # value = 1 → presence
  ggplot(aes(Gene, name, fill = type)) +         # x=Gene，y=name，填充色按 type 映射
  geom_tile(fill = "white", size = 0.5) +        # 白底方块（tile）
  # geom_tile() 画矩形，这里用白色填充作为底色
  # size = 0.5 是边框线宽
  geom_point(pch = 22, size = 5) +               # 叠加点，pch=22 是方形，size=5
  # pch = 22 是带边框的方形（可填充）
  labs(x = NULL, y = NULL, color = NULL) +       # 不显示轴标签和图例标题
  scale_color_manual(values = color) +           # 手动指定颜色（边框色）
  scale_fill_manual(values = color) +            # 手动指定颜色（填充色）
  scale_x_discrete(expand = c(0, 0)) +           # x 轴不留空白
  theme(
    axis.text.x = element_text(color = "black",  # x 轴文字：黑色
                               angle = 90,       # 旋转 90 度
                               size = 8,         # 字号 8
                               hjust = 1,        # 水平对齐
                               vjust = 0.5),     # 垂直对齐
    axis.text.y = element_blank(),               # y 轴文字不显示
    axis.ticks.x = element_blank(),              # x 轴刻度不显示
    axis.ticks.y = element_blank(),              # y 轴刻度不显示
    panel.border = element_rect(fill = NA,       # 面板边框：无填充
                                color = "grey80",# 边框颜色灰色
                                size = 1,        # 边框线宽
                                linetype = "solid"),  # 实线
    legend.title = element_blank(),              # 图例标题不显示
    plot.margin = unit(c(0.2, 0.2, 0.2, 0.2),    # 图形边距：上、右、下、左
                       units = "cm")             # 单位：厘米
  )

# ==================== 准备聚类数据 ====================
df <- read_tsv("data.txt") %>%                   # 读取数据
  select(-1) %>%                                 # 删除第 1 列
  column_to_rownames(var = "Gene") %>%           # 把 Gene 列转为行名
  t() %>%                                        # 转置：行=样本，列=基因
  as.data.frame()                                # 转为数据框

# ==================== 读取分组信息 ====================
group <- read_tsv("group.txt")                   # 读取分组文件

# ==================== 构建行聚类树（样本方向） ====================
phr <- hclust(dist(df)) %>%                      # 对样本做层次聚类
  ggtree(layout = "rectangular",                 # 矩形布局
         branch.length = "none") %<+% group +    # 不按枝长比例绘制；关联分组信息
  # %<+% 是 ggtree 的语法，把分组信息添加到树的叶节点
  geom_tippoint(aes(shape = Group,               # 叶节点散点：形状按 Group
                    color = Group),              # 颜色按 Group
                size = 3) +                      # 点大小 3
  theme(legend.title = element_blank(),          # 图例标题不显示
        legend.key = element_blank(),            # 图例键背景透明
        legend.text = element_text(color = "black",  # 图例文字黑色
                                   size = 10))   # 字号 10

# ==================== 构建列聚类树（基因方向） ====================
phc <- hclust(dist(t(df))) %>%                   # 对基因做层次聚类
  ggtree() +                                     # 绘制树
  layout_dendrogram()                            # 转为树状图布局
# layout_dendrogram() 是 aplot 的函数，让树显示为无标签的树状图

# ==================== 组合图形 ====================
heatmap %>%
  insert_left(phr, width = .6) %>%               # 在热图左侧插入行聚类树
  # width = .6 表示聚类树占热图宽度的 0.6
  insert_top(phc, height = 0.08)                 # 在热图顶部插入列聚类树
# height = 0.08 表示列聚类树占热图高度的 0.08

ggsave("热图.pdf",width = 5,height = 5,dpi = 300)
