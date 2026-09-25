# ==================== 加载包 ====================
library(tidyverse)   # 数据处理和绘图
# install.packages("ggdendro")   # 安装 ggdendro（已注释）
library(ggdendro)    # 提取聚类树数据
library(aplot)       # 用于在图形边缘插入子图

# ==================== 读取并整理数据 ====================
df <- read_tsv("data.xls") %>%              # 读取数据
  column_to_rownames(var = "Pathway")       # Pathway 列转为行名
# 结果：行=通路，列=样本，值=丰度/表达量

df2 <- df %>%
  rownames_to_column(var = "Pathway") %>%   # 行名转为 Pathway 列
  pivot_longer(-Pathway)                    # 宽转长：除 Pathway 外都转成长表
# 结果：Pathway、name、value 三列

# ==================== 聚类分析 ====================
hrdata <- hclust(dist(df)) %>%              # 对行（通路）做层次聚类
  dendro_data(., type = "rectangle")        # 提取树状图数据（矩形布局）

hcdata <- hclust(dist(t(df))) %>%           # 对列（样本）做层次聚类
  dendro_data(., type = "rectangle")        # 提取树状图数据

# ==================== 固定因子顺序 ====================
df2$Pathway <- factor(
  df2$Pathway,
  levels = hrdata$labels %>% select(label) %>% pull()
)
# 按行聚类结果重新排列 Pathway 的顺序

df2$name <- factor(
  df2$name,
  levels = hcdata$labels %>% select(label) %>% pull()
)
# 按列聚类结果重新排列 name（样本）的顺序

# ==================== 绘制热图 ====================
heatmap <- df2 %>%
  ggplot(aes(name, Pathway, fill = value)) +   # x=name，y=Pathway，填充色按 value
  geom_tile() +                                # 绘制热图方块
  labs(x = NULL, y = NULL) +                   # 不显示轴标签
  scale_y_discrete(expand = c(0, 0),           # y 轴不留空白
                   position = "left") +        # y 轴在左侧
  scale_x_discrete(expand = c(0, 0)) +         # x 轴不留空白
  scale_fill_gradientn(
    colours = rev(RColorBrewer::brewer.pal(11, "RdBu"))
  ) +                                          # 颜色：RdBu 配色反转
  theme(
    axis.text.x = element_blank(),             # x 轴文字不显示
    axis.text.y = element_blank(),             # y 轴文字不显示
    axis.ticks = element_blank(),              # 不显示刻度
    legend.title = element_blank(),            # 图例标题不显示
    legend.text = element_text(size = 8,       # 图例文字字号 8
                               color = "black") # 黑色
  ) +
  guides(fill = guide_colorbar(                # 图例设置
    direction = "vertical",                    # 垂直方向
    reverse = F,                               # 不反转
    barwidth = unit(.5, "cm"),                 # 图例条宽
    barheight = unit(10, "cm")                 # 图例条高
  ))

# ==================== 绘制分组注释 ====================
group1 <- read_tsv("group.xls") %>%            # 读取分组数据
  mutate(type = "A") %>%                       # 添加一列 type，值全为 "A"
  ggplot(aes(Sample_id, type, fill = `Subtype-1`)) +  # x=样本，y=type，填充色按亚型
  geom_tile() +                                # 方块
  scale_fill_manual(values = c("#3B9AB2", "#78B7C5")) +  # 手动指定 2 种颜色
  theme_void() +                               # 空主题
  theme(
    legend.title = element_blank(),            # 图例标题不显示
    legend.spacing.x = unit(0.1, 'cm'),        # 图例水平间距
    legend.key.width = unit(0.5, 'cm'),        # 图例键宽度
    legend.key.height = unit(0.5, 'cm'),       # 图例键高度
    legend.background = element_blank(),       # 图例背景透明
    legend.text = element_text(size = 8,       # 图例文字字号 8
                               color = "black") # 黑色
  )

# ==================== 组合图形 ====================
heatmap %>%
  insert_top(group1, height = 0.04)
# 在热图顶部插入分组注释，高度占热图高度的 0.04
# 结果：分组注释显示在热图上方，与热图的 x 轴对齐

# ==================== 保存图片 ====================
ggsave('热图.pdf', width = 6, height = 8, dpi = 300)