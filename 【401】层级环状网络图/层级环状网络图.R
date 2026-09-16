# ==================== 载入所需的 R 包 ====================
library(ggplot2)      # 画图主力库
library(dplyr)        # 数据整理
library(tidyr)        # 数据转换
library(ggraph)       # 复杂网络绘图
library(tidygraph)    # 网络图数据结构处理
library(reshape2)     # 用于 melt 函数
library(ggnewscale)   # 支持多次使用 scale 系列函数（非必须）

# ==================== 第一步：读取原始数据 ====================
# setwd("/Users/mac/Desktop/PowerBI/sci科研绘图/【401】层级环状网络图/")
df <- read.table("data.txt",
                 sep = "\t",              # 制表符分隔
                 header = TRUE,           # 第一行为列名
                 check.names = FALSE)     # 不修改列名

# ==================== 第二步：构造节点数据 ====================
df_node <- data.frame()                   # 创建空数据框

for (i in 1:(length(df) - 1)) {           # 遍历除最后一列（Value）外的所有列
  group <- colnames(df)[i]                # 当前列名（层级名）
  data <- df[c(i, 5)] %>%                 # 提取当前层级列和 Value 列
    group_by(Level = !!sym(group)) %>%    # 按当前层级的节点名分组
    summarise(size = sum(Value),          # 计算每个节点的 size（Value 之和）
              level = i,                  # 记录层级编号
              .groups = 'drop')           # 不保留分组
  df_node <- bind_rows(df_node, data)     # 合并进主数据框
}

# ==================== 第三步：添加每个节点的 group 分组信息 ====================
df2 <- df[1:4]                            # 取前 4 列（层级）
df2$group <- ifelse(df2$Level2 == "groupA", "g1",   # Level2 为 groupA → g1
                    ifelse(df2$Level2 == "groupB", "g2", "g3"))  # groupB → g2，否则 g3

df3 <- unique(melt(df2, id.vars = "group")[, c(1, 3)])
# 把宽表拍平，提取 group 和对应层级值
df3$group[df3$value == "Network"] <- "g4"  # 中心节点 Network 单独归为 g4
colnames(df3)[2] <- "Level"                # 第二列改名为 Level

df_node <- merge(df_node, unique(df3), by = "Level")  # 把 group 信息合并到节点数据

# ==================== 第四步：构造边数据 ====================
df_edge <- data.frame()                   # 创建空数据框

for (i in 1:(ncol(df) - 2)) {             # 遍历相邻的列对
  tmp <- df[, c(i, i + 1)]                # 从第 i 列连接到第 i+1 列
  colnames(tmp) <- c("from", "to")        # 重命名为 from 和 to
  df_edge <- rbind(df_edge, tmp)          # 合并
}
df_edge <- unique(df_edge)                # 去重

# 为每条边分配颜色类别（c1:黄，c2:蓝，c3:绿）
df_edge$linecolor <- ifelse(grepl("groupA", df_edge$from), "c1",   # 来自 groupA → c1
                            ifelse(grepl("groupB", df_edge$from), "c2", "c3"))  # groupB → c2，其他 → c3

# ==================== 第五步：构造图对象，设置节点样式 ====================
df_graph <- tbl_graph(nodes = df_node,    # 节点数据
                      edges = df_edge,    # 边数据
                      directed = TRUE) %>%  # 有向图
  activate(nodes) %>%                     # 激活节点层
  mutate(
    # 设置节点文字大小（中心最大，sample 最小）
    font_size = case_when(
      Level == "Network" ~ 4.5,           # 中心节点最大
      grepl("^group", Level) ~ 3.5,       # group 层
      grepl("^period", Level) ~ 2.8,      # period 层
      TRUE ~ 2.0                          # sample 层最小
    ),
    # 设置文字是否加粗
    fontface = case_when(
      Level == "Network" ~ "bold",        # 中心节点加粗
      grepl("^group", Level) ~ "bold",    # group 层加粗
      grepl("^period", Level) ~ "bold",   # period 层加粗
      TRUE ~ "plain"                      # sample 层普通
    ),
    # 设置文字向外偏移量
    text_offset = ifelse(grepl("^sample", Level), 1.18, 1.12)
    # sample 层偏移更大，其他层偏移较小
  )

# ==================== 第六步：绘图 ====================
p <- ggraph(df_graph,                     # 图对象
            layout = 'dendrogram',        # 树状布局
            circular = TRUE) +            # 圆形排列
  
  # ---- 1. 绘制边（线） ----
geom_edge_diagonal(aes(color = linecolor),   # 按 linecolor 着色
                   linewidth = 0.6,          # 线宽
                   show.legend = FALSE) +    # 不显示图例
  scale_edge_color_manual(values = c("c1" = "#ffb900",   # 黄
                                     "c2" = "#1aafd0",   # 蓝
                                     "c3" = "#3be8b0")) + # 绿
  
  # ---- 2. 绘制节点（圆点） ----
geom_node_point(aes(color = group,       # 按 group 着色
                    size = size),        # 大小按 size
                show.legend = FALSE) +   # 不显示图例
  scale_color_manual(values = c("g1" = "#ffb900",   # groupA → 黄
                                "g2" = "#1aafd0",   # groupB → 蓝
                                "g3" = "#3be8b0",   # groupC → 绿
                                "g4" = "#fc636b")) + # 中心 → 红
  
  coord_fixed() +                          # 固定横纵比，避免变形
  
  # ---- 3. 绘制文字标签 ----
geom_node_text(
  aes(
    x = text_offset * x,                 # 控制 x 偏移
    y = text_offset * y,                 # 控制 y 偏移
    label = Level,                       # 标签文本
    angle = ifelse(
      grepl("^sample", Level),
      atan2(y, x) * 180 / pi,            # sample 标签沿径向旋转
      0                                  # 其他标签保持水平
    ),
    size = font_size,                    # 字号
    fontface = fontface                  # 字体样式
  ),
  color = "black",                       # 黑色文字
  family = "serif",                      # 衬线字体
  show.legend = FALSE                    # 不显示图例
) +
  
  # ---- 4. 其它样式调整 ----
scale_size_identity() +                  # 使用原始 size 值
  scale_size_continuous(range = c(2, 28)) +  # 控制节点大小范围
  theme_void()                             # 空白背景，去除坐标轴

# ==================== 第七步：保存图片 ====================
ggsave("network_tree_final.pdf", p,        # 保存 p
       width = 8, height = 8,              # 8×8 英寸
       dpi = 300,                          # 分辨率 300 dpi
       bg = "white")                       # 白色背景