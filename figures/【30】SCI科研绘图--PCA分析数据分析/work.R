# ==================== 加载所需的包 ====================
pacman::p_load(tidyverse,       # 数据处理和绘图（含 ggplot2、dplyr、readr 等）
               ggrepel,         # 让标签避免重叠
               FactoMineR,      # 提供 PCA 等多元统计分析
               magrittr,        # 提供管道操作符 %>%
               factoextra,      # 基于 ggplot2 的 PCA 可视化
               RColorBrewer)    # 提供配色方案
# pacman::p_load() 是 pacman 包的函数
# 它会自动检查包是否安装，没装就安装，装了就加载
# 相当于 library() 的增强版

# ==================== 读取数据 ====================
df <- read_tsv("F3.xls")   # 读取制表符分隔的数据文件

# ==================== 进行 PCA 分析 ====================
pca <- df %>%
  column_to_rownames(var = "Sample_id") %>%   # 把 Sample_id 列转为行名
  select(-Subtype) %>%                        # 删除 Subtype 列（分类变量不参与 PCA）
  prcomp(., scale. = TRUE)                    # 对数据做 PCA
# prcomp() 是基础 R 的主成分分析函数
# scale. = TRUE 表示对变量做标准化（Z-score）
# . 表示把管道传入的数据作为第一个参数

# ==================== 计算各主成分的解释方差比例 ====================
var_explained <- pca$sdev^2 / sum(pca$sdev^2)
# pca$sdev 是各主成分的标准差
# sdev^2 是各主成分的方差（特征值）
# 除以总方差，得到每个主成分解释的方差比例

# ==================== 绘制 PCA 双标图 ====================
fviz_pca_biplot(pca,                              # PCA 结果对象
                axes = c(1, 2),                   # 使用第 1、2 主成分
                geom.ind = c("point"),            # 样本用点表示
                geom.var = c("arrow", "text"),    # 变量用箭头+文字表示
                pointshape = 20,                  # 样本点的形状（20=实心圆）
                pointsize = 4,                    # 样本点的大小
                label = "var",                    # 只给变量加标签
                repel = TRUE,                     # 标签避免重叠
                col.var = "grey50",               # 变量箭头/文字颜色
                labelsize = 0.5,                  # 标签文字大小
                col.ind = df$Subtype) +           # 样本按 Subtype 着色
  
  # ---- 自定义颜色 ----
scale_color_manual(values = colorRampPalette(brewer.pal(12, "Paired"))(4)) +
  # brewer.pal(12, "Paired") 从 Paired 调色板取 12 种颜色
  # colorRampPalette(...)(4) 从 12 种颜色中插值出 4 种
  # scale_color_manual() 手动指定颜色
  
  # ---- 设置轴标签和标题 ----
labs(x = paste0("(PC1: ", round(var_explained[1] * 100, 2), "%)"),
     # x 轴标签：PC1 及其解释方差百分比
     y = paste0("(PC2: ", round(var_explained[2] * 100, 2), "%)"),
     # y 轴标签：PC2 及其解释方差百分比
     title = "PCA-Biplot") +                     # 图标题
  # paste0() 拼接字符串
  # round(x, 2) 保留 2 位小数
  
  # ---- 主题设置 ----
theme(
  panel.background = element_rect(fill = 'white', colour = 'black'),
  # 面板背景：白色，黑色边框
  axis.title.x = element_text(colour = "black",       # x 轴标题颜色
                              size = 12,               # 字号
                              margin = margin(t = 12)),# 上边距 12
  axis.title.y = element_text(colour = "black",       # y 轴标题颜色
                              size = 12,               # 字号
                              margin = margin(r = 12)),# 右边距 12
  axis.text = element_text(color = "black"),           # 轴文字颜色
  plot.title = element_text(size = 12,                 # 标题字号
                            colour = "black",          # 标题颜色
                            hjust = 0.5,               # 居中（0=左，0.5=中，1=右）
                            face = "bold"),            # 粗体
  legend.title = element_blank(),                      # 图例标题不显示
  legend.key = element_blank(),                        # 图例键背景透明
  legend.text = element_text(color = "black", size = 9),# 图例文字
  legend.spacing.x = unit(0.1, 'cm'),                  # 图例水平间距
  legend.key.width = unit(0.2, 'cm'),                  # 图例键宽度
  legend.key.height = unit(0.2, 'cm'),                 # 图例键高度
  legend.background = element_blank(),                 # 图例背景透明
  legend.box.background = element_rect(colour = "black"),# 图例边框黑色
  legend.position = c(1, 0),                           # 图例位置：右下角（相对坐标）
  legend.justification = c(1, 0)                       # 图例对齐：右下角对齐
)

# ==================== 保存图片 ====================
ggsave("PCA.pdf",          # 文件名
       width = 5,          # 宽度 5 英寸
       height = 5,         # 高度 5 英寸
       dpi = 300)          # 分辨率 300 dpi