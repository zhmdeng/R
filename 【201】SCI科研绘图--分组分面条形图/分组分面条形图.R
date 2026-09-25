# 设置工作环境
rm(list = ls())          # 清空当前环境中的所有对象
# setwd("")              # 设置工作目录（已注释，需要时取消注释并填入路径）

# ==================== 加载 R 包 ====================
library(ggplot2)         # 绘图核心包
library(reshape2)        # 提供 melt()，把宽表转成长表

# ==================== 读取数据 ====================
df <- read.table("data.txt", header = 1, check.names = F, sep = "\t")
# 读取制表符分隔的数据，第一行为列名，不修改列名

# ==================== 整理数据为绘图格式 ====================
data <- melt(df)         # 宽表转长表
# 默认保留所有非数值列作为 id 变量，数值列变成 variable 和 value 两列
# 注意：如果 df 里有 group 和 sample 列，melt 后会保留；如果没有，后面会报错

data$group <- factor(data$group, levels = c("A", "B", "C", "D"))
# 将 group 转为因子，固定顺序为 A、B、C、D
# 这里要求 data 中已有 group 列（通常来自 df 中的 group 列）

data$sample <- factor(data$sample, levels = rev(df$sample))
# 将 sample 转为因子，水平顺序按 df$sample 反转
# 要求 df 中有 sample 列，且 data 中有 sample 列

# ==================== 绘图 ====================
ggplot(data, aes(value, sample, fill = group)) +   # x=value，y=sample，填充=group
  geom_col() +                                     # 绘制柱状图，高度由 value 决定
  facet_grid(~ variable) +                         # 按 variable 分面（水平方向）
  labs(fill = NULL, y = NULL, x = "This is X-axis!") +  # 图例标题为空，y 轴标题为空，x 轴标题自定义
  theme_bw() +                                     # 使用黑白主题
  theme(
    axis.text.y = element_text(size = 10, color = "black"),   # y 轴文字
    axis.text.x = element_text(size = 10, color = "black",
                               angle = 270, vjust = 0.5, hjust = 0),  # x 轴文字旋转 270 度
    strip.text = element_text(size = 12, color = "black"),    # 分面标签文字
    legend.text = element_text(size = 12, color = "black"),   # 图例文字
    axis.title.x = element_text(size = 14, color = "black")   # x 轴标题
  ) +
  scale_fill_manual(values = c("#ff3c41", "#fcd000", "#47cf73", "#0ebeff"),
                    # 手动指定 4 种填充色
                    guide = guide_legend(keywidth = 1.5, keyheight = 7))
# 图例键宽 1.5、高 7（让图例项看起来更“长条”）

# ==================== 保存图片 ====================
ggsave("图.pdf", dpi = 300)   # 保存最后一次绘制的图，默认单位英寸，未指定宽高