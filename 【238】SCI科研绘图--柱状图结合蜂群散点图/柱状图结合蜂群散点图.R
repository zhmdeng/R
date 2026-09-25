# setwd("")                    # 设置工作目录
df <- read.csv("df.csv", header = T)
# 读取 df.csv 文件，header = T 表示第一行是列名，默认逗号分隔

library(tidyr)                 # 加载 tidyr 包，提供 gather() 等数据变形函数

data <- gather(df, gene, value, 1:7)
# 把 df 从宽表转成长表：
# - 第 1 到 7 列作为“键”，列名变成 gene 列的值
# - 对应的数值变成 value 列
# - 其余列（如果有）保持不变
# 注意：gather() 在新版 tidyr 中已被 pivot_longer() 取代，但仍可用

# ==================== 第一张图：柱状图 + 蜂群散点 ====================
library(ggbeeswarm)            # 加载 ggbeeswarm 包，提供 geom_beeswarm() 蜂群散点
library(ggplot2)               # 加载 ggplot2 绘图

ggplot(data, aes(x = gene, y = value, shape = gene)) +
  # 创建 ggplot 对象：x 轴为 gene，y 轴为 value，形状按 gene 映射
  
  geom_bar(stat = "summary",   # 柱状图，stat="summary" 表示用统计汇总值作为高度（默认 mean）
           width = 0.9,        # 柱子宽度 0.9
           size = 0.5,         # 边框线宽 0.5
           color = 'black',    # 边框颜色黑色
           fill = 'white') +   # 填充白色
  
  stat_summary(fun.data = 'mean_se',   # 添加误差棒，统计量为 mean_se（均值 ± 标准误）
               geom = "errorbar",      # 用误差棒几何对象
               colour = "black",       # 误差棒颜色黑色
               width = 0.2,            # 误差棒宽度 0.2
               position = position_dodge(0.7)) +
  # 位置躲开宽度 0.7，避免与柱子重叠
  
  geom_beeswarm(dodge.width = 0.8,     # 蜂群散点，躲开宽度 0.8
                aes(y = value, x = gene, fill = gene),
                # 映射：y=value，x=gene，填充色按 gene
                size = 3,              # 点大小 3
                show.legend = FALSE) + # 不显示图例
  
  scale_shape_manual(values = c(21,21,22,22,21,21,21)) +
  # 手动指定形状：21 是实心圆，22 是实心方块
  
  scale_fill_manual(values = c("black","#68317F","#E7628C","#00A66C",
                               "#5363A5","#F6D6B7","orange")) +
  # 手动指定填充色，共 7 种
  
  theme_classic() +            # 经典主题（无网格线，白色背景）
  theme(
    axis.text = element_text(size = 12, color = "black"),  # 轴文字
    axis.line.y = element_line(color = 'black'),           # y 轴线黑色
    axis.title.y = element_text(size = 14, color = "black") # y 轴标题
  ) +
  theme(axis.title.x = element_blank()) +   # x 轴标题不显示
  theme(
    panel.grid = element_blank(),           # 不显示网格线
    panel.background = element_blank()      # 面板背景透明
  ) +
  scale_y_continuous(expand = c(0, 0))
# y 轴不留空白（柱子从 0 开始）

# ==================== 第二张图：柱状图 + 抖动散点 ====================
ggplot(data, aes(x = gene, y = value, shape = gene)) +
  # 同样是 x=gene，y=value，shape=gene
  
  geom_bar(stat = "summary",
           width = 0.9,
           size = 0.5,
           color = 'black',
           fill = 'white') +
  
  stat_summary(fun.data = 'mean_se',
               geom = "errorbar",
               colour = "black",
               width = 0.2,
               position = position_dodge(0.7)) +
  
  geom_jitter(data = data,                      # 抖动散点
              aes(y = value, x = gene, fill = gene),
              size = 4,                         # 点大小 4
              stroke = 0.15,                    # 边框粗细 0.15
              show.legend = FALSE,              # 不显示图例
              position = position_jitterdodge(
                jitter.height = 0.5,            # 垂直抖动幅度 0.5
                jitter.width = 0.1,             # 水平抖动幅度 0.1
                dodge.width = 0.8)) +           # 躲开宽度 0.8
  
  scale_shape_manual(values = c(21,21,22,22,21,21,21)) +
  scale_fill_manual(values = c("black","#68317F","#E7628C","#00A66C",
                               "#5363A5","#F6D6B7","orange")) +
  
  theme_classic() +
  theme(
    axis.text = element_text(size = 12, color = "black"),
    axis.line.y = element_line(color = 'black'),
    axis.title.y = element_text(size = 14, color = "black")
  ) +
  theme(axis.title.x = element_blank()) +
  theme(
    panel.grid = element_blank(),
    panel.background = element_blank()
  ) +
  scale_y_continuous(expand = c(0, 0))

# ==================== 保存图片 ====================
ggsave("图片.pdf", width = 6, height = 8, dpi = 300)
# 保存最后一次绘制的图（即第二张图）到 图片.pdf
# 宽 6 英寸，高 8 英寸，分辨率 300 dpi