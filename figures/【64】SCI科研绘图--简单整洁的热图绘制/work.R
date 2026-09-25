library(tidyverse)    # 数据处理和绘图
library(ggsci)        # 期刊配色（NPG、JCO 等）
library(cowplot)      # 图形组合（ggdraw、draw_plot、draw_text）
library(patchwork)    # 图形组合（本段未直接使用）

# ==================== 读取数据 1 ====================
df <- read_tsv("data-1.txt") %>% pivot_longer(-sample)
# 读取 data-1.txt，除 sample 外全部转成长表，得到 sample、name、value 三列

df$sample <- factor(df$sample, levels = df$sample %>% unique() %>% rev())
# 将 sample 转为因子，水平反转（让第一个样本显示在最下方）

df$name <- factor(df$name, levels = df$name %>% unique())
# 将 name 转为因子，保持原顺序

# ==================== 绘制图 p1 ====================
p1 <- df %>% ggplot(aes(name, sample, fill = value, color = value)) +
  geom_tile(color = "grey50", fill = "white", size = 0.28) +
  # 白色底块，灰色边框；注意这里 fill="white" 会覆盖 aes 中的 fill 映射
  geom_point(data = df %>% filter(value == "Expressed"),
             size = 6, shape = 21) +
  # 对 value == "Expressed" 的点，用形状 21 画大点
  geom_point(data = df %>% filter(value == "Gene encoded"),
             size = 6, shape = 21) +
  # 对 value == "Gene encoded" 的点，同样画大点
  scale_fill_manual(values = c("grey90", "grey90")) +
  # 手动填充色，但两个值都是 grey90，没有区分
  scale_color_manual(values = c("black", "grey90")) +
  # 手动边框色：黑色和灰色
  labs(x = NULL, y = NULL, color = NULL, fill = NULL) +
  # 不显示轴标签和图例标题
  scale_x_discrete(expand = c(0, 0)) +               # x 轴不留空白
  scale_y_discrete(expand = c(0, 0), position = 'left') +  # y 轴不留空白，标签在左
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1,
                               color = "black", size = 8),  # x 轴文字旋转 45 度
    axis.text.y = element_text(color = "black", size = 8, face = "bold"),  # y 轴文字加粗
    axis.ticks = element_blank(),                      # 不显示刻度
    legend.background = element_blank(),               # 图例背景透明
    plot.background = element_blank(),                 # 图形背景透明
    legend.key = element_blank(),                      # 图例键背景透明
    legend.text = element_text(color = "black", face = "bold"),  # 图例文字加粗
    plot.margin = unit(c(3, 0.2, 0.2, 0.2), units = "cm")  # 上边距 3cm
  )

# ==================== 读取数据 2 ====================
df2 <- read_tsv("data-2.txt")
df2$sample <- factor(df2$sample, levels = df2$sample %>% unique())
# 将 sample 转为因子，保持原顺序（注意与 p1 中的顺序相反）

# ==================== 绘制图 p2 ====================
p2 <- df2 %>% ggplot(aes(sample, value, fill = group)) +
  geom_col(width = 0.9) +                             # 柱状图
  scale_y_continuous(expand = c(0, 0)) +              # y 轴不留空白
  scale_fill_npg() +                                  # NPG 配色
  theme_classic() +                                   # 经典主题
  theme(
    legend.position = "non",                          # ❌ 应为 "none"
    plot.background = element_blank(),                # 图形背景透明
    axis.text.y = element_text(color = "black", face = "bold"),  # y 轴文字加粗
    axis.title = element_blank(),                     # 不显示轴标题
    axis.ticks.x = element_blank(),                   # 不显示 x 轴刻度
    axis.text.x = element_blank()                     # 不显示 x 轴文字
  )

# ==================== 组合图形 ====================
ggdraw(p1) +                                          # 以 p1 为底图
  draw_plot(p2 + theme(legend.position = "none"),     # 叠加 p2（此处已修正为 "none"）
            scale = 0.635, x = -0.124, y = 0.6,
            width = 1, height = 0.4) +
  draw_text(text = "Condition MAG\n relative abundance(%)",
            size = 9, x = 0.25, y = 0.9, color = "black")
# 添加文字注释

# ==================== 保存图片 ====================
ggsave("图.pdf", width = 5, height = 3.6, dpi = 300)