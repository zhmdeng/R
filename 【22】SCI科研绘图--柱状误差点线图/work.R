# ========== 加载所需的包 ==========
library(tidyverse)   # 数据处理和绘图（包含 ggplot2、dplyr、readr 等）
library(readxl)      # 读取 Excel 文件
library(ggsci)       # 提供 Nature、NPG 等期刊配色方案
library(cowplot)     # 用于组合多个图形

# ========== 设置工作目录 ==========
setwd("/Users/mac/Desktop/PowerBI/sci科研绘图/【22】SCI科研绘图--柱状误差点线图/")

# ========== 读取数据 ==========
df <- read_tsv("data.xls")   # 读取制表符分隔的数据文件（虽然扩展名是 .xls，但实际是 tsv;如果文件是真正的 Excel，应该用 read_excel()。）

# ========== 绘制左图 p1：点 + 误差棒 ==========
p1 <- df %>% ggplot(aes(mean, group, color = type, fill = type)) +   # x 轴为 mean，y 轴为 group，颜色和填充按 type 映射
  geom_point(position = position_dodge(0.8)) +                        # 绘制点，position_dodge(0.8) 让同组不同 type 的点错开
  geom_errorbar(aes(xmin = mean - error, xmax = mean + error),        # 绘制误差棒，范围是 mean±error
                position = position_dodge(0.8)) +                     # 误差棒同样错开，与点对齐
  labs(y = NULL, x = "standardized coefficient") +                    # 设置轴标签：y 轴不显示标签，x 轴为 "standardized coefficient"
  scale_fill_npg() +                                                  # 填充色使用 NPG 期刊配色
  scale_color_npg() +                                                 # 边框色使用 NPG 期刊配色
  theme_bw() +                                                        # 使用黑白主题（白底黑线）
  theme(                                                              # 自定义主题细节
    axis.text = element_text(color = "black", size = 8, face = "bold"),       # 轴文字：黑色、8号、粗体
    axis.title = element_text(color = "black", size = 9, face = "bold"),      # 轴标题：黑色、9号、粗体
    legend.title = element_blank(),                                            # 图例标题不显示
    legend.text = element_text(color = "black", size = 7, face = "bold"),     # 图例文字：黑色、7号、粗体
    legend.position = c(0.2, 0.065),                                           # 图例位置：左下角（x=0.2, y=0.065）
    legend.background = element_blank(),                                       # 图例背景透明
    legend.spacing.x = unit(0.1, "cm"),                                        # 图例项之间的水平间距
    legend.key = element_blank(),                                              # 图例键背景透明
    legend.key.height = unit(0.4, "cm"),                                       # 图例键高度
    legend.key.width = unit(0.4, "cm"),                                        # 图例键宽度
    plot.background = element_blank(),                                         # 图形背景透明
    panel.background = element_blank()                                         # 面板背景透明
  )

# ========== 绘制右图 p2：柱状图 ==========
p2 <- df %>% ggplot(aes(`IncMSE (%)`, group, color = type, fill = type)) +   # x 轴为 IncMSE (%)，y 轴为 group
  geom_col(position = position_dodge(0.8)) +                                  # 绘制柱状图，同组不同 type 错开
  scale_x_continuous(expand = c(0, 0)) +                                      # x 轴不留空白（柱子从 0 开始）
  labs(y = NULL) +                                                            # y 轴不显示标签
  scale_fill_npg() +                                                          # 填充色使用 NPG 配色
  scale_color_npg() +                                                         # 边框色使用 NPG 配色
  theme_bw() +                                                                # 黑白主题
  theme(                                                                      # 自定义主题细节
    axis.text.y = element_blank(),                                            # y 轴文字不显示（与左图共用 y 轴）
    axis.text.x = element_text(color = "black", size = 8),                    # x 轴文字：黑色、8号
    axis.title = element_text(color = "black", size = 9, face = "bold"),      # 轴标题：黑色、9号、粗体
    axis.ticks.y = element_blank(),                                           # y 轴刻度线不显示
    legend.position = "non",                                                  # 不显示图例（写错了，应为 "none"）
    plot.background = element_blank(),                                        # 图形背景透明
    panel.background = element_blank()                                        # 面板背景透明
  )

# ========== 组合两张图 ==========
ggdraw() +                                                                    # 创建空白画布
  draw_plot(p1, scale = 0.9, x = -0.026, y = 0, width = 0.6, height = 1) +    # 放置 p1：左侧，占 60% 宽度
  draw_plot(p2, scale = 0.9, x = 0.506,  y = 0, width = 0.4, height = 1)      # 放置 p2：右侧，占 40% 宽度

# ========== 保存图片 ==========
ggsave("柱状误差点线图.pdf", width = 6, height = 6, dpi = 300)               # 保存为 PDF，6×6 英寸，300 dpi