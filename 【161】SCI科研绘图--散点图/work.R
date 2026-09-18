library(tidyverse)      # 数据处理和绘图（含 ggplot2、dplyr、stringr 等）
library(ggpointdensity) # 提供 geom_pointdensity()，绘制密度散点图
library(ggsci)          # 提供 NPG、JCO 等期刊配色

load("df.Rdata")        # 加载数据，df 应包含 score、avg_match_perc、myers_briggs 等列

df %>%
  ggplot(aes(x = score, y = avg_match_perc)) +   # x=score，y=avg_match_perc
  geom_pointdensity(size = 1, shape = 18) +       # 密度散点图：点密度越高颜色越深，形状 18 为菱形
  geom_smooth(method = "lm", color = "grey50") +  # 线性回归拟合线（灰色）
  facet_wrap(vars(myers_briggs)) +                # 按 myers_briggs 分面
  geom_tile(aes(x = 50, y = 80, width = Inf, height = 8,
                fill = case_when(
                  str_starts(myers_briggs, "INTP") ~ "grey60",       # INTP 开头 → 灰色
                  str_starts(myers_briggs, "ENT|EST") ~ "#A2BEFF",   # ENT 或 EST → 浅蓝
                  str_starts(myers_briggs, "INT|IST") ~ "coral2",    # INT 或 IST → 珊瑚红
                  TRUE ~ "cornflowerblue")                            # 其他 → 矢车菊蓝
  )) +
  # 在每个分面的 (50, 80) 处画一个宽无限、高 8 的色块，作为标题背景条
  geom_text(aes(x = 50, y = 80, label = myers_briggs),
            color = "white", stat = "unique", fontface = "bold") +
  # 在色块上叠加白色粗体标签，stat="unique" 保证每个分面只画一次
  scale_y_continuous(breaks = seq(30, 70, 10)) +   # y 轴刻度：30、40、50、60、70
  scale_color_gradientn(colours = rev(RColorBrewer::brewer.pal(8, "RdBu"))) +
  # 点密度颜色：RdBu 配色反转（蓝→红）
  scale_fill_npg() +                               # 填充色：NPG 配色
  labs(x = NULL, y = NULL) +                       # 不显示轴标签
  theme_minimal() +                                # 极简主题
  theme(
    legend.position = "none",                      # 不显示图例
    plot.background = element_rect(fill = "grey97", color = NA),  # 图形背景浅灰
    strip.text = element_blank(),                  # 分面标签不显示（用 geom_text 替代）
    axis.text = element_text(color = "black", size = 7),          # 轴文字
    axis.title.x = element_text(margin = margin(10, 0, 0, 0)),    # x 轴标题边距
    axis.title.y = element_text(margin = margin(0, 10, 0, 0)),    # y 轴标题边距
    plot.margin = margin(10, 10, 10, 10),          # 图形边距
    panel.spacing.x = unit(0.1, "cm")              # 分面水平间距
  )

ggsave("图.pdf", width = 8, height = 8, dpi = 300)   # 保存为 PDF，8×8 英寸，300 dpi