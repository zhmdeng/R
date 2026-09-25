# ==================== 加载包 ====================
library(tidyverse)   # 数据处理和绘图（含 dplyr、tidyr、ggplot2、readr）
library(readxl)      # 读取 Excel 文件
library(ggprism)     # Prism 风格主题和坐标轴
library(cowplot)     # 图形组合和注释

# ==================== 读取数据 ====================
df <- read_excel("41467_2023_38611_MOESM4_ESM.xlsx", sheet = 3) %>%
  # 读取第 3 个工作表
  select(`PatientNumber.Inc`,        # 患者编号（x 轴）
         MAX.CarTum_perc_change,     # 最大肿瘤变化百分比（y 轴）
         PDL1_CPS_r2,                # PD-L1 CPS 评分（柱子上方标注）
         ORR)                        # 客观缓解率（填充色）

# ==================== 固定 x 轴顺序 ====================
df$PatientNumber.Inc <- factor(df$PatientNumber.Inc,        # 患者编号转因子
                               levels = df$PatientNumber.Inc)  # 按原顺序排列

# ==================== 绘制条形图 ====================
plot <- df %>%
  ggplot(aes(`PatientNumber.Inc`,          # x 轴：患者编号
             MAX.CarTum_perc_change,        # y 轴：肿瘤变化百分比
             fill = ORR)) +                 # 填充色按 ORR 映射
  geom_col() +                              # 柱状图
  # ---- 正值的 PD-L1 标注（在柱子下方） ----
geom_text(data = df %>% filter(MAX.CarTum_perc_change > 0),   # 只取正值
          aes(y = -7, label = PDL1_CPS_r2),   # y=-7 位置，标签为 PD-L1 评分
          vjust = 0.5, hjust = 0.5,           # 居中对齐
          size = 5, color = "blue") +         # 字号 5，蓝色
  # ---- 负值的 PD-L1 标注（在柱子上方） ----
geom_text(data = df %>% filter(MAX.CarTum_perc_change < 0),   # 只取负值
          aes(y = 7, label = PDL1_CPS_r2),    # y=7 位置
          vjust = 0.5, hjust = 0.5,
          size = 5, color = "blue") +
  # ---- 参考线（y=0） ----
geom_hline(yintercept = 0,                # y=0 处
           color = "grey80",              # 灰色
           size = 0.8) +                  # 线宽 0.8
  # ---- 手动指定填充色 ----
scale_fill_manual(values = c("#B21B2B",   # 红
                             "#9ECAE1",   # 蓝
                             "#FFA500")) + # 橙
  # ---- 手动指定边框色 ----
scale_color_manual(values = c("#B21B2B", "#9ECAE1", "#FFA500")) +
  # ---- 轴标签和标题 ----
labs(x = NULL,                            # 不显示 x 轴标签
     y = "% Maximum Change from baeline", # y 轴标签（原文拼写错误）
     title = "Lung NENs") +               # 标题
  # ---- y 轴设置 ----
scale_y_continuous(limits = c(-100, 250),       # y 轴范围
                   breaks = seq(-100, 250, 50), # 刻度间隔 50
                   guide = "prism_offset") +    # Prism 风格坐标轴
  # ---- 主题 ----
theme_prism(base_size = 10) +             # Prism 主题，基础字号 10
  theme(
    axis.text.x = element_blank(),          # 不显示 x 轴文字
    axis.ticks.x = element_blank(),         # 不显示 x 轴刻度
    axis.title.y = element_text(color = "black", size = 12),   # y 轴标题
    plot.title = element_text(vjust = 0.5, hjust = 0.5),       # 标题居中
    legend.background = element_blank(),    # 图例背景透明
    legend.text = element_text(color = "black", size = 9, face = "bold"),  # 图例文字
    legend.spacing.x = unit(0.1, "cm"),     # 图例水平间距
    legend.key.height = unit(0.5, "cm"),    # 图例键高度
    legend.key.width = unit(0.5, "cm"),     # 图例键宽度
    legend.position = c(0.9, 0.8)           # 图例位置（右上角）
  )

# ==================== 添加文字注释 ====================
ggdraw(plot) +                              # 用 cowplot 转为可注释对象
  draw_text(text = "ORR: 11.1%",            # 添加文字
            size = 9,                       # 字号
            x = 0.2, y = 0.8,               # 位置（相对坐标）
            color = "black",                # 颜色
            fontface = "bold") +            # 粗体
  draw_text(text = "ORR PD-L1+:33.3%",      # 添加文字
            size = 9,
            x = 0.236, y = 0.768,
            color = "black") +
  draw_text(text = "ORR PD-L1-:0%",         # 添加文字
            size = 9,
            x = 0.22, y = 0.738,
            color = "black")

# ==================== 保存图片 ====================
ggsave("正负分布条形图.pdf", width = 8, height = 5, dpi = 300)