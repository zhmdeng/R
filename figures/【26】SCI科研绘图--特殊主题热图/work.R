# ==================== 加载包 ====================
library(tidyverse)   # 数据处理和绘图（含 dplyr、tidyr、ggplot2、readr）
library(readxl)      # 读取 Excel 文件
library(ggh4x)       # 提供 facet_grid2()、force_panelsizes() 等扩展分面功能
library(grid)        # 提供 unit() 等底层图形工具
library(cowplot)     # 图形组合工具（本代码未直接使用，可能是备用的）

# ==================== 读取并整理数据 ====================
df <- read_excel("41591_2022_2116_MOESM4_ESM.xlsx") %>%
  # 读取 Excel 文件
  pivot_longer(-c(ID, group))
# 宽转长：除 ID 和 group 外的所有列变成长表
# 结果包含 ID、group、name、value 四列

# ==================== 固定 ID 顺序 ====================
df$ID <- factor(df$ID,                          # ID 转因子
                levels = df$ID %>%               # 水平顺序
                  unique() %>%                   # 去重
                  rev())                         # 反转
# 反转后，原来第一个 ID 显示在最下方

# ==================== 绘制热图 ====================
df %>%
  ggplot(aes(name, ID)) +                       # x=name，y=ID
  
  # ---- 白色底块 ----
geom_tile(color = "grey70",                   # 边框色灰色
          fill = "white",                     # 填充白色
          size = 0.3) +                       # 边框线宽 0.3
  # geom_tile() 画矩形，作为热图的底
  
  # ---- 彩色方块（按 value 着色） ----
geom_point(data = df %>% filter(value > 0.08),  # 只取 value > 0.08 的数据
           aes(size = abs(value),               # 大小按 |value|
               color = value),                  # 颜色按 value
           shape = 15,                          # 形状：15 是实心方块
           size = 6) +                          # 大小 6（会覆盖 aes 中的 size）
  # geom_point() 叠加彩色方块
  
  # ---- 颜色渐变 ----
scale_color_gradient2(low = "grey",             # 低值：灰色
                      mid = "grey",             # 中间：灰色
                      high = "black") +         # 高值：黑色
  # 灰色 → 黑色渐变，高值更醒目
  
  # ---- 自定义分面 ----
facet_grid2(vars(group),                        # 按 group 分面
            scale = "free_y",                   # y 轴各分面独立
            switch = "y",                       # 分面标签放在 y 轴一侧
            labeller = label_wrap_gen()) +      # 标签自动换行
  # facet_grid2() 是 ggh4x 的扩展分面函数，支持更灵活的分面设置
  
  # ---- 强制分面大小 ----
force_panelsizes(cols = c(1.5),                 # 列宽 1.5
                 rows = c(0.55, 0.15, 1, 0.15, 0.9, 1.2, 0.55, 0.15, 0.15, 0.15),
                 respect = TRUE) +
  # rows 指定每个分面的高度，共 10 个分面
  # respect = TRUE 保持比例
  
  # ---- y 轴设置 ----
scale_y_discrete(expand = c(0, 0),              # 不留空白
                 position = 'left') +           # y 轴在左侧
  
  # ---- x 轴设置 ----
scale_x_discrete(expand = c(0, 0),              # 不留空白
                 position = 'top') +            # x 轴在顶部
  
  # ---- 轴标签 ----
labs(x = NULL, y = NULL) +                      # 不显示轴标签
  
  # ---- 主题设置 ----
theme(
  panel.background = element_blank(),           # 面板背景透明
  axis.text.x = element_text(color = "black",   # x 轴文字黑色
                             size = 8),         # 字号 8
  axis.text.y = element_text(color = "black"),  # y 轴文字黑色
  legend.title = element_blank(),               # 图例标题不显示
  axis.ticks = element_blank(),                 # 不显示刻度
  strip.placement = 'outside',                  # 分面标签放在轴外侧
  strip.text.y.left = element_text(size = 8,    # 分面文字：字号 8
                                   color = "black",  # 黑色
                                   face = "bold",    # 粗体
                                   angle = 0),       # 不旋转（水平）
  panel.spacing.y = unit(0, "cm")               # 分面间距 0
) +
  
  # ---- 图例设置 ----
guides(color = guide_colorbar(direction = "vertical",   # 颜色图例垂直
                              reverse = F,              # 不反转
                              barwidth = unit(.5, "cm"),   # 图例条宽
                              barheight = unit(21, "cm"))) +  # 图例条高
  
  # ---- 允许元素超出绘图区域 ----
coord_cartesian(clip = 'off')
# 允许图例等元素超出绘图区域

# ==================== 保存图片 ====================
ggsave("特殊主题热图.pdf", width = 8, height = 8, dpi = 300)