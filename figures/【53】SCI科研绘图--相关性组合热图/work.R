

#======如何绘制环境因子相关性热图 + Mantel 检验连接线 + 柱状图的组合图===

# ==================== 加载包 ====================
library(tidyverse)      # 数据处理和绘图
library(readxl)         # 读取 Excel
library(linkET)         # 提供 qcorrplot()、geom_couple()、mantel_test()
library(RColorBrewer)   # 配色方案
library(ggtext)         # 提供 element_markdown()，支持富文本
library(magrittr)       # 提供管道操作符
library(ggnewscale)     # 允许在同一张图中多次使用 scale_*，实现多套配色
library(cowplot)        # 提供 ggdraw()、draw_plot()，用于组合图形

# ==================== 读取环境因子数据 ====================
varechem <- read_excel("imt279-sup-0002-supplementary_tables_1.xlsx", skip = 1) %>%
  # 读取 Excel，跳过第 1 行
  select(pH:DOC) %>%    # 只保留 pH 到 DOC 之间的列（环境因子）
  dplyr::rename(
    "NO<sub>3</sub>-" = `NO3-N`,     # 重命名，用 HTML 标签做下标
    "NO<sub>2</sub>-" = `NO2-N`,
    "NH<sub>4</sub>+" = `NH4-N`,
    "PO<sub>4</sub>3-" = "PO4"
  )

# ==================== 读取物种数据 ====================
varespec <- read_excel("imt279-sup-0002-supplementary_tables_1.xlsx",
                       skip = 1, sheet = 2) %>%
  # 读取第 2 个工作表，跳过第 1 行
  select(-1)             # 删除第 1 列（通常是样本名）

# ==================== Mantel 检验 ====================
mantel <- mantel_test(varespec, varechem,
                      spec_select = list(" " = 1:9)) %>%
  # 对物种和环境因子做 Mantel 检验
  # spec_select 指定物种分组，这里把 1:9 列归为一组
  mutate(
    rd = cut(r, breaks = c(-Inf, 0.2, 0.4, Inf),
             labels = c("< 0.2", "0.2 - 0.4", ">= 0.4")),  # 按 r 值分组
    pd = cut(p, breaks = c(-Inf, 0.01, 0.05, Inf),
             labels = c("0.001-0.01", "0.01-0.05", ">= 0.05"))  # 按 p 值分组
  )

# ==================== 绘制相关性热图 p1 ====================
p1 <- qcorrplot(correlate(varechem, method = "pearson"),
                diag = T, type = "upper") +
  # 环境因子之间的 Pearson 相关性热图
  # diag=T：显示对角线
  # type="upper"：只显示上三角
  geom_point(aes(size = abs(r)), shape = 21) +
  # 用点大小表示 |r|
  scale_fill_gradientn(colours = RColorBrewer::brewer.pal(8, "RdYlBu")) +
  # 填充色：RdYlBu 配色
  scale_color_gradientn(colours = RColorBrewer::brewer.pal(8, "RdYlBu")) +
  # 边框色：同上
  guides(size = "none") +                 # 不显示 size 图例
  new_scale("size") +                     # 开启新的 size scale
  geom_couple(aes(colour = pd, size = rd),  # 按 pd 着色、rd 控制线宽
              data = mantel,              # 数据源
              label.colour = "black",     # 标签颜色
              curvature = nice_curvature(0.1, by = "from"),  # 曲线弯曲度
              nudge_x = -1,               # x 方向偏移
              offset_x = 3,               # x 方向偏移量
              label.fontface = 0,         # 标签字体（0=普通）
              label.size = 4,             # 标签字号
              drop = T) +                 # 丢弃不显著的
  scale_size_manual(values = c(0.5, 1, 2)) +   # 线宽：3 个等级
  scale_colour_manual(values = c("#788CAE", "#EA967B", "#FBD4AA")) +
  # 线条颜色：3 个等级
  guides(
    size = guide_legend(title = "Mantel's r",
                        override.aes = list(colour = "grey35"),
                        order = 2),
    color = "none",                       # 不显示 color 图例
    fill = guide_colorbar(title = "pearson's r", order = 3)
  ) +
  theme(
    plot.margin = unit(c(-1.8, -2, 1, -2), units = "cm"),  # 图形边距
    panel.background = element_blank(),   # 面板背景透明
    axis.text = element_markdown(color = "black", size = 10),  # 轴文字
    legend.key = element_blank(),         # 图例键背景透明
    legend.background = element_blank()   # 图例背景透明
  )

# ==================== 绘制柱状图 p2 ====================
df <- mantel %>% arrange(desc(r))         # 按 r 值降序排列
df$env <- factor(df$env, levels = df$env) # env 转因子，保持顺序

p2 <- df %>% ggplot(aes(env, r, fill = pd)) +   # x=env，y=r，填充=pd
  geom_col(width = 0.8) +                 # 柱状图，宽度 0.8
  scale_fill_manual(values = c("#788CAE", "#EA967B", "#FBD4AA")) +
  # 填充色：3 个等级
  scale_y_continuous(expand = c(0, 0)) +  # y 轴不留空白
  theme_classic() +                       # 经典主题
  labs(x = NULL, y = NULL) +              # 不显示轴标签
  theme(
    axis.text.x = element_markdown(color = "black", angle = 40,
                                   vjust = 1, hjust = 1),  # x 轴文字
    axis.text.y = element_text(color = "black"),           # y 轴文字
    panel.background = element_blank(),   # 面板背景透明
    plot.background = element_blank(),    # 图形背景透明
    legend.text = element_text(color = "black"),  # 图例文字
    legend.background = element_blank(),  # 图例背景透明
    legend.title = element_blank(),       # 图例标题不显示
    legend.key = element_blank(),         # 图例键背景透明
    legend.position = "bottom",           # 图例在底部
    legend.key.height = unit(0.4, "cm"),  # 图例键高度
    legend.spacing.y = unit(0.4, "cm")    # 图例垂直间距
  )

# ==================== 组合图形 ====================
plot <- ggdraw() +                        # 创建空白画布
  draw_plot(p1, scale = 0.63, y = 0.045, width = 1, height = 1) +
  # 放置 p1：缩放 0.63，位置 (默认 x, 0.045)，宽高各占 1
  draw_plot(p2, scale = 0.6, x = -0.2, y = -0.15, width = 1.2, height = 0.7)
# 放置 p2：缩放 0.6，位置 (-0.2, -0.15)，宽 1.2、高 0.7

# ==================== 保存图片 ====================
ggsave(plot, file = "heatmap.pdf",        # 保存对象和文件名
       width = 4.7, height = 4.98,        # 宽 4.7 英寸，高 4.98 英寸
       unit = "in", dpi = 300)            # 单位：英寸（应为 units）