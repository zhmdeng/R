# ==================== 加载包 ====================
library(tidyverse)     # 数据处理和绘图（含 dplyr、tidyr、ggplot2、readr）
library(readxl)        # 读取 Excel 文件
library(ggtree)        # 绘制聚类树
library(cowplot)       # 提供 ggdraw()、draw_plot()、draw_grob()，用于组合图形
library(patchwork)     # 图形组合（本代码未直接使用）
library(ggh4x)         # 提供 force_panelsizes()，强制分面大小
library(magrittr)      # 提供管道操作符
library(ggsci)         # 提供 NPG 等期刊配色
library(ggplotify)     # 把非 ggplot 图形转为 ggplot 对象
library(grid)          # 提供 grid.rect()、gpar() 等底层图形工具

# ==================== 导入数据 ====================
df <- read_excel("41564_2023_1322_MOESM5_ESM.xlsx", sheet = 1) %>%
  # 读取第 1 个工作表
  select(`Sample Name`, `Metabolic pathway`, `Capped copies per organism`) %>%
  # 只保留这 3 列
  pivot_wider(., names_from = `Sample Name`,            # 宽转长：Sample Name 变成列名
              values_from = `Capped copies per organism`)  # 值来自 Capped copies 列
# 结果：行=代谢通路，列=样本名

# ==================== 绘制聚类树 ====================
tree <- hclust(dist(df %>% column_to_rownames(var = "Metabolic pathway"))) %>%
  # 对代谢通路做层次聚类
  ggtree(layout = "rectangular",        # 矩形布局
         branch.length = "none")        # 不按枝长比例绘制
# tree 用于后面调整热图 Y 轴顺序

# ==================== 绘制热图 p1 ====================
p1 <- read_excel("41564_2023_1322_MOESM5_ESM.xlsx", sheet = 1) %>%
  ggplot(aes(`Sample Name`, `Metabolic pathway`,   # x=样本，y=代谢通路
             fill = `Capped copies per organism`)) +  # 填充色按丰度
  geom_tile() +                                 # 热图方块
  facet_grid(. ~ `Data source`, scale = "free") +  # 按数据来源分面
  labs(x = NULL, y = NULL) +                    # 不显示轴标签
  scale_y_discrete(expand = c(0, 0),            # y 轴不留空白
                   limits = get_taxa_name(tree)) +  # y 轴顺序按聚类树
  scale_x_discrete(expand = c(0, 0)) +          # x 轴不留空白
  theme(
    axis.text.x = element_blank(),              # x 轴文字不显示
    axis.text.y = element_text(color = "black"), # y 轴文字黑色
    axis.ticks = element_blank(),               # 不显示刻度
    panel.spacing = unit(0.1, "lines")          # 分面间距
  ) +
  force_panelsizes(cols = c(0.3, 0.5),          # 强制分面宽度比例
                   respect = TRUE) +            # 保持比例
  scale_color_gradientn(colours = rev(RColorBrewer::brewer.pal(6, "RdBu"))) +
  scale_fill_gradientn(colours = rev(RColorBrewer::brewer.pal(6, "RdBu")))
# 填充色：RdBu 配色反转

# ==================== 绘制热图 p2 ====================
p2 <- read_excel("41564_2023_1322_MOESM5_ESM.xlsx", sheet = 2) %>%
  mutate(RPKM = log10(`Total RPKM` + 1)) %>%    # log10 变换（+1 避免 log(0)）
  ggplot(aes(`Sample Name`, `Metabolic pathway`, # x=样本，y=代谢通路
             fill = RPKM)) +                    # 填充色按 log10 RPKM
  geom_tile() +                                 # 热图方块
  facet_grid(. ~ `Data source`, scale = "free") +  # 按数据来源分面
  labs(x = NULL, y = NULL) +
  scale_y_discrete(expand = c(0, 0),
                   limits = get_taxa_name(tree)) +  # y 轴顺序与 p1 一致
  scale_x_discrete(expand = c(0, 0)) +
  theme(
    axis.text.x = element_blank(),              # x 轴文字不显示
    axis.text.y = element_blank(),              # y 轴文字不显示（与 p1 共用）
    axis.ticks = element_blank(),
    panel.spacing = unit(0.08, "lines")         # 分面间距
  ) +
  scale_color_gradientn(colours = rev(RColorBrewer::brewer.pal(6, "RdBu"))) +
  scale_fill_gradientn(colours = rev(RColorBrewer::brewer.pal(6, "RdBu")))

# ==================== 绘制堆叠柱状图 p3 ====================
p3 <- read_excel("41564_2023_1322_MOESM5_ESM.xlsx", sheet = 3) %>%
  select(4, 3, 5) %>%                           # 取第 4、3、5 列
  set_colnames(c("group", "value", "sample")) %>%  # 重命名列
  mutate(face = "Tara Oceans (metagenomes)") %>%   # 添加分面标签
  bind_rows(.,                                  # 合并
            read_excel("41564_2023_1322_MOESM5_ESM.xlsx", sheet = 4) %>%
              select(6, 4, 5) %>%
              set_colnames(c("group", "value", "sample")) %>%
              mutate(face = "This study (metagenomes)")) %>%
  bind_rows(.,
            read_excel("41564_2023_1322_MOESM5_ESM.xlsx", sheet = 5) %>%
              select(6, 4, 5) %>%
              set_colnames(c("group", "value", "sample")) %>%
              mutate(face = "Tara Oceans (metatranscriptomes)")) %>%
  ggplot(aes(sample, value, fill = group)) +    # x=样本，y=值，填充=group
  geom_bar(stat = "identity",                   # 直接用数值
           position = "fill",                   # 归一化堆叠
           width = 0.8) +                       # 柱宽 0.8
  facet_grid(. ~ face, scale = "free") +        # 按 face 分面
  labs(x = NULL, y = NULL) +
  scale_y_continuous(expand = c(0, 0)) +        # y 轴不留空白
  scale_x_discrete(expand = c(0, 0)) +          # x 轴不留空白
  theme_test() +                                # test 主题
  theme(
    axis.text.x = element_text(color = "black", # x 轴文字
                               angle = 45,      # 旋转 45 度
                               vjust = 1, hjust = 1,  # 对齐
                               size = 6),       # 字号 6
    panel.spacing = unit(0.3, "lines")          # 分面间距
  ) +
  force_panelsizes(cols = c(1, 1.7, 2),         # 强制分面宽度
                   respect = TRUE) +
  scale_fill_npg()                              # NPG 配色

# ==================== 拼图 ====================
(p1 + theme(legend.position = "none",          # 隐藏图例
            plot.margin = margin(0.1, 12, 10, 0, unit = "cm"))) %>%
  # 设置 p1 的边距
  ggdraw() +                                    # 创建空白画布
  draw_plot(p3 + theme(legend.position = "none"),  # 放置 p3（堆叠柱状图）
            scale = 0.7,                        # 缩放 0.7
            x = 0.082, y = -0.318) +            # 位置
  draw_plot(p2 + theme(legend.position = "none"),  # 放置 p2（热图）
            scale = 1,                          # 缩放 1
            x = 0.638, y = 0.321,               # 位置
            width = 0.2959, height = 0.68) +    # 宽高
  draw_plot(ggpubr::get_legend(p3) %>%          # 提取 p3 的图例
              as.ggplot(),                      # 转为 ggplot 对象
            scale = 0.05,                       # 缩放 0.05
            x = -0.32, y = -0.27) +             # 位置
  draw_grob(grid::grid.rect(                    # 添加阴影矩形
    gp = grid::gpar(fill = "#55BF93",           # 填充色：绿色
                    col = "#55BF93",            # 边框色：绿色
                    alpha = 0.2)),              # 透明度 0.2
    x = 0.065, y = 0.859,                       # 位置
    height = 0.05, width = 0.19)                # 高、宽

# ==================== 保存图片 ====================
ggsave("基因丰度图.pdf", width = 14, height = 12, dpi = 300, limitsize = FALSE)
# 保存为 PDF，宽 14 英寸，高 12 英寸