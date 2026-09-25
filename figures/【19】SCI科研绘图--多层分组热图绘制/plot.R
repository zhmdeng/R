# ==================== 加载包 ====================
library(tidyverse)    # 数据处理和绘图
library(ggh4x)        # 提供 axis_nested、facet_grid2 等扩展功能
library(MetBrewer)    # 提供 met.brewer() 配色方案

# setwd("")           # 设置工作目录

# ==================== 读取并整理数据 ====================
df <- read_tsv("data.tsv") %>%
  pivot_longer(-gene) %>%                          # 宽转长：除 gene 外都转成长表
  mutate(
    group1 = sub("\\s.*", "", name),               # 提取 name 中第一个空格前的内容
    # sub("\\s.*", "", name)：把第一个空格及其后面的内容全部删掉
    # 例如 "Basal 1-1" → "Basal"，"Luminal 1-1" → "Luminal"
    group1 = case_when(
      group1 == "Basal"   ~ "Bas",                 # Basal → Bas
      group1 == "Luminal" ~ "Lum",                 # Luminal → Lum
      TRUE ~ group1                                # 其他值保持不变（兜底，避免 NA）
    ),
    group2 = case_when(
      grepl("1-", name) ~ "Bio rep 1",             # name 含 "1-" → Bio rep 1
      grepl("2-", name) ~ "Bio rep 2",             # name 含 "2-" → Bio rep 2
      grepl("3-", name) ~ "Bio rep 3",             # name 含 "3-" → Bio rep 3
      TRUE ~ "Other"                               # 其他 → Other
    )
  )

# ==================== 固定基因顺序 ====================
df$gene <- factor(df$gene,
                  levels = df$gene %>% unique() %>% rev())
# 把 gene 转为因子，顺序反转（让第一个基因显示在最下方）

# ==================== 绘制多层分组热图 ====================
df %>%
  ggplot(., aes(interaction(name, group1), gene,   # x=name 和 group1 的交互，y=gene
                color = value, fill = value)) +    # 颜色和填充按 value 映射
  geom_tile() +                                    # 热图方块
  geom_point(data = df %>% filter(is.na(value)),   # 只取 value 为 NA 的数据
             shape = 4, size = 4, color = "black") +  # 用叉号标记缺失值
  facet_grid(. ~ group2, scales = "free_x",        # 按 group2 分面
             switch = "x") +                       # 分面标签放在 x 轴一侧
  guides(x = "axis_nested") +                      # x 轴使用嵌套轴（多层分组）
  scale_x_discrete(expand = c(0, 0),               # x 轴不留空白
                   position = 'top') +             # x 轴放在顶部
  scale_y_discrete(expand = c(0, 0)) +             # y 轴不留空白
  scale_fill_gradientn(colors = met.brewer("Cassatt1"),  # 填充色：MetBrewer 配色
                       na.value = NA) +            # NA 值不填充
  scale_color_gradientn(colors = met.brewer("Cassatt1"), # 边框色：同上
                        na.value = NA) +
  labs(x = NULL, y = NULL,                         # 不显示轴标签
       fill = "Row \n z-score",                    # fill 图例标题
       color = "Row \n z-score") +                 # color 图例标题
  theme(
    axis.text.x = element_blank(),                 # x 轴文字不显示
    axis.text.y = element_text(color = "black",    # y 轴文字：黑色
                               size = 8,           # 字号 8
                               face = "italic"),   # 斜体（基因名）
    axis.ticks.x = element_blank(),                # 不显示 x 轴刻度
    axis.ticks.y = element_blank(),                # 不显示 y 轴刻度
    strip.background = element_blank(),            # 分面标签背景透明
    strip.text = element_text(color = "black",     # 分面文字：黑色
                              size = 9,            # 字号 9
                              face = "bold"),      # 粗体
    panel.background = element_blank(),            # 面板背景透明
    plot.background = element_blank(),             # 图形背景透明
    legend.spacing.x = unit(0.1, "cm"),            # 图例水平间距
    panel.spacing.x = unit(0.01, "cm"),            # 分面水平间距
    panel.border = element_rect(fill = NA,         # 面板边框：无填充
                                color = "black",   # 黑色
                                size = 0.5,        # 线宽 0.5
                                linetype = "solid"),  # 实线
    ggh4x.axis.nestline.x = element_line(size = 0.5, color = "black"),
    # 嵌套轴的分隔线
    ggh4x.axis.nesttext.x = element_text(
      colour = "black",                            # 嵌套轴文字颜色
      angle = 0,                                   # 不旋转
      size = 10,                                   # 字号 10
      vjust = 0, hjust = 0.5,                      # 对齐
      face = "bold",                               # 粗体
      margin = margin(b = 3)                       # 下边距 3
    ),
    plot.margin = unit(c(0.2, 0.2, 0.2, 0.2),      # 图形边距
                       units = "cm")               # 单位：厘米
  )

# ==================== 保存图片 ====================
ggsave("多层分组热图绘制.pdf",
       width = 6, height = 6, dpi = 300)