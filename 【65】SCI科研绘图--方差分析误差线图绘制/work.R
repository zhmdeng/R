# ========== 加载所需的包 ==========
library(tidyverse)    # 数据处理和绘图
library(ggtext)       # 支持富文本（如上下标）的 ggplot2 扩展
# BiocManager::install("ggprism")  # 安装 ggprism（已注释）
library(ggprism)      # 提供 GraphPad Prism 风格的图表主题
library(ggsignif)     # 添加显著性标注
library(rstatix)      # 提供统计检验的管道友好接口
library(ggpubr)       # 提供 publication-ready 的绘图函数

# ========== 设置工作目录 ==========
# setwd("/Users/mac/Desktop/PowerBI/sci科研绘图/【65】SCI科研绘图--方差分析误差线图绘制/")

# ========== 读取并整理数据 ==========
df <- read_tsv("data.txt") %>%          # 读取制表符分隔的数据
  pivot_longer(-Type) %>%               # 宽转长：把除 Type 外的所有列转成 name/value 两列
  dplyr::filter(Type == "Day 5") %>%    # 只保留 Type 为 "Day 5" 的行
  select(-Type)                         # 删除 Type 列

# ========== 方差分析 ==========
result.aov <- aov(value ~ name, data = df)   # 单因素方差分析（ANOVA）
result.tukey <- TukeyHSD(result.aov)         # Tukey 事后检验（两两比较）

# ========== 整理 Tukey 检验结果 ==========
aov_pvalue <- result.tukey$name %>%                    # 提取 Tukey 结果中 name 部分
  as.data.frame() %>%                                  # 转为数据框
  rownames_to_column(var = "group") %>%                # 行名转为 group 列
  dplyr::select(1, `p adj`) %>%                        # 只保留 group 和 p adj 两列
  separate(`group`, into = c("group2", "group1"),      # 拆分 group 列为 group2 和 group1
           sep = "CAR T-", convert = TRUE) %>%         # 用 "CAR T-" 作为分隔符
  mutate(group1 = str_replace_all(`group1`, "CAR T", "")) %>%  # 去掉 group1 中的 "CAR T"
  select(2, 1, 3) %>%                                  # 调整列顺序
  select(-1, -2) %>%                                   # 删除前两列（只保留第三列？）
  mutate(p_signif = symnum(`p adj`, corr = FALSE, na = FALSE,  # 根据 p 值生成显著性符号
                           cutpoints = c(0, 0.01, 0.05, 1),     # 显著性阈值
                           symbols = c("**", "*", "ns")))       # 符号：** / * / ns

# ========== 整理 Wilcoxon 检验结果 ==========
df_pvalue <- df %>% 
  mutate(`name` = str_replace_all(`name`, "CAR T", "")) %>%   # 去掉 name 中的 "CAR T"
  mutate(name = str_trim(name)) %>%                           # 去除首尾空格
  wilcox_test(value ~ name) %>%                               # Wilcoxon 秩和检验（两两比较）
  add_significance(p.col = "p.adj") %>%                       # 添加显著性标记
  add_xy_position(x = "name") %>%                             # 计算显著性标注的位置
  select(-p.adj) %>%                                          # 删除 p.adj 列
  bind_cols(aov_pvalue)                                       # 合并 Tukey 检验的结果

# ========== 绘制图形 ==========
df %>%
  mutate(`name` = str_replace(`name`, "CAR T", "")) %>%       # 去掉 name 中的 "CAR T"
  ggplot(aes(name, value)) +                                  # x 轴为 name，y 轴为 value
  stat_summary(aes(color = name),                             # 误差棒按 name 着色
               fun = mean, geom = "errorbar", width = .2,     # 误差棒：均值 ± 标准差
               fun.max = function(x) mean(x) + sd(x),         # 上界：均值 + 标准差
               fun.min = function(x) mean(x) - sd(x)) +       # 下界：均值 - 标准差
  stat_summary(fun = mean, geom = "crossbar",                 # 均值横线
               width = 0.4, color = "black", size = 0.5) +    # 黑色横线
  geom_jitter(aes(fill = name, color = name, shape = name),   # 散点：按 name 映射填充、颜色、形状
              width = 0.1, height = 0) +                      # 抖动幅度
  stat_pvalue_manual(df_pvalue,                               # 添加显著性标注
                     label = "p_signif",                      # 标签列
                     label.size = 5,                          # 标签大小
                     hide.ns = T) +                           # 隐藏不显著的标注
  scale_shape_manual(values = c(21, 22, 23, 24)) +            # 手动指定点形状
  scale_fill_manual(values = c("#679289", "#ee2e31",         # 手动指定填充色
                               "#c9cba3", "#f4c095")) +
  scale_color_manual(values = c("#679289", "#ee2e31",        # 手动指定边框色
                                "#c9cba3", "#f4c095")) +
  scale_y_continuous(guide = "prism_minor",                  # y 轴使用 Prism 风格次刻度
                     limits = c(0, 40),                      # y 轴范围
                     expand = c(0, 0)) +                     # y 轴不留空白
  labs(x = NULL, y = "CAR T cells x (10<sup>3</sup>/g)") +   # 轴标签（支持 HTML 上下标）
  theme_prism() +                                            # 使用 Prism 风格主题
  theme(
    strip.background = element_blank(),                      # 分面标题背景透明
    legend.background = element_rect(color = NA),            # 图例背景无边框
    legend.key = element_blank(),                            # 图例键背景透明
    legend.spacing.x = unit(-0.09, "in"),                    # 图例水平间距（负值缩小）
    legend.spacing.y = unit(-0.09, "in"),                    # 图例垂直间距
    legend.text = element_text(color = "black", size = 6,    # 图例文字
                               face = "bold"),
    legend.position = "top",                                 # 图例放在顶部
    axis.text.x = element_blank(),                           # x 轴文字不显示
    axis.text.y = element_text(color = "black", size = 8),   # y 轴文字
    axis.title.y = element_markdown(color = "black",         # y 轴标题支持富文本
                                    face = "bold", size = 10),
    axis.ticks.x = element_blank()                           # x 轴刻度不显示
  ) +
  guides(shape = guide_legend(override.aes = list(size = 3), # 图例中点的大小
                              direction = "horizontal",      # 图例水平排列
                              nrow = 3, byrow = TRUE))       # 分 3 行

# ========== 保存图片 ==========
ggsave("方差误差.pdf", width = 6, height = 6)                 # 保存为 PDF，6×6 英寸