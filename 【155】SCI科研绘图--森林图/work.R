library(tidyverse)    # 数据处理和绘图核心包（含 ggplot2、dplyr、readr 等）
library(GGally)       # 提供 ggpairs 等，这里可能没直接用到，但可能是为了其他函数

# 读取单因素 Cox 回归结果（各癌种的 HR、置信区间、p 值等）
unicox <- read_csv("AKT3_mRNA_OS_pancan_unicox.csv")

# ==================== p1：森林图主图 ====================
p1 <- ggplot(unicox, aes(HR_log, cancer, col = Type)) +   # x=HR_log，y=cancer，颜色按 Type
  geom_point(aes(size = -log10(p.value))) +                # 点大小按 -log10(p) 映射，显著性越强点越大
  geom_errorbarh(aes(xmax = upper_95_log, xmin = lower_95_log),  # 水平误差棒（置信区间）
                 height = 0.4) +                           # 误差棒高度
  scale_x_continuous(limits = c(-2, 2),                    # x 轴范围 -2 到 2
                     breaks = seq(-1, 1, 1)) +             # 刻度 -1、0、1
  # ⚠️ 如果 HR_log 或置信区间超出 -2~2，会被截断为 NA 并产生警告
  geom_vline(aes(xintercept = 0)) +                        # 参考线 x=0（HR=1 对应 logHR=0）
  xlab('HR(95%CI)') + ylab(' ') +                          # x 轴标签；y 轴标签为空
  theme_bw(base_size = 12) +                               # 黑白主题，基础字号 12
  theme(
    axis.text.y = element_blank(),                         # 不显示 y 轴文字（癌症名单独放在 p2）
    axis.ticks.y = element_blank(),                        # 不显示 y 轴刻度
    axis.text.x = element_text(color = "black"),           # x 轴文字黑色
    axis.title.y = element_blank(),                        # 不显示 y 轴标题
    plot.margin = unit(rep(0, 4), "cm"),                   # 四周边距为 0
    legend.title = element_blank(),                        # 图例标题不显示
    legend.key = element_blank(),                          # 图例键背景透明
    legend.text = element_text(color = "black", size = 9), # 图例文字
    legend.spacing.x = unit(0.1, 'cm'),                    # 图例水平间距
    legend.spacing.y = unit(0.1, 'cm'),                    # 图例垂直间距
    legend.key.width = unit(0.2, 'cm'),                    # 图例键宽度
    legend.key.height = unit(0.2, 'cm'),                   # 图例键高度
    legend.background = element_blank(),                   # 图例背景透明
    legend.box.background = element_rect(colour = "black"),# 图例框黑色边框
    legend.position = c(1, 0),                             # 图例位置：右下角
    legend.justification = c(1, 0)                         # 图例对齐：右下角
  ) +
  scale_color_manual(values = c("gray", "steelblue", "red"))  # 手动颜色映射

# ==================== p2：癌症名称文本列 ====================
p2 <- unicox %>% 
  select(cancer) %>%                                       # 只取 cancer 列
  distinct() %>%                                           # 去重（每个癌种只留一行）
  mutate(group = "A") %>%                                  # 添加一列 group，值全为 "A"
  ggplot(aes(group, cancer)) +                             # x=group，y=cancer
  geom_text(aes(group, cancer, label = cancer),            # 在对应位置显示癌症名
            size = 3, color = "black") +
  geom_stripped_rows() +                                   # ❌ 需要 ggpubr 包提供，未加载会报错
  theme(
    panel.grid.major = element_blank(),                    # 不显示主网格线
    axis.text = element_blank(),                           # 不显示轴文字
    axis.ticks = element_blank(),                          # 不显示刻度
    axis.title.y = element_blank(),                        # 不显示 y 轴标题
    plot.margin = unit(rep(0, 4), "cm")                    # 边距为 0
  ) +
  labs(x = "Gene") +                                       # x 轴标题为 "Gene"
  scale_x_discrete(position = "top")                       # x 轴标签放在顶部

# ==================== p3：HR (95% CI) 文本列 ====================
p3 <- unicox %>% 
  select(6, 7, 8) %>%                                      # 选取第 6、7、8 列（应为 HR_log、lower_95_log、upper_95_log）
  mutate(across(where(is.numeric), ~round(., 2))) %>%      # 所有数值列保留 2 位小数
  mutate(`HR(95%Cl)` = paste(HR_log, "[", lower_95_log, " to ", upper_95_log, "]", sep = "")) %>%
  # 拼接成 "HR [lower to upper]" 格式的字符串
  select(4) %>%                                            # 只保留新生成的列（第 4 列）
  mutate(group = "HR(95%Cl)") %>%                          # 添加 group 列
  ggplot(aes(group, `HR(95%Cl)`)) +                        # x=group，y=字符串
  geom_text(aes(group, `HR(95%Cl)`, label = `HR(95%Cl)`),  # 显示文本
            size = 3, color = "black") +
  geom_stripped_rows() +                                   # ❌ 同样需要 ggpubr
  theme(
    panel.grid.major = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    axis.title.y = element_blank(),
    plot.margin = unit(rep(0, 4), "cm")
  ) +
  labs(x = "HR(95%Cl)") +                                  # x 轴标题
  scale_x_discrete(position = "top")                       # 标签在顶部

# ==================== p4：p 值文本列 ====================
p4 <- unicox %>% 
  select(p.value) %>%                                      # 只取 p.value 列
  mutate(across(where(is.numeric), ~round(., 2))) %>%      # 保留 2 位小数
  mutate(group = "A", p.value = as.character(p.value)) %>% # 转字符型，添加 group 列
  ggplot(aes(group, p.value)) +                            # x=group，y=p.value
  geom_text(aes(group, p.value, label = p.value),          # 显示 p 值
            size = 3, color = "black") +
  geom_stripped_rows() +                                   # ❌ 需要 ggpubr
  theme(
    panel.grid.major = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    axis.title.y = element_blank(),
    plot.margin = unit(rep(0, 4), "cm")
  ) +
  labs(x = "p.value") +                                    # x 轴标题
  scale_x_discrete(position = "top")

# ==================== 拼图 ====================
library(patchwork)                                         # 加载 patchwork 用于拼图
(p2 + p3 + p4 + p1) + plot_layout(widths = c(.12, 0.28, .15, .8))
# 将四张图横向拼在一起，宽度比例分别为 0.12、0.28、0.15、0.8

# ==================== 保存 ====================
ggsave("图.pdf", width = 10, height = 6, dpi = 300)