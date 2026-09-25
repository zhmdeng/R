# ==================== 加载包 ====================
library(tidyverse)   # 数据处理和绘图（含 dplyr、tidyr、ggplot2、readr）
library(rstatix)     # 提供 t_test() 等统计检验
library(ggpubr)      # 提供 add_pvalue() 等绘图工具
library(ggprism)     # Prism 风格主题和坐标轴

# ==================== 设置工作目录 ====================
setwd("/Users/mac/Desktop/PowerBI/sci科研绘图/【37】SCI科研绘图--添加p值/")

# ==================== 读取并汇总数据 ====================
df <- read_tsv('F1-a.txt') %>%                    # 读取数据
  pivot_longer(-`MUFA-PI / total PI [%]`) %>%     # 宽转长：除该列外都转成长表
  group_by(name) %>%                              # 按 name 分组
  summarise(
    value_mean = mean(value),                     # 计算均值
    sd         = sd(value),                       # 计算标准差
    se         = sd(value) / sqrt(n())            # 计算标准误
  ) %>%
  mutate(
    group = case_when(
      name == "w/o"  ~ "black",                   # w/o 组 → black
      name == "TNFα" ~ "grey",                    # TNFα 组 → grey
      name == "I3M"  ~ "grey",                    # I3M 组 → grey
      TRUE           ~ "green"                    # 其他 → green
    )
  )

# ==================== 固定 x 轴顺序 ====================
df$name <- factor(
  df$name,
  levels = read_tsv('F1-a.txt') %>%               # 重新读取数据
    pivot_longer(-`MUFA-PI / total PI [%]`) %>%   # 宽转长
    select(name) %>%                              # 只取 name 列
    distinct() %>%                                # 去重
    pull()                                        # 转为向量
)
# 保证 x 轴顺序与原数据中出现的顺序一致

# ==================== 统计检验 ====================
stat.test <- read_tsv('F1-a.txt') %>%             # 读取数据
  pivot_longer(-`MUFA-PI / total PI [%]`) %>%     # 宽转长
  t_test(data = ., value ~ name, ref.group = "w/o") %>%   # 以 w/o 为参照做 t 检验
  mutate(
    p.adj.signif = replace_na(p.adj.signif, ""),  # NA 替换为空字符串
    across("p.adj.signif", str_replace, "ns", "") # "ns" 替换为空
  ) %>%
  select(group1, group2, p.adj, p.adj.signif) %>% # 只保留需要的列
  left_join(., df, by = c("group2" = "name")) %>%  # 按 group2 和 name 合并
  mutate(y.position = value_mean + se + 0.3)       # 计算 p 值标注的 y 位置

# ==================== 自定义主题 ====================
theme_niwot <- function() {
  theme_test() +                                   # 基础主题：test
    theme(
      axis.title = element_blank(),                # 不显示轴标题
      axis.ticks.x = element_blank(),              # 不显示 x 轴刻度
      panel.grid.major.y = element_line(color = "#DAE1E7"),  # y 方向主网格线
      panel.grid.major.x = element_blank(),        # 不显示 x 方向网格线
      plot.margin = unit(rep(0.2, 4), "cm"),       # 图形边距：上下左右各 0.2 cm
      axis.text = element_text(size = 10, color = "#22292F"),  # 轴文字
      axis.text.y = element_text(margin = margin(r = 5)),      # y 轴文字右边距
      axis.text.x = element_text(margin = margin(t = 5)),      # x 轴文字上边距
      legend.position = "non"                      # 图例位置（应为 "none"，写错但不影响）
    )
}

# ==================== 绘图 ====================
df %>% ggplot(., aes(name, value_mean)) +          # x=name，y=value_mean
  geom_errorbar(aes(ymax = value_mean + se,        # 误差棒上界
                    ymin = value_mean - se),       # 误差棒下界
                width = 0.2,                        # 误差棒宽度
                color = "grey30") +                 # 误差棒颜色
  geom_col(width = 0.5,                             # 柱状图，宽度 0.5
           aes(fill = group),                       # 填充色按 group 映射
           color = "grey50") +                      # 柱子边框色
  add_pvalue(stat.test,                             # 添加 p 值标注
             label = "p.adj.signif",                # 标签列
             label.size = 6,                        # 标签大小
             coord.flip = TRUE,                     # 坐标翻转（p 值在柱子上方）
             remove.bracket = TRUE) +               # 移除显著性括号
  scale_y_continuous(expand = c(0, 0),              # y 轴不留空白
                     limits = c(0, 57)) +           # y 轴范围 0-57
  theme_niwot() +                                   # 应用自定义主题
  scale_fill_brewer(palette = "Blues")              # 使用 Blues 配色

# ==================== 保存图片 ====================
ggsave("添加p值.pdf", width = 5, height = 5, dpi = 300)