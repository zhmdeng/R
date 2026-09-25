# ==================== 加载包 ====================
library(tidyverse)   # 数据处理和绘图（含 dplyr、tidyr、ggplot2、readr）
library(rstatix)     # 提供 t_test()、adjust_pvalue()、add_significance() 等
library(ggpubr)      # 提供 stat_pvalue_manual() 等绘图工具
library(GGally)      # 提供 geom_stripped_cols()，给列加背景条纹
library(ggsci)       # 提供 JCO、NPG 等期刊配色

# ==================== 读取数据 ====================
df <- read_csv("easy_input.csv")   # 读取 CSV 数据

# ==================== 统计检验 ====================
test <- df %>%
  group_by(tissue) %>%                          # 按 tissue 分组
  t_test(tpm ~ type2) %>%                       # 对每个 tissue 做 t 检验（tpm ~ type2）
  adjust_pvalue() %>%                           # 校正 p 值（默认 BH 法）
  add_significance("p.adj") %>%                 # 添加显著性符号（***、**、*、ns）
  add_xy_position(x = "tissue") %>%             # 计算显著性标注的位置
  select(-y.position) %>%                       # 删除自动计算的 y 位置
  mutate(y.position = 10)                       # 手动指定 y 位置为 10

# ==================== 准备背景条纹数据 ====================
df2 <- df %>%
  left_join(.,                                  # 左连接
            test %>% select(tissue, p.adj.signif) %>%   # 从 test 中提取 tissue 和显著性
              mutate(group = case_when(                 # 根据显著性分组
                p.adj.signif == "ns" ~ "B",             # 不显著 → B
                TRUE ~ "A"                              # 显著 → A
              )),
            by = "tissue") %>%
  select(-p.adj.signif)                         # 删除 p.adj.signif 列

# ==================== 绘图 ====================
df %>% ggplot(aes(tissue, tpm)) +               # x=tissue，y=tpm
  
  # ---- 误差棒 ----
stat_boxplot(aes(fill = type2),               # 按 type2 填充
             geom = "errorbar",               # 画误差棒
             position = position_dodge(width = 0.6),  # 躲开宽度 0.6
             width = 0.2) +                   # 误差棒宽度
  
  # ---- 箱线图 ----
geom_boxplot(aes(fill = type2),               # 按 type2 填充
             position = position_dodge(width = 0.6),  # 躲开宽度 0.6
             width = 0.4,                     # 箱体宽度
             outlier.shape = NA) +            # 不显示离群点
  
  # ---- 显著性标注 ----
stat_pvalue_manual(test,                      # 统计检验结果
                   label = "p.adj.signif",    # 标签列：显著性符号
                   label.y = 10,              # y 位置
                   label.size = 4,            # 标签大小
                   hide.ns = T,               # 隐藏不显著的标注
                   tip.length = 0) +          # 横线长度 0（不显示横线）
  
  # ---- 轴标签 ----
labs(x = NULL, y = NULL) +                    # 不显示轴标签
  
  # ---- 背景条纹 ----
geom_stripped_cols(data = df2 %>% filter(group == "A"),  # 只给显著的 tissue 加条纹
                   odd = "#33333333",         # 奇数条纹颜色（带透明度）
                   even = "#33333333") +      # 偶数条纹颜色
  
  # ---- 配色 ----
scale_fill_jco() +                            # 使用 JCO 配色
  
  # ---- 主题 ----
theme_test() +                                # 基础主题：test
  theme(
    plot.margin = unit(c(0.2, 0.2, 0.2, 0.2),   # 图形边距
                       units = "cm"),           # 单位：厘米（原代码漏了 "cm"）
    axis.line = element_line(color = "black",   # 轴线颜色
                             size = 0.4),       # 轴线粗细
    panel.grid.minor = element_blank(),         # 不显示次网格线
    panel.grid.major = element_line(size = 0.2, # 主网格线
                                    color = "#e5e5e5"),  # 浅灰色
    axis.text.y = element_text(color = "black", # y 轴文字黑色
                               size = 10),      # 字号 10
    axis.text.x = element_text(margin = margin(t = 0.5),  # x 轴文字上边距
                               color = "black", # 黑色
                               size = 10),      # 字号 10
    legend.position = "none",                   # 不显示图例
    panel.spacing = unit(0, "lines")            # 分面间距 0
  )

# ==================== 保存图片 ====================
ggsave("组合箱图2.pdf", width = 15, height = 5, dpi = 300)