rm(list = ls())          # 清空当前环境中的所有对象
# setwd("")              # 设置工作目录（已注释，需要时取消注释并填入路径）

# ==================== 加载包 ====================
library(ggplot2)         # 绘图核心包
library(dplyr)           # 数据处理
library(stringr)         # 字符串处理（str_sub）
library(ggpmisc)         # 提供 stat_poly_eq()，显示回归方程和 R²
library(Hmisc)           # 提供 mean_cl_normal、mean_sdl 等汇总函数

# ==================== 读取数据 ====================
df <- read.table("data.txt", sep = "\t", header = T, check.names = F)
# 读取制表符分隔的数据，第一行为列名，不修改列名
# 数据应包含 group（分组）和 value（数值）两列

# ==================== 提取分组编号 ====================
df$G <- str_sub(df$group, 6, 6)   # 提取 group 字符串的第 6 个字符（假设形如 "group1"）
df$G <- as.numeric(df$G)          # 转为数值型，用于后续拟合曲线
# ⚠️ 如果 group 格式不是 "group1" 这种，第 6 位可能取不到数字，会变成 NA

# ==================== 计算每组的均值及其 x 轴位置 ====================
df %>%
  group_by(group) %>%
  summarise(mean_value = mean(value)) %>%   # 计算每组的均值
  bind_cols(x = c(1:5)) -> df1              # 给每组分配一个 x 位置（1 到 5）
# ⚠️ 这里硬编码了 1:5，如果分组数不是 5，会报错或错位

# ==================== p1：分组散点 + 拟合曲线 ====================
p1 <- ggplot(df, aes(group, value)) +
  geom_jitter(aes(fill = group), shape = 21, color = "black",
              width = 0.3, size = 3, alpha = 0.4) +   # 抖动散点，形状 21 可填充
  geom_segment(data = df1,
               aes(x = x - 0.2, xend = x + 0.2,
                   y = mean_value, yend = mean_value),
               color = "grey20", linewidth = 0.8) +       # 每组均值短横线
  stat_summary(color = "grey10", fun.data = "mean_cl_normal",
               geom = "errorbar",
               width = 0.15, linewidth = 0.8) +           # 均值 ± 95% 置信区间误差棒
  geom_smooth(aes(x = G, y = value),                     # 用数值 G 作为 x 拟合
              method = "lm", color = "#ee4f4f",
              level = 0.95,
              formula = y ~ poly(x, 2, raw = TRUE),
              linetype = 1, alpha = 0.2, linewidth = 1) + # 二次多项式拟合
  stat_poly_eq(formula = y ~ poly(x, 2, raw = TRUE),
               aes(x = G, y = value,
                   label = paste(after_stat(eq.label),
                                 after_stat(adj.rr.label),
                                 sep = "~~~")),
               parse = TRUE, label.x = 0.05, label.y = 0.95,
               size = 3.5, color = "black") +             # 显示回归方程和调整 R²
  labs(x = "This is x-axis", y = "This is y-axis") +      # 轴标题
  scale_fill_manual(values = c("#00b2a9","#a626aa","#6639b7","#aea400","#ff6319")) +
  theme_bw() +
  theme(legend.position = "none",
        panel.grid = element_blank(),
        axis.text.x = element_text(size = 10, angle = 45, vjust = 1, hjust = 1),
        axis.text.y = element_text(size = 10),
        axis.title = element_text(size = 12, color = "black"))
p1

# ==================== p2：分组箱线图 + 拟合曲线 ====================
p2 <- ggplot(df, aes(group, value)) +
  geom_boxplot(aes(color = group)) +                      # 箱线图，按 group 着色
  geom_jitter(aes(color = group), width = 0.3, size = 1.5) +  # 散点
  geom_smooth(aes(x = G, y = value),                      # 同样用数值 G 拟合
              method = "lm", color = "#ee4f4f",
              level = 0.95,
              formula = y ~ poly(x, 2, raw = TRUE),
              linetype = 1, alpha = 0.2, linewidth = 1) +
  stat_poly_eq(formula = y ~ poly(x, 2, raw = TRUE),
               aes(x = G, y = value,
                   label = paste(after_stat(eq.label),
                                 after_stat(adj.rr.label),
                                 sep = "~~~")),
               parse = TRUE, label.x = 0.05, label.y = 0.95,
               size = 3.5, color = "black") +
  labs(x = "This is x-axis", y = "This is y-axis") +
  scale_color_manual(values = c("#00b2a9","#a626aa","#6639b7","#aea400","#ff6319")) +
  theme_bw() +
  theme(legend.position = "none",
        panel.grid = element_blank(),
        axis.text.x = element_text(size = 10, angle = 45, vjust = 1, hjust = 1),
        axis.text.y = element_text(size = 10),
        axis.title = element_text(size = 12, color = "black"))
p2

# ==================== p3：分组小提琴图 + 拟合曲线 ====================
p3 <- ggplot(df, aes(group, value)) +
  geom_violin(aes(fill = group), trim = FALSE) +          # 小提琴图，不裁剪尾部
  geom_jitter(color = "black", fill = "white", shape = 21,
              width = 0.3, size = 2.5) +                  # 白底黑边散点
  geom_smooth(aes(x = G, y = value),                      # 同样用数值 G 拟合
              method = "lm", color = "#ee4f4f",
              level = 0.95,
              formula = y ~ poly(x, 2, raw = TRUE),
              linetype = 1, alpha = 0.2, linewidth = 1) +
  stat_poly_eq(formula = y ~ poly(x, 2, raw = TRUE),
               aes(x = G, y = value,
                   label = paste(after_stat(eq.label),
                                 after_stat(adj.rr.label),
                                 sep = "~~~")),
               parse = TRUE, label.x = 0.05, label.y = 0.95,
               size = 3.5, color = "black") +
  labs(x = "This is x-axis", y = "This is y-axis") +
  scale_fill_manual(values = c("#00b2a9","#a626aa","#6639b7","#aea400","#ff6319")) +
  theme_bw() +
  theme(legend.position = "none",
        panel.grid = element_blank(),
        axis.text.x = element_text(size = 10, angle = 45, vjust = 1, hjust = 1),
        axis.text.y = element_text(size = 10),
        axis.title = element_text(size = 12, color = "black"))
p3

# ==================== p4：分组条形图 + 拟合曲线 ====================
p4 <- ggplot(df, aes(group, value)) +
  stat_summary(fun.data = 'mean_sdl', geom = "errorbar",
               width = 0.15, linewidth = 0.8) +           # 均值 ± 标准差误差棒
  geom_bar(aes(fill = group), stat = "summary", fun = mean,
           position = "dodge", size = 0.5) +              # 柱状图（均值）
  geom_jitter(fill = "white", color = "black", shape = 21,
              width = 0.3, size = 2.5) +                  # 散点
  geom_smooth(aes(x = G, y = value),                      # 同样用数值 G 拟合
              method = "lm", color = "#ee4f4f",
              level = 0.95,
              formula = y ~ poly(x, 2, raw = TRUE),
              linetype = 1, alpha = 0.2, linewidth = 1) +
  stat_poly_eq(formula = y ~ poly(x, 2, raw = TRUE),
               aes(x = G, y = value,
                   label = paste(after_stat(eq.label),
                                 after_stat(adj.rr.label),
                                 sep = "~~~")),
               parse = TRUE, label.x = 0.05, label.y = 0.95,
               size = 3.5, color = "black") +
  labs(x = "This is x-axis", y = "This is y-axis") +
  scale_fill_manual(values = c("#00b2a9","#a626aa","#6639b7","#aea400","#ff6319")) +
  scale_y_continuous(expand = c(0, 0)) +                  # y 轴从 0 开始，不留空白
  theme_bw() +
  theme(legend.position = "none",
        panel.grid = element_blank(),
        axis.text.x = element_text(size = 10, angle = 45, vjust = 1, hjust = 1),
        axis.text.y = element_text(size = 10),
        axis.title = element_text(size = 12, color = "black"))
p4

# ==================== 拼图 ====================
library(patchwork)        # 加载 patchwork 用于拼图
(p1 + p2) / (p3 + p4)     # 2×2 布局

# ==================== 保存 ====================
ggsave("图.pdf", dpi = 300)   # 保存为 PDF，默认尺寸，300 dpi