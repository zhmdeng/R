rm(list = ls())   # 清空当前 R 环境中所有对象
# rm() 是删除对象的函数
# list = ls() 表示要删除的对象列表是当前环境所有对象
# ls() 列出当前环境所有对象的名称

# setwd('')   # 设置工作目录

## ==================== 加载 R 包 ====================
library(ggplot2)   # 加载 ggplot2 包，用于绘图
# library() 加载已安装的包
# ggplot2 是绘图核心包

library(reshape2)  # 加载 reshape2 包，用于数据变形
# 主要用到 melt() 函数，把宽表转成长表

library(ggpmisc)   # 加载 ggpmisc 包，用于扩展 ggplot2
# 主要用到 stat_poly_eq() 函数，显示回归方程和 p 值

## ==================== 读取数据 ====================
df <- read.table(file = "data.txt",   # 要读取的文件名
                 sep = "\t",           # 分隔符：制表符
                 header = T,           # 第一行是列名
                 check.names = FALSE,  # 不修改列名中的特殊字符
                 row.names = 1)        # 第一列作为行名
# read.table() 是基础 R 读取表格文件的函数
# 返回值赋给 df

df$Sample <- rownames(df)   # 把 df 的行名（样本名）添加为 Sample 列
# $ 用于访问数据框的列
# rownames() 返回行名

## ==================== Z-score 标准化 ====================
df1 <- df[2:5]   # 提取 df 的第 2 到第 5 列
# [] 是数据框索引
# 2:5 表示第 2、3、4、5 列（即 4 个多样性指数）

df_zscore <- as.data.frame(scale(df1))   # 对 df1 做 Z-score 标准化
# scale() 对每一列做标准化（减均值、除标准差）
# as.data.frame() 把矩阵转为数据框

df_zscore$Sample <- rownames(df_zscore)   # 给标准化后的数据添加 Sample 列

data <- merge(df[c(1, 6)], df_zscore, by = "Sample")
# merge() 合并两个数据框
# df[c(1, 6)] 取 df 的第 1 列和第 6 列（Period 和 Sample）
# df_zscore 是标准化后的数据
# by = "Sample" 按 Sample 列合并

## ==================== 宽转长 ====================
data2 <- melt(data,                        # 要转换的数据框
              id.vars = c("Sample", "Period"),   # 作为标识列的列
              measure.vars = c('ACE', 'Chao', 'Shannon', 'Simpson'))
# melt() 把宽表转成长表
# id.vars 指定哪些列保持不变
# measure.vars 指定哪些列要转成长表的 value

data2$Period <- factor(data2$Period,       # 要转为因子的列
                       levels = c("period1", "period2", "period3",
                                  "period4", "period5"))
# factor() 把字符转为因子
# levels 指定水平顺序

data2$variable <- factor(data2$variable,
                         levels = c("ACE", "Chao", "Shannon", "Simpson"))
# 同上，固定 variable 的水平顺序

## ==================== 构造自定义 x 轴 ====================
data2$x <- rep(c(1:5, 7:11, 13:17, 19:23),   # 要重复的向量
               each = 10)
# rep() 重复向量
# c(1:5, 7:11, 13:17, 19:23) 是 20 个数字
# each = 10 表示每个数字重复 10 次
# 结果赋给 data2 的 x 列

## ==================== 计算组间显著性 ====================
df2 <- melt(df,                            # 用原始数据 df
            id.vars = c("Sample", "Period"),
            measure.vars = c('ACE', 'Chao', 'Shannon', 'Simpson'))

df2$Period <- factor(df2$Period,
                     levels = c("period1", "period2", "period3",
                                "period4", "period5"))

df2$variable <- factor(df2$variable,
                       levels = c("ACE", "Chao", "Shannon", "Simpson"))

## ---- ACE 组内比较 ----
ACE_results_matrix <- matrix(NA,           # 创建一个矩阵，初始值为 NA
                             nrow = 5,     # 5 行
                             ncol = 5)     # 5 列
# matrix() 创建矩阵
# NA 是初始填充值

rownames(ACE_results_matrix) <- colnames(ACE_results_matrix) <- paste0("period", 1:5)
# rownames() 设置行名
# colnames() 设置列名
# paste0() 拼接字符串，不添加分隔符
# 1:5 生成 1 到 5

for (i in 1:5) {                           # 外层循环：i 从 1 到 5
  for (j in 1:5) {                         # 内层循环：j 从 1 到 5
    if (i == j) {                          # 如果 i 等于 j
      next                                 # 跳过本次循环
    } else {                               # 否则
      x <- subset(df2,                     # 从 df2 中取子集
                  variable == "ACE" &      # 条件：variable 为 ACE
                    Period == paste0("period", i))$value
      # subset() 按条件筛选数据
      # $value 取 value 列
      
      y <- subset(df2,
                  variable == "ACE" &
                    Period == paste0("period", j))$value
      
      result <- wilcox.test(x, y,          # 对 x 和 y 做 Wilcoxon 检验
                            exact = FALSE) # exact = FALSE 用近似方法
      # wilcox.test() 秩和检验
      
      p_value <- result$p.value            # 提取 p 值
      
      ACE_results_matrix[i, j] <- p_value  # 存入矩阵
    }
  }
}
ACE_results_matrix <- as.data.frame(ACE_results_matrix)   # 矩阵转数据框

## ---- Chao 组内比较 ----
Chao_results_matrix <- matrix(NA, nrow = 5, ncol = 5)
rownames(Chao_results_matrix) <- colnames(Chao_results_matrix) <- paste0("period", 1:5)

for (i in 1:5) {
  for (j in 1:5) {
    if (i == j) {
      next
    } else {
      x <- subset(df2, variable == "Chao" & Period == paste0("period", i))$value
      y <- subset(df2, variable == "Chao" & Period == paste0("period", j))$value
      result <- wilcox.test(x, y, exact = FALSE)
      p_value <- result$p.value
      Chao_results_matrix[i, j] <- p_value
    }
  }
}
Chao_results_matrix <- as.data.frame(Chao_results_matrix)

## ---- Shannon 组内比较 ----
Shannon_results_matrix <- matrix(NA, nrow = 5, ncol = 5)
rownames(Shannon_results_matrix) <- colnames(Shannon_results_matrix) <- paste0("period", 1:5)

for (i in 1:5) {
  for (j in 1:5) {
    if (i == j) {
      next
    } else {
      x <- subset(df2, variable == "Shannon" & Period == paste0("period", i))$value
      y <- subset(df2, variable == "Shannon" & Period == paste0("period", j))$value
      result <- wilcox.test(x, y, exact = FALSE)
      p_value <- result$p.value
      Shannon_results_matrix[i, j] <- p_value
    }
  }
}
Shannon_results_matrix <- as.data.frame(Shannon_results_matrix)

## ---- Simpson 组内比较 ----
Simpson_results_matrix <- matrix(NA, nrow = 5, ncol = 5)
rownames(Simpson_results_matrix) <- colnames(Simpson_results_matrix) <- paste0("period", 1:5)

for (i in 1:5) {
  for (j in 1:5) {
    if (i == j) {
      next
    } else {
      x <- subset(df2, variable == "Simpson" & Period == paste0("period", i))$value
      y <- subset(df2, variable == "Simpson" & Period == paste0("period", j))$value
      result <- wilcox.test(x, y, exact = FALSE)
      p_value <- result$p.value
      Simpson_results_matrix[i, j] <- p_value
    }
  }
}
Simpson_results_matrix <- as.data.frame(Simpson_results_matrix)

## ==================== 绘图 ====================
ggplot() +   # 创建空白 ggplot 对象
  # ggplot() 初始化图形，后续用 + 添加图层
  
  ## ---- ACE 箱线图 ----
geom_boxplot(data = data2[data2$variable == "ACE", ],   # 数据：只取 ACE 行
             aes(x, value, fill = Period),              # 映射：x 轴为 x，y 为 value，填充为 Period
             outlier.color = NA) +                      # 隐藏离群点
  # geom_boxplot() 画箱线图
  # outlier.color = NA 让离群点透明（即不显示）
  
  ## ---- Chao 箱线图 ----
geom_boxplot(data = data2[data2$variable == "Chao", ],
             aes(x, value, fill = Period),
             outlier.color = NA) +
  
  ## ---- Shannon 箱线图 ----
geom_boxplot(data = data2[data2$variable == "Shannon", ],
             aes(x, value, fill = Period),
             outlier.color = NA) +
  
  ## ---- Simpson 箱线图 ----
geom_boxplot(data = data2[data2$variable == "Simpson", ],
             aes(x, value, fill = Period),
             outlier.color = NA) +
  
  ## ---- ACE 散点 ----
geom_jitter(data = data2[data2$variable == "ACE", ],   # 数据：ACE 行
            aes(x, value, fill = Period),              # 映射同上
            color = "grey30",                          # 点的边框色
            size = 1.5,                                # 点大小
            width = 0.3,                               # 水平抖动幅度
            shape = 21) +                              # 点形状：21 是带填充的圆
  # geom_jitter() 添加抖动散点
  
  ## ---- Chao 散点 ----
geom_jitter(data = data2[data2$variable == "Chao", ],
            aes(x, value, fill = Period),
            color = "grey30", size = 1.5, width = 0.3, shape = 21) +
  
  ## ---- Shannon 散点 ----
geom_jitter(data = data2[data2$variable == "Shannon", ],
            aes(x, value, fill = Period),
            color = "grey30", size = 1.5, width = 0.3, shape = 21) +
  
  ## ---- Simpson 散点 ----
geom_jitter(data = data2[data2$variable == "Simpson", ],
            aes(x, value, fill = Period),
            color = "grey30", size = 1.5, width = 0.3, shape = 21) +
  
  ## ---- ACE 线性回归拟合 ----
geom_smooth(data = data2[data2$variable == "ACE", ],   # 数据：ACE 行
            aes(x, value),                             # 映射：x 和 value
            method = "lm",                             # 方法：线性回归
            se = F,                                    # 不显示置信区间
            formula = y ~ x,                           # 公式：y 对 x 回归
            linewidth = 0.8,                           # 线宽
            linetype = 1) +                            # 线型：实线
  # geom_smooth() 添加拟合曲线
  
  ## ---- Chao 线性回归拟合 ----
geom_smooth(data = data2[data2$variable == "Chao", ],
            aes(x, value), method = "lm", se = F,
            formula = y ~ x, linewidth = 0.8, linetype = 1) +
  
  ## ---- Shannon 线性回归拟合 ----
geom_smooth(data = data2[data2$variable == "Shannon", ],
            aes(x, value), method = "lm", se = F,
            formula = y ~ x, linewidth = 0.8, linetype = 1) +
  
  ## ---- Simpson 线性回归拟合 ----
geom_smooth(data = data2[data2$variable == "Simpson", ],
            aes(x, value), method = "lm", se = F,
            formula = y ~ x, linewidth = 0.8, linetype = 1) +
  
  ## ---- ACE 回归 p 值标注 ----
stat_poly_eq(data = data2[data2$variable == "ACE", ],   # 数据：ACE 行
             aes(x, value,                              # 映射：x、value
                 label = after_stat(p.value.label)),    # 标签：p 值
             formula = y ~ x,                           # 回归公式
             parse = TRUE,                              # 解析为表达式
             label.x = 0.1,                             # 标签 x 位置（相对坐标）
             label.y = 0.1,                             # 标签 y 位置
             size = 4,                                  # 文字大小
             color = "black") +                         # 文字颜色
  # stat_poly_eq() 添加回归方程和 p 值
  
  ## ---- Chao 回归 p 值标注 ----
stat_poly_eq(data = data2[data2$variable == "Chao", ],
             aes(x, value, label = after_stat(p.value.label)),
             formula = y ~ x, parse = TRUE,
             label.x = 0.35, label.y = 0.1, size = 4, color = "black") +
  
  ## ---- Shannon 回归 p 值标注 ----
stat_poly_eq(data = data2[data2$variable == "Shannon", ],
             aes(x, value, label = after_stat(p.value.label)),
             formula = y ~ x, parse = TRUE,
             label.x = 0.65, label.y = 0.1, size = 4, color = "black") +
  
  ## ---- Simpson 回归 p 值标注 ----
stat_poly_eq(data = data2[data2$variable == "Simpson", ],
             aes(x, value, label = after_stat(p.value.label)),
             formula = y ~ x, parse = TRUE,
             label.x = 0.9, label.y = 0.1, size = 4, color = "black") +
  
  ## ---- 各组下方短横线 ----
geom_segment(data = data2,                              # 数据
             aes(x = 1, xend = 5,                       # 起点 x 和终点 x
                 y = -2.2, yend = -2.2),                # 起点 y 和终点 y
             color = "black",                           # 颜色
             linewidth = 0.6) +                         # 线宽
  # geom_segment() 画线段
  
  geom_segment(data = data2, aes(x = 7, xend = 11, y = -2.2, yend = -2.2),
               color = "black", linewidth = 0.6) +
  
  geom_segment(data = data2, aes(x = 13, xend = 17, y = -2.2, yend = -2.2),
               color = "black", linewidth = 0.6) +
  
  geom_segment(data = data2, aes(x = 19, xend = 23, y = -2.2, yend = -2.2),
               color = "black", linewidth = 0.6) +
  
  ## ---- 显著性横线（第 1 组） ----
geom_segment(data = data2, aes(x = 1, xend = 2, y = 0.3, yend = 0.3),
             color = "black", linewidth = 0.6) +
  annotate("text",                # 添加文字
           x = 1.5,               # x 坐标
           y = 0.4,               # y 坐标
           label = "*",           # 文字内容
           size = 5,              # 文字大小
           color = "black") +     # 文字颜色
  # annotate() 手动添加注释
  
  geom_segment(data = data2, aes(x = 4, xend = 5, y = 2.3, yend = 2.3),
               color = "black", linewidth = 0.6) +
  annotate("text", x = 4.5, y = 2.4, label = "*", size = 5, color = "black") +
  
  ## ---- 显著性横线（第 2 组） ----
geom_segment(data = data2, aes(x = 7, xend = 8, y = -0.3, yend = -0.3),
             color = "black", linewidth = 0.6) +
  annotate("text", x = 7.5, y = 0, label = "**", size = 5, color = "black") +
  
  geom_segment(data = data2, aes(x = 9, xend = 10, y = 1.5, yend = 1.5),
               color = "black", linewidth = 0.6) +
  annotate("text", x = 9.5, y = 1.6, label = "*", size = 5, color = "black") +
  
  ## ---- 显著性横线（第 3 组） ----
geom_segment(data = data2, aes(x = 16, xend = 17, y = 2, yend = 2),
             color = "black", linewidth = 0.6) +
  annotate("text", x = 16.5, y = 2.1, label = "**", size = 5, color = "black") +
  
  ## ---- 显著性横线（第 4 组） ----
geom_segment(data = data2, aes(x = 21, xend = 22, y = 1, yend = 1),
             color = "black", linewidth = 0.6) +
  annotate("text", x = 21.5, y = 1.1, label = "**", size = 5, color = "black") +
  
  ## ---- x 轴设置 ----
scale_x_continuous(breaks = c(3, 9, 15, 21),                # x 轴刻度位置
                   labels = c("ACE", "Chao", "Shannon", "Simpson")) +
  # scale_x_continuous() 设置连续型 x 轴
  
  scale_y_continuous(limits = c(-2.8, 2.5)) +                 # y 轴范围
  # scale_y_continuous() 设置连续型 y 轴
  
  labs(x = NULL,                                              # x 轴标题（NULL 表示不显示）
       y = "Alpha diversity index") +                         # y 轴标题
  # labs() 设置标签
  
  ## ---- 组间分割线 ----
geom_vline(xintercept = 6,                                  # x 轴位置
           linetype = 2,                                    # 线型：2 是虚线
           color = "black",                                 # 颜色
           linewidth = 0.8) +                               # 线宽
  # geom_vline() 画垂直线
  
  geom_vline(xintercept = 12, linetype = 2, color = "black", linewidth = 0.8) +
  geom_vline(xintercept = 18, linetype = 2, color = "black", linewidth = 0.8) +
  
  ## ---- 主题设置 ----
theme_bw() +                                                # 使用黑白主题
  theme(panel.grid = element_blank(),                         # 不显示网格线
        axis.text.x = element_text(angle = 45,                # x 轴文字旋转 45 度
                                   vjust = 1,                 # 垂直对齐
                                   hjust = 1,                 # 水平对齐
                                   size = 12),                # 文字大小
        axis.text.y = element_text(size = 12),                # y 轴文字大小
        axis.title = element_text(size = 14),                 # 轴标题大小
        legend.position = c(0.95, 0.8),                       # 图例位置
        legend.background = element_blank()) +                # 图例背景透明
  
  ## ---- 自定义填充色 ----
scale_fill_manual(values = c("#ff3c41", "#fbb034", "#fcd000",
                             "#47cf73", "#0ebeff"))
# scale_fill_manual() 手动指定填充色

## ==================== 保存 ====================
ggsave("箱线图+组内显著+组内线性回归.pdf",   # 文件名
       width = 12,                          # 宽度（英寸）
       height = 5,                          # 高度（英寸）
       dpi = 300)                           # 分辨率
# ggsave() 保存图片