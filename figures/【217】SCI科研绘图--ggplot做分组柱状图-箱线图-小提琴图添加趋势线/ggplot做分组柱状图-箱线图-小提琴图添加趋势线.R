# setwd('')
# 设置工作目录

library(RColorBrewer)   # 配色方案
library(ggpubr)         # 提供 stat_compare_means()、ggarrange() 等
library(ggplot2)        # 绘图核心包
library(cowplot)        # 提供 plot_grid()，用于拼图

# ==================== 读取并整理数据 ====================
Exp <- read.csv("Exp.csv", header = T, row.names = 1)
# 读取表达矩阵，行名是基因名，列名是样本

gene <- c("CD28","CD3D","CD8A","LCK",
          "GATA3","EOMES","IL23A","CXCL8",
          "IL1R2","IL1R1","MMP8","MMP9")
# 挑选 12 个基因用于绘图

Exp <- log2(Exp + 1)          # FPKM 数据 log2 转换（+1 避免 log(0)）
Exp_plot <- Exp[, gene]        # 只保留选中的基因

info <- read.csv("info.csv", header = T)   # 读取样本分组信息
Exp_plot <- Exp_plot[info$Sample, ]        # 按 info 中样本顺序重排表达矩阵
Exp_plot$sam <- info$Type                  # 添加分组列
Exp_plot$sam <- factor(Exp_plot$sam,
                       levels = c("Asymptomatic", "Mild", "Severe", "Critical"))
# 固定分组的因子顺序

col <- c("#5CB85C", "#337AB7", "#F0AD4E", "#D9534F")   # 4 组颜色

# ==================== 第一组图：小提琴图 + 散点 + 线性拟合 ====================
plist2 <- list()                              # 创建空列表存放图形

for (i in 1:length(gene)) {                   # 遍历 12 个基因
  df <- Exp_plot[, c(gene[i], "sam")]         # 提取当前基因和分组
  colnames(df) <- c("Expression", "sam")      # 统一列名
  
  # 定义 6 组两两比较
  my_comparisons1 <- list(c("Asymptomatic", "Mild"))
  my_comparisons2 <- list(c("Asymptomatic", "Severe"))
  my_comparisons3 <- list(c("Asymptomatic", "Critical"))
  my_comparisons4 <- list(c("Mild", "Severe"))
  my_comparisons5 <- list(c("Mild", "Critical"))
  my_comparisons6 <- list(c("Severe", "Critical"))
  
  p <- ggplot(df, aes(x = sam, y = Expression)) +
    geom_point(color = '#bbbdbf', position = 'jitter') +   # 灰色抖动散点
    geom_violin(notch = F, outlier.colour = NA,            # 小提琴图，不显示离群点
                mapping = aes(fill = as.factor(sam))) +    # 按分组填充
    geom_smooth(data = df,
                mapping = aes(x = as.numeric(sam), y = Expression),
                color = 'red', se = F, method = 'lm') +    # 红色线性拟合线
    # ⚠️ x = as.numeric(sam)：把因子转成 1、2、3、4，在离散 x 轴上拟合会画出一条直线
    # 但这根线是基于序号拟合的，不是基于真实数值，解释时需谨慎
    scale_fill_manual(values = col) +                      # 自定义填充色
    theme(axis.line = element_line(colour = "black"),      # 轴线
          axis.title.x = element_blank(),                  # 不显示 x 轴标题
          axis.title.y = element_blank(),                  # 不显示 y 轴标题
          axis.text.x = element_text(size = 5, angle = 30, # x 轴文字旋转 30 度
                                     vjust = 1, hjust = 1),
          axis.text.y = element_text(size = 5),            # y 轴文字
          plot.title = element_text(hjust = 0.5, size = 5, face = "bold"),  # 标题
          legend.position = "NA") +                        # ❌ 应为 "none"
    ggtitle(gene[i]) +                                     # 标题为基因名
    stat_compare_means(method = "t.test", hide.ns = F,     # t 检验
                       comparisons = c(my_comparisons1, my_comparisons2,
                                       my_comparisons3, my_comparisons4,
                                       my_comparisons5, my_comparisons6),
                       label = "p.signif")                 # 显示显著性符号
  
  plist2[[i]] <- p                                         # 存入列表
}

plot_grid(plist2[[1]], plist2[[2]], plist2[[3]],
          plist2[[4]], plist2[[5]], plist2[[6]],
          plist2[[7]], plist2[[8]], plist2[[9]],
          plist2[[10]], plist2[[11]], plist2[[12]], ncol = 4)
# 12 张图拼成 4 列 3 行
# ⚠️ 结果没有赋值给变量，直接打印了

# ==================== 第二组图：小提琴图 + 箱线图 + 线性拟合 ====================
plist3 <- list()

for (i in 1:length(gene)) {
  df <- Exp_plot[, c(gene[i], "sam")]
  colnames(df) <- c("Expression", "sam")
  # 6 组比较同上（代码重复，可提取为函数）
  
  p <- ggplot(df, aes(x = sam, y = Expression)) +
    geom_point(color = '#bbbdbf', position = 'jitter') +
    geom_violin(notch = F, outlier.colour = NA, mapping = aes(fill = as.factor(sam))) +
    geom_boxplot(mapping = aes(fill = as.factor(sam)), width = 0.2) +  # 叠加箱线图
    geom_smooth(data = df, mapping = aes(x = as.numeric(sam), y = Expression),
                color = 'red', se = F, method = 'lm') +
    scale_fill_manual(values = col) +
    theme(axis.line = element_line(colour = "black"),
          axis.title.x = element_blank(), axis.title.y = element_blank(),
          axis.text.x = element_text(size = 5, angle = 30, vjust = 1, hjust = 1),
          axis.text.y = element_text(size = 5),
          plot.title = element_text(hjust = 0.5, size = 5, face = "bold"),
          legend.position = "NA") +
    ggtitle(gene[i]) +
    stat_compare_means(method = "t.test", hide.ns = F,
                       comparisons = c(my_comparisons1, my_comparisons2,
                                       my_comparisons3, my_comparisons4,
                                       my_comparisons5, my_comparisons6),
                       label = "p.signif")
  plist3[[i]] <- p
}

plot_grid(plist3[[1]], plist3[[2]], plist3[[3]],
          plist3[[4]], plist3[[5]], plist3[[6]],
          plist3[[7]], plist3[[8]], plist3[[9]],
          plist3[[10]], plist3[[11]], plist3[[12]], ncol = 4)

# ==================== 第三组图：小提琴图 + 箱线图 + LOESS 曲线 ====================
plist4 <- list()

for (i in 1:length(gene)) {
  df <- Exp_plot[, c(gene[i], "sam")]
  colnames(df) <- c("Expression", "sam")
  # 6 组比较同上
  
  p <- ggplot(df, aes(x = sam, y = Expression)) +
    geom_point(color = '#bbbdbf', position = 'jitter') +
    geom_violin(notch = F, outlier.colour = NA, mapping = aes(fill = as.factor(sam))) +
    geom_boxplot(mapping = aes(fill = as.factor(sam)), width = 0.2) +
    geom_smooth(data = df, mapping = aes(x = as.numeric(sam), y = Expression),
                color = 'red', se = F, method = 'loess') +   # 改用 loess 平滑曲线
    scale_fill_manual(values = col) +
    theme(axis.line = element_line(colour = "black"),
          axis.title.x = element_blank(), axis.title.y = element_blank(),
          axis.text.x = element_text(size = 5, angle = 45, vjust = 1, hjust = 1),
          axis.text.y = element_text(size = 5),
          plot.title = element_text(hjust = 0.5, size = 5, face = "bold"),
          legend.position = "NA") +
    ggtitle(gene[i]) +
    stat_compare_means(method = "t.test", hide.ns = F,
                       comparisons = c(my_comparisons1, my_comparisons2,
                                       my_comparisons3, my_comparisons4,
                                       my_comparisons5, my_comparisons6),
                       label = "p.signif")
  plist4[[i]] <- p
}

plot_grid(plist4[[1]], plist4[[2]], plist4[[3]],
          plist4[[4]], plist4[[5]], plist4[[6]],
          plist4[[7]], plist4[[8]], plist4[[9]],
          plist4[[10]], plist4[[11]], plist4[[12]], ncol = 4)

# ==================== 第四组图：柱状图 + 散点 + 线性拟合 ====================
plist5 <- list()

for (i in 1:length(gene)) {
  df <- Exp_plot[, c(gene[i], "sam")]
  colnames(df) <- c("Expression", "sam")
  # 6 组比较同上
  
  p <- ggplot(df, aes(x = sam, y = Expression)) +
    geom_bar(width = 0.5, aes(fill = sam), stat = 'summary') +   # 柱状图（默认 mean）
    geom_point(color = '#bbbdbf', position = 'jitter') +          # 叠加散点
    geom_smooth(data = df, mapping = aes(x = as.numeric(sam), y = Expression),
                color = 'red', se = F, method = 'lm') +
    scale_fill_manual(values = col) +
    theme(axis.line = element_line(colour = "black"),
          axis.title.x = element_blank(), axis.title.y = element_blank(),
          axis.text.x = element_text(size = 5, angle = 30, vjust = 1, hjust = 1),
          axis.text.y = element_text(size = 5),
          plot.title = element_text(hjust = 0.5, size = 5, face = "bold"),
          legend.position = "NA") +
    ggtitle(gene[i]) +
    stat_compare_means(method = "t.test", hide.ns = F,
                       comparisons = c(my_comparisons1, my_comparisons2,
                                       my_comparisons3, my_comparisons4,
                                       my_comparisons5, my_comparisons6),
                       label = "p.signif")
  plist5[[i]] <- p
}

plot_grid(plist5[[1]], plist5[[2]], plist5[[3]],
          plist5[[4]], plist5[[5]], plist5[[6]],
          plist5[[7]], plist5[[8]], plist5[[9]],
          plist5[[10]], plist5[[11]], plist5[[12]], ncol = 4)

# ==================== 保存 ====================
ggsave("图.pdf", width = 9, height = 9, dpi = 300)
# 只保存最后一次 plot_grid 的结果（即 plist5 的拼图）
# 前面 plist2、plist3、plist4 的拼图都没有被保存