library(ggplot2)

df <- read.csv('df.csv', header = T)   # 读取 CSV 数据，第一行为列名
# df <- df[-c(10:12), ]                # 删除第 10-12 行（已注释）
write.csv(df, file = "df.csv")         # 把当前 df 写回 df.csv（覆盖原文件，慎用）

df <- cbind(df[, 1:7], df[, 8:14] * -1)
# 把第 8-14 列乘以 -1（通常用于把“下调”的值变成负数，方便作图区分方向）

library(tidyr)
data <- gather(df, gene, value, 1:14)  # 宽表转长表：第 1-14 列变为 gene/value 两列
# gather() 在新版 tidyr 中已被 pivot_longer() 取代

data$group <- ''                       # 新增 group 列，值全为空字符串
# data$group <- ifelse(data$value > 0, "Up_regulation", "Down_regulation")
# 上一行被注释掉了，所以 group 全为空，后面 scale_fill_manual 不会起作用

# ==================== 第一张图：柱状图 ====================
ggplot(data, aes(fill = group,               # 填充色按 group（实际上全空）
                 y = value,                  # y 轴为 value
                 x = reorder(gene, -value))) +  # x 轴按 value 降序排列
  geom_bar(position = position_dodge(),      # 柱子并排
           stat = "summary",                 # 用统计汇总值（默认 mean）作为高度
           width = 0.9,                      # 柱宽 0.9
           size = 1) +                       # ⚠️ 新版 ggplot2 应改为 linewidth
  stat_summary(fun.data = 'mean_se',         # 添加均值 ± 标准误误差棒
               geom = "errorbar",
               colour = "black",
               width = 0.2,
               position = position_dodge(0.7)) +
  scale_fill_manual(values = c('#F69CA4', '#EE2024')) +   # 两个颜色，但 group 为空，不生效
  theme(axis.text.x = element_blank()) +     # x 轴文字不显示
  theme(axis.text.y = element_text(size = 12, color = "black"),  # y 轴文字
        axis.line.y = element_line(color = 'black'),             # y 轴线
        axis.title.y = element_text(size = 14, color = "black")) +# y 轴标题
  theme(axis.title.x = element_blank(),      # x 轴标题不显示
        axis.ticks.x = element_blank()) +    # x 轴刻度不显示
  theme(panel.grid = element_blank(),        # 不显示网格
        panel.background = element_blank()) +# 面板背景透明
  theme(legend.position = 'none') +          # 不显示图例
  geom_hline(aes(yintercept = 0),            # y=0 参考线
             linetype = 1, cex = 1, color = 'black') +  # cex 已弃用，应改用 linewidth
  labs(title = "", y = "Relative expression", x = "") +  # 轴标签
  annotate(geom = 'text', label = "Up_regulation",       # 添加文字
           x = 3.5, y = -15, size = 4) +
  annotate("segment", x = 0.5, xend = 7.5,   # 添加线段
           y = -10, yend = -10, color = 'black') +
  annotate(geom = 'text', label = "Down_regulation",
           x = 11.5, y = 15, size = 4) +
  annotate("segment", x = 8.5, xend = 13.5,
           y = 10, yend = 10, color = 'black')

# ==================== 第二张图：柱状图 + 散点 ====================
ggplot(data, aes(fill = group, y = value, x = reorder(gene, -value))) +
  geom_bar(position = position_dodge(),
           stat = "summary",
           width = 0.9,
           size = 1) +
  stat_summary(fun.data = 'mean_se',
               geom = "errorbar",
               colour = "black",
               width = 0.2,
               position = position_dodge(0.7)) +
  scale_fill_manual(values = c('#F69CA4', '#EE2024')) +
  theme(axis.text.x = element_blank()) +
  theme(axis.text.y = element_text(size = 12, color = "black"),
        axis.line.y = element_line(color = 'black'),
        axis.title.y = element_text(size = 14, color = "black")) +
  theme(axis.title.x = element_blank(),
        axis.ticks.x = element_blank()) +
  theme(panel.grid = element_blank(),
        panel.background = element_blank()) +
  theme(legend.position = 'none') +
  geom_hline(aes(yintercept = 0),
             linetype = 1, cex = 1, color = 'black') +
  labs(title = "", y = "Relative expression", x = "") +
  geom_jitter(data = data, aes(y = value, x = reorder(gene, -value)),
              size = 3, shape = 16,            # 点大小 3，实心圆
              color = "grey60",                # 灰色
              stroke = 0.15,                   # 点边框粗细
              show.legend = FALSE,
              position = position_jitterdodge(
                jitter.height = 0.5,           # 垂直抖动
                jitter.width = 0.1,            # 水平抖动
                dodge.width = 0.8))            # 与柱子对齐

# ==================== 保存 ====================
ggsave("图.pdf", width = 6, height = 5, dpi = 300)
# 保存最后一次绘制的图（第二张），第一张不会被保存