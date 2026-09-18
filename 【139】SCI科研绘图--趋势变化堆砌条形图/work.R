library(tidyverse)

# ==================== 构造示例数据 ====================
df = data.frame()                              # 创建一个空数据框
df = data.frame(matrix(df, nrow = 200, ncol = 2))
# 用空数据框创建 200×2 的矩阵再转数据框，结果是 200 行 2 列，值全为 NA

colnames(df) <- c("cluster", "name")           # 给两列命名
df$cluster <- sample(20, size = nrow(df), replace = TRUE)
# cluster 列重新赋值：从 1:20 中有放回抽样 200 次
df$fruit <- sample(c("banana", "apple", "orange", "kiwi", "plum"),
                   size = nrow(df), replace = TRUE)
# fruit 列：从 5 种水果中有放回抽样 200 次

# ==================== 绘图 ====================
df %>% as_tibble() %>%
  mutate(
    cluster = factor(cluster,
                     names(sort(table(fruit == 'apple', cluster)[2, ]))),
    # 把 cluster 转为因子，水平顺序按“每个 cluster 中 apple 的数量”排序
    # table(fruit == 'apple', cluster) 生成 2×20 的列联表
    # 第 2 行是 TRUE（即 apple）的计数，排序后取列名
    fruit = factor(fruit, c('apple', 'kiwi', 'banana', 'orange', 'plum'))
    # 固定 fruit 的因子水平
  ) %>%
  ggplot(aes(x = cluster, fill = fruit)) +     # x=cluster，填充=fruit
  geom_bar(position = position_stack(reverse = TRUE)) +
  # 堆叠柱状图，reverse=TRUE 让堆叠顺序反转
  scale_y_discrete(expand = c(0, 0)) +
  # ❌ 这里应该是 scale_x_discrete，因为 x 轴是 cluster（离散变量）
  # 而且后面要 coord_flip，y 轴才是计数
  labs(y = NULL) +
  coord_flip() +                               # 翻转坐标轴，横向柱状图
  ggthemes::theme_wsj() +                      # WSJ 主题
  ggthemes::scale_fill_ptol() +                # Ptol 配色
  theme(
    axis.text.y = element_text(color = "black", size = 8, margin = margin(r = 1)),
    axis.text.x = element_text(color = "black", size = 9, margin = margin(t = 8)),
    axis.title.x = element_text(size = 11, margin = margin(t = 8),
                                color = "black", face = "bold"),
    plot.margin = unit(c(0.3, 0.3, 0.3, 0.3), units = "cm"),
    # ❌ 原代码写的是 units = ,"cm"，中间多了个逗号，会报错
    panel.background = element_blank(),        # 面板背景透明
    axis.line = element_line(color = "black"), # 轴线黑色
    axis.ticks.length.x = unit(-.2, "cm"),     # x 轴刻度朝内
    legend.key = element_blank(),              # 图例键背景透明
    legend.background = element_blank(),       # 图例背景透明
    legend.title = element_blank(),            # 图例标题不显示
    legend.text = element_text(size = 8, color = "black"),  # 图例文字
    legend.spacing.x = unit(0.1, 'cm'),        # 图例水平间距
    legend.key.width = unit(0.4, "cm"),        # 图例键宽度
    legend.key.height = unit(0.4, "cm")        # 图例键高度
  )

# ==================== 保存 ====================
ggsave("图.pdf", width = 6, height = 6, dpi = 300)