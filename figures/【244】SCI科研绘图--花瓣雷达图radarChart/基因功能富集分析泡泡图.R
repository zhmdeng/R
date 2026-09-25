

Sys.setenv(LANGUAGE = "en")      # 设置 R 报错信息为英文
options(stringsAsFactors = FALSE) # 禁止字符型自动转为因子（R 4.0+ 已默认）

library(ggplot2)                 # 再次加载 ggplot2（重复）
Sys.setenv(LANGUAGE = "en")      # 再次设置英文报错（重复）
options(stringsAsFactors = FALSE) # 再次设置（重复）

# 查看 GeneRatio 的分布，用来确定 x 轴范围
summary(df$GeneRatio)

# ==================== 第一张图：按 GeneRatio 升序排列 ====================
sortdf <- df[order(df$GeneRatio), ]   # 按 GeneRatio 升序排序
# 如果想按 PValue 排序，把 GeneRatio 改成 PValue 即可

sortdf$Term <- factor(sortdf$Term, levels = sortdf$Term)
# 把 Term 转为因子，水平按当前排序后的顺序，保证 y 轴顺序正确

pl <- ggplot(sortdf, aes(GeneRatio, Term,      # x=GeneRatio，y=Term
                         colour = PValue)) +   # 颜色映射到 PValue
  geom_point(aes(size = Count)) +              # 点的大小映射到 Count
  scale_size_continuous(range = c(2, 10)) +    # 点大小范围 2~10
  scale_color_gradientn(colours = c("red", "yellow")) +  # 颜色从红到黄
  # scale_x_continuous(limits = c(0.2, 1)) +  # 可选：设置 x 轴范围
  theme_bw() +                                 # 黑白主题
  ylab("") +                                   # y 轴标题为空
  theme(legend.position = c(1, 0),             # 图例放在右下角
        legend.justification = c(1, 0)) +      # 图例对齐到右下角
  theme(legend.background = element_blank()) + # 图例背景透明
  theme(legend.key = element_blank())          # 图例键背景透明

pl   # 显示第一张图

ggsave("dot图.pdf", width = 7, height = 6)
# 保存 pl 为 PDF，7×6 英寸（默认单位英寸）
# 也可以保存为 png、tiff：
# ggsave("dot.png", width = 7, height = 6)
# ggsave("dot.tiff", width = 7, height = 6)

# ==================== 第二张图：按 GeneRatio 降序排列 ====================
sortdf <- df[order(df$GeneRatio, decreasing = TRUE), ]  # 降序排序
sortdf$Term <- factor(sortdf$Term, levels = sortdf$Term) # 固定 y 轴顺序

pr <- ggplot(sortdf, aes(GeneRatio, Term,      # x=GeneRatio，y=Term
                         colour = PValue)) +   # 颜色映射 PValue
  geom_point(aes(size = Count)) +              # 点大小映射 Count
  scale_color_gradientn(colours = c("red", "yellow")) +  # 红到黄
  scale_size_continuous(range = c(2, 10)) +    # 点大小 2~10
  # scale_x_continuous(limits = c(0.2, 1)) +  # 可选 x 轴范围
  ylab("") +                                   # y 轴标题为空
  scale_y_discrete(position = "right") +       # y 轴标签放在右侧
  theme_bw() +                                 # 黑白主题
  theme(legend.position = c(0, 0),             # 图例放在左下角
        legend.justification = c(0, 0)) +      # 对齐左下角
  theme(legend.background = element_blank()) + # 图例背景透明
  theme(legend.key = element_blank())          # 图例键背景透明

pr   # 显示第二张图

# ==================== 拼图 ====================
library(cowplot)                # 加载 cowplot，用于拼图
plot_grid(pl, pr, labels = "")  # 左右并排，不加 A/B 标签

ggsave(file = "图.pdf")          # 保存拼图（默认保存最后一次显示的图）