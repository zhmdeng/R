# 加载包
library(multcompView)   # 用于多重比较字母标记
library(patchwork)      # 拼图
library(tidyverse)      # 数据处理和绘图
library(ggrepel)        # 标签防重叠
library(FactoMineR)     # 多元统计（本段未直接用，但可能后续用）
library(magrittr)       # 管道
library(factoextra)     # 多元统计可视化（本段未直接用）
library(RColorBrewer)   # 配色
library(vegan)          # 生态统计（vegdist、adonis2）
library(ape)            # pcoa 函数

# 读取数据，重命名 sample 列，并把 Subtype 中的 "-" 换成 "_"
df <- read_tsv("data.xls") %>% 
  dplyr::rename(sample = "Sample_id") %>% 
  mutate(across("Subtype", str_replace, "-", "_"))

# 计算 Bray-Curtis 距离，然后做 PCoA
pcoa <- df %>% 
  column_to_rownames(var = "sample") %>% 
  dplyr::select(-Subtype) %>% 
  vegdist(method = "bray") %>% 
  pcoa(correction = "none", rn = NULL)

# 提取前两轴坐标，组成数据框，并加入分组信息
pcoadata <- data.frame(pcoa$vectors[, 1], pcoa$vectors[, 2]) %>% 
  set_colnames(c("PC1", "PC2")) %>% 
  rownames_to_column(var = "sample") %>% 
  left_join(df %>% dplyr::select(sample, Subtype), by = "sample")

# 绘制 PCoA 散点图
plot <- ggplot(pcoadata, aes(PC1, PC2)) +
  geom_point(aes(colour = Subtype, fill = Subtype), size = 4) +
  scale_color_manual(values = colorRampPalette(brewer.pal(12, "Paired"))(4)) +
  labs(x = paste0("(PC1: ", round(pcoa$values$Relative_eig[1] * 100, 2), "%)"),
       y = paste0("(PC2: ", round(pcoa$values$Relative_eig[2] * 100, 2), "%)")) +
  geom_vline(aes(xintercept = 0), linetype = "dotted") +
  geom_hline(aes(yintercept = 0), linetype = "dotted") +
  theme_bw() +
  theme(
    panel.background = element_rect(fill = 'white', colour = 'black'),
    axis.title.x = element_text(colour = "black", size = 12, margin = margin(t = 5), face = "bold"),
    axis.title.y = element_text(colour = "black", size = 12, margin = margin(r = 5), face = "bold"),
    axis.text = element_text(color = "black", face = "bold"),
    plot.title = element_blank(),
    legend.title = element_blank(),
    legend.key = element_blank(),
    legend.text = element_text(color = "black", size = 9, face = "bold"),
    legend.spacing.x = unit(0.1, 'cm'),
    legend.key.width = unit(0.5, 'cm'),
    legend.key.height = unit(0.5, 'cm'),
    legend.background = element_blank(),
    legend.box.background = element_rect(colour = "black"),
    legend.position = c(0.001, 0.999),
    legend.justification = c(0.0001, 1)
  )

# ---- 对 PC1 做单因素方差分析 + Tukey 检验，提取字母标记 ----
cld1 <- multcompLetters4(
  aov(PC1 ~ Subtype, data = pcoadata),
  TukeyHSD(aov(PC1 ~ Subtype, data = pcoadata))
)

dt1 <- pcoadata %>% 
  group_by(Subtype) %>% 
  dplyr::summarise(value_max = max(PC1), sd = sd(PC1)) %>% 
  ungroup() %>% 
  arrange(desc(value_max))

text1 <- as.data.frame.list(cld1$Subtype) %>% 
  rownames_to_column(var = "Subtype") %>% 
  left_join(dt1, by = "Subtype")

# ---- 对 PC2 做同样的事 ----
cld2 <- multcompLetters4(
  aov(PC2 ~ Subtype, data = pcoadata),
  TukeyHSD(aov(PC2 ~ Subtype, data = pcoadata))
)

dt2 <- pcoadata %>% 
  group_by(Subtype) %>% 
  dplyr::summarise(value_max = max(PC2)) %>% 
  ungroup() %>% 
  arrange(desc(value_max))

text2 <- as.data.frame.list(cld2$Subtype) %>% 
  rownames_to_column(var = "Subtype") %>% 
  left_join(dt2, by = "Subtype")

# ---- 绘制 PC1 箱线图（横向） ----
p2 <- ggplot(pcoadata, aes(Subtype, PC1, fill = Subtype)) +
  geom_boxplot(outlier.shape = NA, width = 0.5, color = "black", linetype = "dotted") +
  stat_boxplot(aes(ymin = after_stat(lower), ymax = after_stat(upper)),
               outlier.shape = NA, width = 0.5) +
  stat_boxplot(geom = "errorbar", aes(ymin = after_stat(ymax)), width = 0.2, size = 0.35) +
  stat_boxplot(geom = "errorbar", aes(ymax = after_stat(ymin)), width = 0.2, size = 0.35) +
  labs(x = NULL, y = NULL) +
  geom_text(data = text1, aes(label = Letters, y = value_max + 0.0082),
            angle = -90, color = "black", size = 4) +
  scale_fill_manual(values = colorRampPalette(brewer.pal(12, "Paired"))(4)) +
  theme_bw() +
  theme(
    panel.background = element_rect(fill = 'white', colour = 'black'),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    plot.title = element_blank(),
    legend.position = "none"
  ) +
  coord_flip()

# ---- 绘制 PC2 箱线图 ----
p3 <- ggplot(pcoadata, aes(Subtype, PC2, fill = Subtype)) +
  geom_boxplot(outlier.shape = NA, width = 0.5, color = "black", linetype = "dotted") +
  stat_boxplot(aes(ymin = after_stat(lower), ymax = after_stat(upper)),
               outlier.shape = NA, width = 0.5) +
  stat_boxplot(geom = "errorbar", aes(ymin = after_stat(ymax)), width = 0.2, size = 0.35) +
  stat_boxplot(geom = "errorbar", aes(ymax = after_stat(ymin)), width = 0.2, size = 0.35) +
  labs(x = NULL, y = NULL) +
  geom_text(data = text2, aes(label = Letters, y = value_max + 0.0055),
            angle = 0, color = "black", size = 4) +
  scale_fill_manual(values = colorRampPalette(brewer.pal(12, "Paired"))(4)) +
  theme_bw() +
  theme(
    panel.background = element_rect(fill = 'white', colour = 'black'),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    plot.title = element_blank(),
    legend.position = "none"
  )

# ---- PERMANOVA 分析 ----
# 构建距离矩阵（样本顺序与 pcoadata 一致，但 pcoadata 来自原 df，顺序可能一致，这里直接用原始 df）
dist_mat <- df %>% 
  column_to_rownames(var = "sample") %>% 
  dplyr::select(-Subtype) %>% 
  vegdist(method = "bray")

# 确保分组顺序与距离矩阵一致
group_vec <- df$Subtype[match(rownames(as.matrix(dist_mat)), df$sample)]

otu.adonis <- adonis2(dist_mat ~ group_vec, distance = "bray")

# 绘制文字说明（PERMANOVA 结果）
p4 <- ggplot() +
  geom_text(aes(x = 0, y = 0.1,
                label = paste("PERMANOVA:\ndf = ", otu.adonis$Df[1],
                              "\nR2 =", round(otu.adonis$R2[1], 5),
                              "\np-value = ", otu.adonis$`Pr(>F)`[1], sep = "")),
            size = 3.5, color = "black", fontface = "bold") +
  theme_bw() +
  theme(
    panel.background = element_rect(fill = 'white', colour = 'black'),
    axis.title = element_blank(),
    axis.ticks = element_blank(),
    axis.text = element_blank(),
    plot.title = element_blank(),
    legend.position = "none"
  )

# ---- 拼图 ----
# 布局：第一行 p2（PC1箱线图）和 p4（文字），第二行 plot（散点图）和 p3（PC2箱线图）
# 注意 p2 是横向箱线图，p3 是纵向箱线图，需要调整尺寸
p2 + p4 + plot + p3 + 
  plot_layout(heights = c(1, 4), widths = c(4, 1), ncol = 2, nrow = 2)

# 保存图片
ggsave("图.pdf", width = 8, height = 6, dpi = 300)