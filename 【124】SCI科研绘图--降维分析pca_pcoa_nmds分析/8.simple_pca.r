setwd("")
# 设置工作目录

# ==================================================
# 模块一：简单 PCA 分析
# ==================================================
rm(list = ls())                       # 清空环境
options(stringsAsFactors = F)         # 字符型不自动转因子

library(vegan)                        # 生态学统计
library(ggplot2)                      # 绘图
library(ggrepel)                      # 标签避免重叠
library(dplyr)                        # 数据处理
library(RColorBrewer)                 # 配色方案

time <- Sys.time()                    # 记录当前时间（未使用）

mydata <- read.delim('step2.even.5000.feature-table.txt',   # 读取特征表
                     header = T,                            # 第一行为列名
                     row.names = 1,                         # 第一列为行名
                     check.names = F)                       # 不修改列名
mydata <- as.data.frame(t(mydata))                          # 转置：行=样本，列=物种
mydata <- decostand(mydata, method = "hellinger")           # Hellinger 标准化

otu_pca <- prcomp(mydata, scal = F)                         # PCA（参数名应为 scale.）
pc12 <- as.data.frame(otu_pca$x[, 1:2]) * 100               # 前两主成分坐标 ×100
pc12$samples <- rownames(pc12)                              # 添加样本列

groups <- read.delim('group.txt', header = T)               # 读取分组
colnames(groups)[1] <- 'samples'                            # 第一列改名
groups$group <- factor(groups$group,                        # group 转因子
                       levels = groups$group[!duplicated(groups$group)])  # 保序

pc <- summary(otu_pca)$importance[2, ] * 100                # 各 PC 解释方差
pc12 <- merge(pc12, groups, by = 'samples')                 # 合并
colnames(pc12)[2:3] <- c('PC1', 'PC2')                      # 重命名 PC 列

paste(brewer.pal(n = 4, 'Set1'), collapse = "','")          # 查看配色（仅查看）
mycol <- c('#E41A1C', '#377EB8', '#4DAF4A', '#984EA3')      # 手动指定 4 色

# ---- 第一张 PCA 图 ----
ggplot(data = pc12, aes(x = PC1, y = PC2, color = group, shape = group)) +
  geom_point(size = 3) +                                    # 散点
  geom_hline(yintercept = 0, linetype = 2, color = 'gray') + # y=0 虚线
  geom_vline(xintercept = 0, linetype = 2, color = 'gray') + # x=0 虚线
  labs(x = paste0("PC1(", round(pc[1], 2), "%", ")"),       # x 轴标签
       y = paste0("PC2(", round(pc[2], 2), "%)")) +         # y 轴标签
  theme_bw() +                                              # 黑白主题
  theme(panel.grid = element_blank()) +                     # 无网格线
  stat_ellipse(aes(x = PC1, y = PC2, color = group, fill = group),
               alpha = 0.1,                                 # 椭圆透明度
               linetype = 1,                                # 实线
               level = 0.95,                                # 95% 置信
               geom = 'polygon') +                          # 多边形
  scale_color_manual(values = mycol)                        # 手动颜色

# ---- 第二张 PCA 图（带样本标签） ----
ggplot(data = pc12, aes(PC1, PC2, color = group, shape = group, label = samples)) +
  geom_point(size = 3) +                                    # 散点
  geom_hline(yintercept = 0, linetype = 2) +                # y=0 虚线
  geom_vline(xintercept = 0, linetype = 2) +                # x=0 虚线
  geom_text_repel() +                                       # 样本标签
  labs(x = paste0("PC1(", round(pc[1], 2), "%", ")"),
       y = paste0("PC2(", round(pc[2], 2), "%)")) +
  theme_bw() +
  theme(panel.grid = element_blank()) +
  scale_color_manual(values = mycol)

filename <- paste0("simple_pca", format(Sys.time(), "%Y%m%d_%H%M%S"), ".pdf")
# 带时间戳的文件名
ggsave(filename, width = 6, height = 4)                     # 保存

# ==================================================
# 模块二：简单 PCoA 分析
# ==================================================
rm(list = ls())                       # 清空环境
options(stringsAsFactors = F)         # 字符型不自动转因子
library(vegan)                        # 生态学统计
library(ggplot2)                      # 绘图
library(ggrepel)                      # 标签避免重叠
library(dplyr)                        # 数据处理

genus <- read.delim('step2.even.5000.feature-table.txt', header = T,
                    row.names = 1, check.names = F)         # 读取数据
genus <- as.data.frame(t(genus))                            # 转置
otu.dist <- vegdist(genus, method = "bray")                 # Bray-Curtis 距离

otu_pcoa <- cmdscale(otu.dist, eig = TRUE)                  # PCoA
pc12 <- as.data.frame(otu_pcoa$points[, 1:2])               # 前两轴坐标
pc12$samples <- rownames(pc12)                              # 添加样本列

groups <- read.delim('group.txt', header = T)               # 读取分组
colnames(groups)[1] <- 'samples'                            # 第一列改名

pc <- round(otu_pcoa$eig / sum(otu_pcoa$eig) * 100, digits = 2)  # 各轴方差
pc12 <- merge(pc12, groups, by = 'samples')                 # 合并
pc12$group <- factor(pc12$group, levels = unique(groups$group))  # group 转因子
colnames(pc12)[2:3] <- c('PC1', 'PC2')                      # 重命名

mycol <- c('#E41A1C', '#377EB8', '#4DAF4A', '#984EA3')      # 4 色
myshape <- c(21, 21, 21, 21)                                # 4 个形状

# ---- PERMANOVA 检验 ----
otu.dist <- as.matrix(otu.dist)                             # 转矩阵
otu.dist <- otu.dist[groups$samples, groups$samples]        # 按分组顺序重排
adist <- as.dist(otu.dist)                                  # 转 dist 对象
adist                                                       # 打印（查看）

ADONIS <- adonis2(adist ~ groups$group)                     # PERMANOVA
ADONIS                                                      # 打印结果

TEST <- ADONIS[1, "Pr(>F)"]                                 # ✅ 提取 p 值

R2adonis <- round(ADONIS[1, "R2"], digits = 3)              # ✅ 提取 R²
sink('step9.adonis.txt')                                    # 输出重定向到文件
print(ADONIS)
sink()                                                      # 关闭重定向

ADONIS_df <- as.data.frame(ADONIS)                          # 转数据框
Fvalue <- round(ADONIS_df$F[1], digits = 3)                 # ✅ 提取 F 值

# ---- PCoA 图 ----
ggplot(data = pc12, aes(PC1, PC2)) +
  geom_point(aes(fill = group, shape = group), size = 3, color = 'black') +  # 散点
  stat_ellipse(aes(x = PC1, y = PC2, color = group),        # 95% 椭圆
               linetype = 1, level = 0.95, show.legend = F) +
  scale_shape_manual(values = myshape) +                    # 手动形状
  scale_fill_manual(values = mycol) +                       # 手动填充色
  scale_color_manual(values = mycol) +                      # 手动边框色
  geom_hline(yintercept = 0, linetype = 2) +                # y=0 虚线
  geom_vline(xintercept = 0, linetype = 2) +                # x=0 虚线
  labs(x = paste0("PCoA1(", round(pc[1], 2), "%", ")"),
       y = paste0("PCoA2(", round(pc[2], 2), "%)")) +
  ggtitle(label = paste('PERMANOVA:F=', Fvalue, ', p=', TEST, sep = '')) +  # 标题
  theme_bw() + theme(panel.grid = element_blank(),
                     legend.title = element_blank(),
                     axis.text = element_text(size = 10),
                     axis.title = element_text(size = 12))

ggsave('step9.pcoa.pdf', width = 6, height = 4)             # 保存

write.table(otu_pcoa$points, paste0('step9.', "pcoa_by_group_sites.xls"),
            sep = "\t", col.names = NA, quote = F)          # 保存坐标
write.table(otu_pcoa$eig / sum(otu_pcoa$eig), paste0('step9.', "pcoa_by_group_importance.xls"),
            sep = "\t", quote = F)                          # 保存特征值

# ---- 第二张 PCoA 图（带样本标签） ----
ggplot(data = pc12, aes(PC1, PC2)) +
  geom_point(aes(fill = group, shape = group), size = 3, color = 'black') +
  scale_shape_manual(values = myshape) +
  geom_text_repel(data = pc12, aes(PC1, PC2, label = samples), size = 4) +  # 样本标签
  scale_fill_manual(values = mycol) +
  scale_color_manual(values = mycol) +
  geom_hline(yintercept = 0, linetype = 2) +
  geom_vline(xintercept = 0, linetype = 2) +
  labs(x = paste0("PCoA1(", round(pc[1], 2), "%", ")"),
       y = paste0("PCoA2(", round(pc[2], 2), "%)")) +
  ggtitle(label = paste('PERMANOVA:F=', Fvalue, ', p=', TEST, sep = '')) +
  theme_bw() + theme(panel.grid = element_blank(),
                     legend.title = element_blank(),
                     axis.text = element_text(size = 10),
                     axis.title = element_text(size = 12))

filename <- paste0("simple_pcoa", format(Sys.time(), "%Y%m%d_%H%M%S"), ".pdf")
ggsave(filename, width = 6, height = 4)                     # 保存

# ==================================================
# 模块三：简单 NMDS 分析
# ==================================================
library(vegan)                        # 生态学统计
library(ggplot2)                      # 绘图
library(RColorBrewer)                 # 配色方案
library(scales)                       # 提供 scientific()

genus <- read.delim('step2.even.5000.feature-table.txt', header = T,
                    row.names = 1, check.names = F)         # 读取数据
genus <- as.data.frame(t(genus))                            # 转置
dist <- vegdist(genus, method = "bray")                     # Bray-Curtis 距离
dist <- as.matrix(dist)                                     # 转矩阵
nmds_result <- metaMDS(dist, k = 2)                         # NMDS，降到 2 维
stress <- paste0("Stress=", scientific(nmds_result$stress, digits = 3))  # Stress 值

nmds12 <- as.data.frame(nmds_result$points)                 # NMDS 坐标
nmds12$samples <- rownames(nmds12)                          # 添加样本列

groups <- read.delim('group.txt', header = T)               # 读取分组
colnames(groups)[1] <- 'samples'                            # 第一列改名
nmds12 <- merge(nmds12, groups, by = 'samples')             # 合并

mycol <- c('#E41A1C', '#377EB8', '#4DAF4A', '#984EA3')      # 4 色
myshape <- 21:25                                            # 形状

# ---- 第一张 NMDS 图 ----
ggplot() +
  geom_point(data = nmds12, aes(MDS1, MDS2, shape = group, fill = group), size = 4) +
  scale_fill_manual(values = mycol) +                       # 手动填充色
  scale_shape_manual(values = myshape) +                    # 手动形状
  geom_hline(yintercept = 0, linetype = 2) +                # y=0 虚线
  geom_vline(xintercept = 0, linetype = 2) +                # x=0 虚线
  labs(x = 'NMDS1', y = "NMDS2", title = stress) +          # 标题含 Stress
  theme_bw() + theme(panel.grid = element_blank(),
                     legend.title = element_blank())         # 无图例标题

ggsave('step10.nmds.pdf', width = 8, height = 6)            # 保存

# ---- 第二张 NMDS 图（带样本标签） ----
library(ggrepel)
ggplot(data = nmds12, aes(MDS1, MDS2, shape = group, fill = group, label = samples)) +
  geom_point(size = 4) +
  scale_fill_manual(values = mycol) +
  scale_shape_manual(values = 21:25) +
  geom_text_repel(max.overlaps = 30) +                      # 标签避免重叠
  geom_hline(yintercept = 0, linetype = 2) +
  geom_vline(xintercept = 0, linetype = 2) +
  labs(x = 'NMDS1', y = "NMDS2", title = stress) +
  theme_bw() + theme(panel.grid = element_blank(),
                     legend.title = element_blank())

ggsave('simple_nmds.pdf', width = 8, height = 6)            # 保存

# ==================================================
# 模块四：两组 PCA
# ==================================================
rm(list = ls())                       # 清空环境
options(stringsAsFactors = F)         # 字符型不自动转因子
library(vegan)                        # 生态学统计
library(ggplot2)                      # 绘图
library(ggrepel)                      # 标签避免重叠
library(dplyr)                        # 数据处理

genus <- read.delim('step2.even.5000.feature-table.txt', header = T,
                    row.names = 1, check.names = F)         # 读取数据
genus <- as.data.frame(t(genus))                            # 转置
genus <- decostand(genus, method = "hellinger")             # Hellinger 标准化
otu_pca <- prcomp(genus, scal = F)                          # PCA（参数名应为 scale.）
pc12 <- as.data.frame(otu_pca$x[, 1:2])                     # 前两主成分坐标
pc12$samples <- rownames(pc12)                              # 添加样本列

groups <- read.delim('group2.txt', header = T)              # 注意是 group2.txt
colnames(groups)[1] <- 'samples'                            # 第一列改名

groups$group1 <- factor(groups$group1,                      # group1 转因子
                        levels = groups$group1[!duplicated(groups$group1)])
groups$group2 <- factor(groups$group2,                      # group2 转因子
                        levels = groups$group2[!duplicated(groups$group2)])

pc <- summary(otu_pca)$importance[2, ] * 100                # 各 PC 解释方差
pc12 <- merge(pc12, groups, by = 'samples')                 # 合并
pc12$group <- paste(pc12$group1, pc12$group2, sep = '')     # 合并两个分组
colnames(pc12)[2:3] <- c('PC1', 'PC2')                      # 重命名

mycol <- c('#E41A1C', '#377EB8')                            # 2 色
myshape <- c(21:22)                                         # 2 形状

ggplot(data = pc12, aes(PC1, PC2)) +
  geom_point(aes(fill = group1, shape = group2), size = 3) +  # 颜色按 group1，形状按 group2
  scale_shape_manual(values = myshape) +
  scale_fill_manual(values = mycol) +
  scale_color_manual(values = mycol) +
  guides(color = 'none',
         fill = guide_legend(override.aes = list(size = 4, fill = mycol, shape = 21)),
         size = guide_legend(override.aes = list(shape = myshape))) +
  geom_hline(yintercept = 0, linetype = 2) +
  geom_vline(xintercept = 0, linetype = 2) +
  labs(x = paste0("PC1(", round(pc[1], 2), "%", ")"),
       y = paste0("PC2(", round(pc[2], 2), "%)")) +
  theme_bw() + theme(panel.grid = element_blank()) +
  stat_ellipse(aes(x = PC1, y = PC2, color = group1),        # 按 group1 画椭圆
               linetype = 1, level = 0.95)

ggsave('twogroup_pca.pdf', width = 8, height = 6)           # 保存

# ==================================================
# 模块五：两组 PCoA（带 PERMANOVA）
# ==================================================
rm(list = ls())                       # 清空环境
options(stringsAsFactors = F)         # 字符型不自动转因子
library(vegan)                        # 生态学统计
library(ggplot2)                      # 绘图
library(ggrepel)                      # 标签避免重叠
library(dplyr)                        # 数据处理

genus <- read.delim('step2.even.5000.feature-table.txt', header = T,
                    row.names = 1, check.names = F)         # 读取数据
genus <- as.data.frame(t(genus))                            # 转置
otu.dist <- vegdist(genus, method = "bray")                 # Bray-Curtis 距离

otu_pcoa <- cmdscale(otu.dist, eig = TRUE)                  # PCoA
pc12 <- as.data.frame(otu_pcoa$points[, 1:2])               # 前两轴坐标
pc12$samples <- rownames(pc12)                              # 添加样本列

groups <- read.delim('group2.txt', header = T)              # 注意是 group2.txt
colnames(groups)[1] <- 'samples'                            # 第一列改名

pc <- round(otu_pcoa$eig / sum(otu_pcoa$eig) * 100, digits = 2)  # 各轴方差
pc12 <- merge(pc12, groups, by = 'samples')                 # 合并
colnames(pc12)[2:3] <- c('PC1', 'PC2')                      # 重命名

pc12$group1 <- factor(pc12$group1, levels = unique(groups$group1))  # group1 转因子
pc12$group2 <- factor(pc12$group2, levels = unique(groups$group2))  # group2 转因子

temp <- as.matrix(otu.dist)                                 # 转矩阵
table(colnames(temp) == groups$samples)                     # 检查样本顺序是否一致

ADONIS <- adonis2(otu.dist ~ groups$group1)                 # PERMANOVA
ADONIS                                                      # 打印结果

Fvalue <- round(ADONIS[1, "F"], digits = 3)                 # ✅ 提取 F 值

TEST_adonis <- ADONIS[1, "Pr(>F)"]                          # ✅ 提取 p 值
R2adonis <- round(ADONIS[1, "R2"], digits = 3)              # ✅ 提取 R²
sink('adonis.txt')                                          # 输出重定向到文件
print(ADONIS)
sink()                                                      # 关闭重定向

mycol <- c("#E41A1C", "#377EB8")                            # 2 色
myshape <- 21:22                                            # 2 形状

ggplot(data = pc12, aes(PC1, PC2, group = group1, fill = group1, shape = group2)) +
  geom_point(color = 'black', size = 3) +                   # 散点
  scale_fill_manual(values = mycol) +
  scale_color_manual(values = mycol) +
  scale_shape_manual(values = myshape) +
  geom_hline(yintercept = 0, linetype = 2) +
  geom_vline(xintercept = 0, linetype = 2) +
  labs(x = paste0("PCoA1(", round(pc[1], 2), "%", ")"),
       y = paste0("PCoA2(", round(pc[2], 2), "%)")) +
  stat_ellipse(data = pc12, aes(x = PC1, y = PC2, fill = group1),   # 95% 椭圆
               linetype = 1, geom = 'polygon', alpha = 0.3,
               level = 0.95, show.legend = F, inherit.aes = F) +
  guides(fill = guide_legend(title = 'group1', order = 1,
                             override.aes = list(size = 4, fill = mycol, shape = 21)),
         shape = guide_legend(title = 'group2', override.aes = list(size = 3))) +
  ggtitle(label = paste('PERMANOVA:F=', Fvalue, ', p=', TEST_adonis, sep = '')) +
  theme_bw() + theme(title = element_text(size = 10))

ggsave('twogroup_pcoa.pdf', width = 7, height = 5)          # 保存

# ==================================================
# 模块六：两组 NMDS
# ==================================================
library(vegan)                        # 生态学统计
library(ggplot2)                      # 绘图
library(RColorBrewer)                 # 配色方案
library(scales)                       # 提供 scientific()

genus <- read.delim('step2.even.5000.feature-table.txt', header = T,
                    row.names = 1, check.names = F)         # 读取数据
genus <- as.data.frame(t(genus))                            # 转置
dist <- vegdist(genus, method = "bray")                     # Bray-Curtis 距离
dist <- as.matrix(dist)                                     # 转矩阵
nmds_result <- metaMDS(dist, k = 2)                         # NMDS
stress <- paste0("Stress=", scientific(nmds_result$stress, digits = 3))  # Stress 值

nmds12 <- as.data.frame(nmds_result$points)                 # NMDS 坐标
nmds12$samples <- rownames(nmds12)                          # 添加样本列

groups <- read.delim('group2.txt', header = T)              # 注意是 group2.txt
colnames(groups)[1] <- 'samples'                            # 第一列改名
nmds12 <- merge(nmds12, groups, by = 'samples')             # 合并
nmds12$group1 <- factor(nmds12$group1, levels = unique(groups$group1))  # group1 转因子
nmds12$group2 <- factor(nmds12$group2, levels = unique(groups$group2))  # group2 转因子

mycol <- c("#E41A1C", "#377EB8")                            # 2 色
myshape <- 21:22                                            # 2 形状

ggplot() +
  geom_point(data = nmds12, aes(MDS1, MDS2, fill = group1, shape = group2), size = 4) +
  scale_fill_manual(values = mycol) +
  scale_shape_manual(values = myshape) +
  geom_hline(yintercept = 0, linetype = 2) +
  geom_vline(xintercept = 0, linetype = 2) +
  labs(x = 'NMDS1', y = "NMDS2", title = stress) +
  theme_bw() + theme(panel.grid = element_blank(),
                     legend.title = element_blank()) +
  guides(fill = guide_legend(override.aes = list(shape = 21)))

ggsave('twogroup_nmds.pdf', width = 8, height = 6)          # 保存