# ========== 加载所需的包 ==========
library(readxl)      # 读取 Excel 文件
library(tidyverse)   # 数据处理和绘图
library(aplot)       # 用于在图形边缘插入子图（如密度曲线）
library(vegan)       # 生态学统计（这里可能用于后续分析）
library(patchwork)   # 用于组合多个图形
library(cowplot)     # 图形组合工具
library(grid)        # 图形系统底层工具
library(ggplotify)   # 将非 ggplot 图形转换为 ggplot 对象

# library(pairwiseAdonis)  # 用于成对 PERMANOVA 分析（已注释）

# ========== 设置工作目录 ==========
# setwd("/Users/mac/Desktop/PowerBI/sci科研绘图/【40】.../")

# ========== 读取第一个数据集：分类学 PCA ==========
dat.taxa <- read_excel("F1.xlsx", sheet = "Fig 1a taxonomy PCA")   # 从 Excel 指定 sheet 读取
pca <- dat.taxa                                                     # 复制到 pca 变量

# ========== 绘制 Fig1a 分类学 PCA 主图 ==========
Fig1a.taxa.pca <- ggplot(pca, aes(PC1, PC2)) +                      # x 轴 PC1，y 轴 PC2
  geom_point(size = 2, aes(color = Disease, shape = Cohort), show.legend = F) +  # 散点，颜色按 Disease，形状按 Cohort
  scale_color_manual(values = c("#5686C3", "#75C500")) +            # 手动指定颜色
  scale_shape_manual(values = c(16, 15)) +                          # 手动指定点形状（16=实心圆，15=实心方块）
  stat_ellipse(aes(color = Disease), fill = "white", geom = "polygon",  # 添加 95% 置信椭圆
               level = 0.95, alpha = 0.01, show.legend = F) +
  labs(x = "PC1 (23.3%)", y = "PC2 (9.1%)") +                       # 轴标签带解释方差比例
  theme_classic() +                                                 # 经典主题（无网格线）
  theme(
    axis.line = element_line(colour = "black"),                     # 轴线为黑色
    axis.title = element_text(color = "black", face = "bold"),      # 轴标题：黑色、粗体
    panel.grid.major = element_blank(),                             # 无主网格线
    panel.grid.minor = element_blank(),                             # 无次网格线
    panel.background = element_blank(),                             # 面板背景透明
    axis.text = element_text(color = "black", size = 10, face = "bold")  # 轴文字：黑色、10号、粗体
  )

# ========== 创建分组变量 ==========
pca$Group = paste(pca$Disease, "|", pca$Cohort, sep = "")           # 将 Disease 和 Cohort 组合成新分组变量

# ========== 绘制 PC1 方向的密度曲线（顶部） ==========
Fig1a.taxa.pc1.density <-
  ggplot(pca) +
  geom_density(aes(x = PC1, group = Group, fill = Disease, linetype = Cohort),  # 按 Group 分组画密度曲线
               color = "black", alpha = 0.6, position = 'identity',              # 黑色边框，透明度 0.6，不堆叠
               show.legend = F) +
  scale_fill_manual(values = c("#5686C3", "#75C500")) +             # 填充色
  scale_linetype_manual(values = c("solid", "dashed")) +            # 线型：实线/虚线
  scale_y_discrete(expand = c(0, 0.001)) +                          # y 轴不留空白
  labs(x = NULL, y = NULL) +                                        # 无轴标签
  theme_classic() +
  theme(
    axis.text.x = element_blank(),                                  # x 轴文字不显示
    axis.ticks.x = element_blank()                                  # x 轴刻度不显示
  )

# ========== 绘制 PC2 方向的密度曲线（右侧） ==========
Fig1a.taxa.pc2.density <-
  ggplot(pca) +
  geom_density(aes(x = PC2, group = Group, fill = Disease, linetype = Cohort),
               color = "black", alpha = 0.6, position = 'identity', show.legend = F) +
  scale_fill_manual(values = c("#5686C3", "#75C500")) +
  theme_classic() +
  scale_linetype_manual(values = c("solid", "dashed")) +
  scale_y_discrete(expand = c(0, 0.001)) +
  labs(x = NULL, y = NULL) +
  theme(
    axis.text.y = element_blank(),                                  # y 轴文字不显示
    axis.ticks.y = element_blank()                                  # y 轴刻度不显示
  ) +
  coord_flip()                                                      # 翻转坐标轴，让密度曲线横向显示

# ========== 组合分类学 PCA 图 ==========
p1 <- Fig1a.taxa.pca %>%
  insert_top(Fig1a.taxa.pc1.density, height = 0.3) %>%              # 在主图顶部插入 PC1 密度曲线
  insert_right(Fig1a.taxa.pc2.density, width = 0.3) %>%             # 在主图右侧插入 PC2 密度曲线
  as.ggplot()                                                       # 转换为 ggplot 对象，方便后续组合

# ==================== 分隔线：功能 PCA 部分 ====================

# ========== 读取第二个数据集：宏基因组功能 PCA ==========
dat.funct <- read_excel("F1.xlsx", sheet = "Fig 1a metagenome PCA")
pca <- dat.funct                                                    # 更新 pca 变量

# ========== 绘制 Fig1a 功能 PCA 主图 ==========
Fig1a.function.pca <- ggplot(pca, aes(PC1, PC2)) +
  geom_point(size = 2, aes(col = Disease, shape = Cohort)) +        # 注意这里用了 col（与前面 color 等价）
  scale_shape_manual(values = c(16, 15)) +
  scale_color_manual(values = c("#5686C3", "#75C500")) +
  stat_ellipse(aes(color = Disease), fill = "white", geom = "polygon",
               level = 0.95, alpha = 0.01, show.legend = F) +
  labs(x = "PC1 (17.3%)", y = "PC2 (7.3%)") +
  theme_classic() +
  theme(
    axis.line = element_line(colour = "black"),
    axis.title = element_text(color = "black", face = "bold"),
    axis.text = element_text(color = "black", face = "bold", size = 10),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank()
  )

# ========== 创建功能 PCA 的分组变量 ==========
pca$Group = paste(pca$Disease, "|", pca$Cohort, sep = "")
Fig1a.function.pca                                                 # 查看主图

# ========== 绘制功能 PCA 的 PC1 密度曲线 ==========
Fig1a.function.pc1.density <-
  ggplot(pca) +
  geom_density(aes(x = PC1, group = Group, fill = Disease, linetype = Cohort),
               color = "black", alpha = 0.6, position = 'identity', show.legend = F) +
  scale_fill_manual(values = c("#5686C3", "#75C500")) +
  theme_classic() +
  scale_linetype_manual(values = c("solid", "dashed")) +
  scale_y_discrete(expand = c(0, 0.001)) +
  labs(x = NULL, y = NULL) +
  theme_classic() +
  theme(
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank()
  )

# ========== 绘制功能 PCA 的 PC2 密度曲线 ==========
Fig1a.function.pc2.density <-
  ggplot(pca) +
  geom_density(aes(x = PC2, group = Group, fill = Disease, linetype = Cohort),
               color = "black", alpha = 0.6, position = 'identity', show.legend = F) +
  scale_fill_manual(values = c("#5686C3", "#75C500")) +
  scale_linetype_manual(values = c("solid", "dashed")) +
  scale_y_discrete(expand = c(0, 0.001)) +
  labs(x = NULL, y = NULL) +
  theme_classic() +
  coord_flip() +                                                    # 翻转坐标轴
  theme(
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank()
  )

# ========== 组合功能 PCA 图 ==========
p2 <- Fig1a.function.pca %>%
  insert_top(Fig1a.function.pc1.density, height = 0.3) %>%          # 顶部插入 PC1 密度曲线
  insert_right(Fig1a.function.pc2.density, width = 0.3) %>%         # 右侧插入 PC2 密度曲线
  as.ggplot()

# ========== 用 patchwork 组合两张 PCA 图 ==========
(p1 | p2) + plot_layout(ncol = 2, width = c(0.8, 1))
# 将 p1 和 p2 左右并排，p1 占 0.8 宽度，p2 占 1 宽度

# ========== 单独查看 p1 ==========
p1

# ========== 保存图片 ==========
ggsave("plot.pdf", width = 5, height = 5)