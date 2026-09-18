rm(list = ls())   # 清空环境
# setwd('')   # 设置工作目录

# ==================== 加载包 ====================
library(ggplot2)
library(RColorBrewer)
library(grid)
library(scales)
library(dplyr)
library(ggrepel)   # 已加上，geom_text_repel 可用

# ==================== 读取数据 ====================
df <- read.table("data.txt", header = 1, check.names = F, sep = "\t")
df$group <- factor(df$group, levels = c("group1-group2", "group1-group3", "group2-group3"))

# 根据 p 值和 log2FC 判定上/下调/不显著
df$group2 <- as.factor(ifelse(df$p_value < 0.05 & abs(df$log2FC) >= 2,
                              ifelse(df$log2FC >= 2, 'up', 'down'), 'NS'))
df$group2 <- factor(df$group2, levels = c("up", "down", "NS"))

# 标记需要添加标签的点（|log2FC| >= 4 且显著）
df$label <- ifelse(df$p_value < 0.05 & abs(df$log2FC) >= 4, "Y", "N")
df$label <- ifelse(df$label == 'Y', as.character(df$OTU), '')

# ==================== 计算每组的最大/最小 log2FC ====================
df_bg <- df %>%
  group_by(group) %>%
  dplyr::summarize(
    max_log2FC = max(log2FC),
    min_log2FC = min(log2FC)
  )

# ==================== 分步绘制 ====================
# p：灰色背景柱子
p <- ggplot() +
  geom_col(data = df_bg, aes(group, max_log2FC), fill = "grey85", width = 0.8, alpha = 0.5) +
  geom_col(data = df_bg, aes(group, min_log2FC), fill = "grey85", width = 0.8, alpha = 0.5)

# p1：叠加数据点
p1 <- p + geom_jitter(data = df, aes(x = group, y = log2FC, color = group2),
                      size = 3, width = 0.4, alpha = 0.7)

# p2：用 geom_col 在 y=±0.3 处添加色块
p2 <- p1 + geom_col(data = df_bg, aes(x = group, y = 0.3, fill = group)) +
  geom_col(data = df_bg, aes(x = group, y = -0.3, fill = group))

# p2 被重新赋值（geom_rect 版本），覆盖了上面的 p2
p2 <- p1 + geom_rect(data = df_bg, aes(xmin = 1-0.4, xmax = 1+0.4, ymin = -0.3, ymax = 0.3),
                     alpha = 0.5, color = NA, fill = "red", show.legend = F) +
  geom_rect(data = df_bg, aes(xmin = 2-0.4, xmax = 2+0.4, ymin = -0.3, ymax = 0.3),
            alpha = 0.5, color = NA, fill = "green", show.legend = F) +
  geom_rect(data = df_bg, aes(xmin = 3-0.4, xmax = 3+0.4, ymin = -0.3, ymax = 0.3),
            alpha = 0.5, color = NA, fill = "yellow", show.legend = F)

# p3：在色块上添加分组文字
p3 <- p2 + geom_text(data = df_bg, aes(x = group, y = 0, label = group),
                     size = 4, color = "#dbebfa")

# ==================== 最终绘图 ====================
ggplot() +
  geom_col(data = df_bg, aes(group, max_log2FC), fill = "grey85", width = 0.8, alpha = 0.5) +
  geom_col(data = df_bg, aes(group, min_log2FC), fill = "grey85", width = 0.8, alpha = 0.5) +
  geom_jitter(data = df, aes(x = group, y = log2FC, color = group2),
              size = 3, width = 0.4, alpha = 0.7) +
  geom_col(data = df_bg, aes(x = group, y = 0.4, fill = group), width = 0.8) +
  geom_col(data = df_bg, aes(x = group, y = -0.4, fill = group), width = 0.8) +
  geom_text(data = df_bg, aes(x = group, y = 0, label = group),
            size = 4, color = "#dbebfa", fontface = "bold") +
  scale_color_manual(values = c("#e42313", "#0061d5", "#8b8c8d")) +
  scale_fill_manual(values = c("#ed7902", "#ef5734", "#b5c327")) +
  geom_text_repel(data = df, aes(x = group, y = log2FC, label = label),
                  max.overlaps = 10000, size = 3,
                  box.padding = unit(0.8, 'lines'),
                  point.padding = unit(0.8, 'lines'),
                  segment.color = 'black', show.legend = FALSE) +
  theme_classic() +
  theme(axis.line.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.line.y = element_line(linewidth = 0.8),
        axis.text.y = element_text(size = 12, color = "black"),
        axis.title = element_text(size = 14, color = "black"),
        axis.ticks.y = element_line(linewidth = 0.8)) +
  labs(x = "group", y = "Log2FoldChange", fill = NULL, color = NULL) +
  guides(color = guide_legend(override.aes = list(size = 6, alpha = 1)))

# ==================== 添加背景色（有问题） ====================
color <- colorRampPalette(brewer.pal(11, "BrBG"))(30)
grid.raster(alpha(color, 0.2),                    # ❌ grid.raster 画在当前设备上
            width = unit(1, "npc"),
            height = unit(1, "npc"),
            interpolate = T)

ggsave("图.pdf", dpi = 300)