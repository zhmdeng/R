# 加载必要的库
library(ggplot2)
library(dplyr)

#设置工作路径
# setwd("")

# 读取数据
data <- read.csv("deaths_eapc.csv")

# 重命名列以匹配原始代码
colnames(data) <- c("pathway", "value")

# 设置颜色变量
color <- rep("#ae4531", nrow(data))  # 默认颜色为正值颜色
color[which(data$value < 0)] <- "#2f73bb"  # 负值颜色
color[which(data$value == 0)] <- "#808080"  # 零值颜色
data$color <- color

# 最终绘图代码
ggplot(data) +
  geom_col(aes(reorder(pathway, value), value, fill = color)) +
  scale_fill_manual(values = c("#808080", "#2f73bb", "#ae4531")) +
  # 加一条竖线：
  annotate("segment", y = 0, yend = 0, x = 0, xend = nrow(data) + 0.8) +
  theme_classic() +
  ylim(-20, 20) +
  coord_flip() +
  # 调整主题：
  theme(
    legend.position = "none",
    plot.title = element_text(hjust = 0.5),
    axis.line.y = element_blank(),
    axis.title.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.text.y = element_blank(),
  ) +
  ylab("Normalized Enrichment Score") +
  # 添加pathway的label：
  geom_text(data = data[which(data$value >= 0), ], aes(x = pathway, y = 0, label = pathway), 
            hjust = 1.1, size = 2) +
  geom_text(data = data[which(data$value < 0), ], aes(x = pathway, y = 0, label = pathway), 
            hjust = -0.1, size = 2) +
  # 添加value的label：
  geom_text(data = data[which(data$value >= 0), ], aes(x = pathway, y = value, label = value), 
            hjust = -0.2, size = 2, color = "black") +
  geom_text(data = data[which(data$value < 0), ], aes(x = pathway, y = value, label = value), 
            hjust = 1.2, size = 2, color = "black") +
  ggtitle("Deaths EAPC \n FDR < 0.0001") +
  scale_x_discrete(expand = expansion(add = c(0, 1.5)))

# 保存图形
ggsave("图.pdf", height = 7, width = 7)
