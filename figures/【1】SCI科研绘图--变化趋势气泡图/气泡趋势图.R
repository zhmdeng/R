#如果还未安装这三个包，请先使用install.packages安装这三个包，然后再加载
library(tidyverse)
library(ggsci)
library(patchwork)


setwd("G:/生信课程/基因课资料/【会员专享】统计计量（张敬信老师主讲）")#设置你自己的文件夹
A <- read.csv("趋势气泡图.csv", header = TRUE, encoding = "UTF-8", col.names = c("expression", "Diameter", "group"))
# 转换数据类型，并处理缺失值
A$expression <- as.numeric(A$expression)
A$Diameter <- as.numeric(A$Diameter)
 
# 计算expression列的值并更新
A$expression <- 0.01 * A$expression

# 检查并处理NA值
A <- A[!is.na(A$expression), ]

p <- ggplot(A, aes(x = Diameter, y = expression, group = group, color = group)) +
  geom_point(aes(size = expression), shape = 21, color = 'grey') +
  scale_size_continuous(range = c(0, 10), breaks = c(0, 20, 40, 60, 80, 100, 120, 140, 160)) +
  stat_summary(geom = 'line', fun = 'mean', linewidth = 0.5, linetype = 2) +  # 使用linewidth代替size
  geom_point(aes(size = expression), alpha = 0.5) +
  scale_color_manual(values = c('#6666FF', '#FF66CC', 'grey', '#66CC66'))

# 添加主题和标签
p <- p + theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme(panel.border = element_blank()) +
  theme(axis.line = element_line(colour = "black", linewidth = 1)) +  # 使用linewidth代替size
  theme_classic(base_size = 15) +
  labs(title = "", y = "Relative abundance", x = "Diameter", fill = "Macrophage", size = "HLA-DR+\nexpression")

# 打印图形
print(p)



