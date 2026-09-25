rm(list = ls())
setwd("D:/桌面/SCI论文写作与绘图/R语言绘图/基础图形绘制/分组箱线图")

#加载R包
library(ggplot2) # Create Elegant Data Visualisations Using the Grammar of Graphics

#加载数据（随机编写，无实际意义）
df <- read.table("data.txt", header = 1, check.names = F, sep = "\t")

#绘图
p1 <- ggplot(df, aes(group, value, fill = XG))+
  geom_boxplot(linewidth = 0.6)+#箱线图绘制函数
  #添加分组矩形
  annotate("rect", xmin = 0.4, xmax = 1.5, ymin = -Inf, ymax = Inf, alpha = 0.2,fill="#c1f1fc") +
  annotate("rect", xmin = 1.5, xmax = 2.5, ymin = -Inf, ymax = Inf, alpha = 0.2,fill="#ebffac") +
  annotate("rect", xmin = 2.5, xmax = 3.5, ymin = -Inf, ymax = Inf, alpha = 0.2,fill="#53d769")+
  annotate("rect", xmin = 3.5, xmax = 4.5, ymin = -Inf, ymax = Inf, alpha = 0.2,fill="#ffaaaa") +
  annotate("rect", xmin = 4.5, xmax = 5.6, ymin = -Inf, ymax = Inf, alpha = 0.2,fill="#d7dcdd")+
  #添加散点
  geom_dotplot(dotsize = 0.8,binaxis = "y", stackdir = "center",position = position_dodge(0.8))+
  #添加分组辅助线
  geom_vline(xintercept = 1.5, lty="dashed", color = "grey50", linewidth = 0.8)+
  geom_vline(xintercept = 2.5, lty="dashed", color = "grey50", linewidth = 0.8)+
  geom_vline(xintercept = 3.5, lty="dashed", color = "grey50", linewidth = 0.8)+
  geom_vline(xintercept = 4.5, lty="dashed", color = "grey50", linewidth = 0.8)+
  #主题
  theme_bw()+
  theme(axis.text.y = element_text(size=10, color = "#204056"),
        axis.text.x = element_text(size=10, angle = 45, hjust = 1, vjust = 1, color = "#204056"),
        axis.title = element_blank(),
        panel.grid = element_blank())+
  #自定义颜色
  scale_fill_manual(values = c("#0ebeff", "#47cf73", "#ae63e4", "#fcd000", "#ff3c41"))
p1

##以分面形式进行展示
p2 <- ggplot(df, aes(XG, value, fill = XG))+
  geom_boxplot(linewidth = 0.6)+#箱线图绘制函数
  geom_point(shape = 21,size=2)+
  #分面
  facet_grid(~group)+
  #主题
  theme_bw()+
  theme(axis.text.y = element_text(size=10, color = "#204056"),
        axis.text.x = element_text(size=10, color = "#204056"),
        axis.title = element_blank(),
        panel.grid = element_blank())+
  #自定义颜色
  scale_fill_manual(values = c("#0ebeff", "#47cf73", "#ae63e4", "#fcd000", "#ff3c41"))
p2
#拼图
p1/p2


