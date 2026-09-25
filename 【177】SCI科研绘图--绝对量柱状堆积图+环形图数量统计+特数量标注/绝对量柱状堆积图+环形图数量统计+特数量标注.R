#设置工作环境
rm(list = ls())                 # 清空当前环境中的所有对象，避免旧变量干扰
# setwd("")                     # 设置工作目录

##加载R包
library(ggplot2)                # 加载 ggplot2，用于绘图
library(formattable)            # 加载 formattable，用于格式化数字（如百分比）

##加载数据（随机编写，无实际意义）
df <- read.table("data.txt", header = 1, check.names = F, sep = "\t")
# 从 data.txt 读取数据，制表符分隔，第一行为列名，不检查列名合法性
df$group <- factor(df$group, levels = df$group[1:10])
# 将 group 列转换为因子，水平顺序按 group 列前 10 个唯一值（或前10个观测值）设定

##绘图
#绘制绝对量的柱状堆积图
p1 <- ggplot(df, aes(sample, value, fill = group)) +   # 初始化 ggplot，x 轴为 sample，y 轴为 value，填充色按 group
  #绘制柱状堆积图
  geom_col(width = 0.6, color = ifelse(df$`Special marking` == 1, "black", "transparent"),
           linetype = ifelse(df$`Special marking` == 1, 2, 0)) +
  # 绘制柱状图，宽度 0.6；边框颜色和线型根据 Special marking 列决定：等于1时黑色虚线，否则透明无框
  #轴标题
  labs(y = "Absolute quantity value", x = NULL, fill = NULL) +  # 设置 y 轴标题，去掉 x 轴标题和图例标题
  #y轴范围
  scale_y_continuous(expand = c(0,0), limits = c(0, 320), 
                     breaks = c(50,100,150,200,250,300)) +
  # 设置 y 轴范围 0-320，不扩展，刻度位置为 50 到 300
  #主题相关设置
  theme_bw() +                   # 使用黑白主题
  theme(panel.grid = element_blank(),   # 去掉网格线
        axis.text.x = element_text(size = 10, color = "black", angle = 45, vjust = 1, hjust = 1),
        # x 轴文字大小 10，黑色，旋转 45 度，垂直和水平对齐调整
        axis.text.y = element_text(size = 10, color = "black"),  # y 轴文字大小 10，黑色
        axis.title.y = element_text(size = 12, color = "black")) +  # y 轴标题大小 12，黑色
  #自定义颜色
  scale_fill_manual(values = c("#ffaaaa", "#ffc2e5","#ebffac","#c1f1fc","#00c7f2",
                               "#c2ff00", "#ff0092","#ffed00","#ff0000","#cd595a"),
                    guide = guide_legend(keywidth = 1, keyheight = 1)) +
  # 手动设置填充颜色，共 10 种颜色；图例键宽高各为 1
  p1                                # 显示 p1 图形

#使用aggregate函数计算每个组的值和
data <- aggregate(value ~ group, df, sum)
# 按 group 分组，对 value 求和，得到每个组的总值，存入 data
#计算相对丰度
data$Rel <- data$value / sum(data$value)   # 计算每个组占总值的比例（相对丰度）
#转换为百分比
data$per <- percent(data$Rel, 1)            # 使用 formattable 包的 percent 函数将比例转为百分比，保留 1 位小数
data$group <- factor(data$group, levels = unique(df$group)[1:10])
# 将 data 中的 group 转换为因子，水平顺序按 df 中 group 的前 10 个唯一值设定
#确定位置
data$ymax <- cumsum(data$Rel)               # 计算累积和，作为每个矩形块的上边界
data$ymin <- c(0, head(data$ymax, n = -1))  # 下边界为上一个累积和，第一个为 0
data$labelposition <- (data$ymax + data$ymin) / 2  # 标签位置放在每个矩形块的中间
#绘制环形图
p2 <- ggplot(data, aes(ymax = ymax, ymin = ymin, xmax = 3, xmin = 2)) +  # 初始化 ggplot，设置矩形边界
  #通过方块先绘制柱状堆积图
  geom_rect(aes(fill = group)) +            # 绘制矩形，填充色按 group
  #添加标签
  geom_text(x = 2.5, aes(y = labelposition, label = per), size = 3, color = "black") +
  # 在 x=2.5 处添加百分比标签，y 位置为 labelposition，大小 3，黑色
  #通过拉大x轴范围实现环图绘制
  xlim(1, 3) +                              # 设置 x 轴范围为 1 到 3，使矩形变成环状
  #转换为极坐标
  coord_polar(theta = "y") +                # 将 y 轴转换为极坐标，形成环形图
  theme_void() +                            # 使用空白主题，去掉所有坐标轴和背景
  theme(legend.position = "none") +         # 隐藏图例
  #自定义颜色
  scale_fill_manual(values = c("#ffaaaa", "#ffc2e5","#ebffac","#c1f1fc","#00c7f2",
                               "#c2ff00", "#ff0092","#ffed00","#ff0000","#cd595a"))
# 手动设置填充颜色，与 p1 一致
p2                                          # 显示 p2 图形

##组合图形
p1 + annotation_custom(grob = ggplotGrob(p2), xmin = 3.5, xmax = 6.5, ymin = 150, ymax = 320)
# 将 p2 转换为 grob 对象，并作为自定义注释添加到 p1 的指定位置（x 3.5-6.5，y 150-320）

ggsave("绝对量柱状堆积图+环形图数量统计+特数量标注.pdf", width = 5, height = 8, dpi = 300)
# 保存最终图形为 PDF，宽度 5 英寸，高度 8 英寸，分辨率 300 dpi