#########
######推文题目：跟着Nature Communications学绘图—并列柱状堆积图+误差线+显著性！！！

rm(list=ls())                 # 清空当前环境中的所有对象，避免旧变量干扰
# setwd('')                   # 设置工作路径

##加载R包
library(ggplot2)              # 加载 ggplot2，用于绘图
library(dplyr)                # 加载 dplyr，用于数据操作
library(rstatix)              # 加载 rstatix，用于统计检验（如 wilcox_test）
library(ggpubr)               # 加载 ggpubr，用于在 ggplot 上添加显著性标记

##加载数据
df <- read.table("data.txt", header = 1, check.names = F, sep = "\t")
# 从 data.txt 读取数据，制表符分隔，第一行为列名，不检查列名合法性

##计算均值及误差，并按照均值确定误差线位置
df %>% 
  group_by(X, Period, Group) %>%              # 按 X、Period、Group 三个变量分组
  summarise(mean = mean(value), sd = sd(value, na.rm = T)) %>%  # 计算每组的均值和标准差（忽略缺失值）
  mutate(ep = cumsum(mean)) -> df1            # 计算累积均值 ep，用于堆积柱状图的上边界位置
# 说明：cumsum 在每个 (X, Period) 分组内按 Group 的顺序累积求和，得到堆积柱的累积高度

#保证误差线可以对应其正确位置
df1$Group <- factor(df1$Group, levels = c("group4","group3","group2","group1"))
# 将 Group 转换为因子，并指定水平顺序为 group4 -> group3 -> group2 -> group1
# 这样在堆积图中，group4 在最下方，group1 在最上方（取决于 geom_col 的堆叠顺序）

##根据分类变量将其转变为数值型变量
#“A”“B”“C”“D”中间各隔开一个数值,根据个人需要确立间隔即可
df1$X2 <- ifelse(df1$X == "A" & df1$Period == "T1", 1,
                 ifelse(df1$X == "A" & df1$Period == "T2", 2,
                        ifelse(df1$X == "B" & df1$Period == "T1", 3.5,
                               ifelse(df1$X == "B" & df1$Period == "T2", 4.5,
                                      ifelse(df1$X == "C" & df1$Period == "T1", 6, 7)))))
# 将分类变量 X 和 Period 映射为数值型 x 坐标：
# A-T1 -> 1, A-T2 -> 2, B-T1 -> 3.5, B-T2 -> 4.5, C-T1 -> 6, C-T2 -> 7
# 这样可以在 x 轴上留出间隔，使不同大组（A、B、C）分开显示

##计算显著性
df_sig <- df %>% 
  group_by(X) %>%                             # 按 X 分组
  wilcox_test(value ~ Period) %>%             # 对每个 X 组，用 Wilcoxon 秩和检验比较 Period 两组（T1 vs T2）
  adjust_pvalue() %>%                         # 对 p 值进行多重检验校正（默认 BH 方法）
  add_significance("p.adj") %>%               # 根据校正后的 p 值添加显著性标记（如 *、**、***）
  add_xy_position(x = "X")                    # 为显著性标记自动计算 x、y 位置（基于 ggplot 的坐标）

##根据数据调整标签位置
#y轴位置
df_sig$y.position <- ifelse(df_sig$X == "A", max(df1[df1$X == "A", ]$ep) + 15,  # A 组的 y 位置：A 组最大累积高度 + 15
                            ifelse(df_sig$X == "B", max(df1[df1$X == "B", ]$ep) + 15,  # B 组类似
                                   max(df1[df1$X == "C", ]$ep) + 15))                # C 组类似
# 说明：df1 中每个 X 组有多个 (Period, Group) 行，ep 是累积高度，取最大值再加偏移量，作为显著性标记的 y 坐标

#x轴位置,根据此前添加数值型数据添加
df_sig$xmin <- ifelse(df_sig$X == "A", 1,     # A 组显著性横线的左端点 x 坐标
                      ifelse(df_sig$X == "B", 3.5,  # B 组左端点
                             6))                    # C 组左端点
df_sig$xmax <- ifelse(df_sig$X == "A", 2,     # A 组显著性横线的右端点 x 坐标
                      ifelse(df_sig$X == "B", 4.5,  # B 组右端点
                             7))                    # C 组右端点
# 说明：这些 x 坐标与之前 X2 的数值对应，确保显著性横线覆盖 T1 和 T2 两个柱子

##绘图
df1 %>% 
  ggplot() +
  #绘制柱状堆积图
  geom_col(aes(X2, mean, fill = Group), position = 'stack', 
           width = 1, color = "black", alpha = 0.8) +
  # 用 df1 绘制堆积柱状图：x 轴为 X2（数值坐标），y 轴为 mean，填充色按 Group
  # position = 'stack' 表示堆叠，width = 1 表示柱宽，color = "black" 为边框色，alpha = 0.8 为透明度
  
  #添加误差线
  geom_errorbar(aes(X2, ymin = ep - sd, ymax = ep + sd),
                width = 0.1, color = "black", linewidth = 0.6) +
  # 添加误差线：x 为 X2，上下界为累积均值 ep ± 标准差 sd
  # width = 0.1 控制误差线宽度，color 和 linewidth 设置颜色和线宽
  
  #自定义X轴标签
  scale_x_continuous(breaks = c(1, 2, 3.5, 4.5, 6, 7),
                     labels = c("T1", "T2", "T1", "T2", "T1", "T2"),
                     limits = c(0.5, 7.5)) +
  # 设置 x 轴刻度位置和标签：1->T1, 2->T2, 3.5->T1, 4.5->T2, 6->T1, 7->T2
  # limits 设置 x 轴范围，留出边距
  
  #确定y轴范围
  scale_y_continuous(expand = c(0, 0), limits = c(0, 190)) +
  # y 轴范围 0-190，不扩展（expand = c(0,0) 表示不从 0 留白）
  
  #主题相关设置
  theme_bw() +                                 # 使用黑白主题
  theme(panel.grid = element_blank(),          # 去掉网格线
        legend.position = "right",             # 图例放在右侧
        legend.background = element_blank(),   # 图例背景透明
        axis.text = element_text(size = 12),   # 坐标轴文字大小 12
        axis.title = element_text(size = 15),  # 坐标轴标题大小 15
        legend.title = element_text(color = 'red', size = 15),  # 图例标题红色，大小 15
        legend.text = element_text(color = 'black', size = 11)) +  # 图例文字黑色，大小 11
  
  #图例设置
  guides(fill = guide_legend(ncol = 1, byrow = F, reverse = T,
                             keywidth = 1, keyheight = 3)) +
  # 设置填充图例：单列，不按行填充，反转顺序，键宽 1，键高 3
  
  labs(x = "Period", y = "mean") +             # 设置 x 轴标题为 Period，y 轴标题为 mean
  
  #自定义颜色
  scale_fill_manual(values = c("#ff3c41", "#fcd000", "#47cf73", "#76daff")) +
  # 手动设置填充颜色，对应 Group 的四个水平
  
  #根据计算的显著性数据添加显著性标签
  stat_pvalue_manual(df_sig[1, ], label = "p.adj.signif",
                     label.size = 5.5, linetype = 2,
                     tip.length = c(0.55, 0.05)) +
  # 为第一组（A）添加显著性标记：使用 df_sig 第 1 行数据
  # label = "p.adj.signif" 表示显示校正后的显著性符号
  # label.size = 5.5 标签大小，linetype = 2 虚线，tip.length 控制横线两端下折长度
  
  stat_pvalue_manual(df_sig[2, ], label = "p.adj.signif",
                     label.size = 6, linetype = 2,
                     tip.length = c(0.05, 0.15)) +
  # 为第二组（B）添加显著性标记，使用 df_sig 第 2 行数据
  
  stat_pvalue_manual(df_sig[3, ], label = "p.adj.signif",
                     label.size = 6, linetype = 2,
                     tip.length = c(0.05, 0.6)) +
  # 为第三组（C）添加显著性标记，使用 df_sig 第 3 行数据
  
  ##添加大的分组注释
  geom_segment(aes(x = 1, xend = 2, y = 170, yend = 170),
               color = "black", linewidth = 0.8) +
  # 在 A 组上方画一条水平线段，从 x=1 到 x=2，y=170
  annotate("text", x = 1.5, y = 180, label = "A", size = 6, color = "#c68143") +
  # 在 A 组线段上方添加文本 "A"，位置 (1.5, 180)
  
  geom_segment(aes(x = 3.5, xend = 4.5, y = 170, yend = 170),
               color = "black", linewidth = 0.8) +
  # 在 B 组上方画水平线段，从 x=3.5 到 x=4.5，y=170
  annotate("text", x = 4, y = 180, label = "B", size = 6, color = "#c68143") +
  # 在 B 组线段上方添加文本 "B"
  
  geom_segment(aes(x = 6, xend = 7, y = 170, yend = 170),
               color = "black", linewidth = 0.8) +
  # 在 C 组上方画水平线段，从 x=6 到 x=7，y=170
  annotate("text", x = 6.5, y = 180, label = "C", size = 6, color = "#c68143")
# 在 C 组线段上方添加文本 "C"

ggsave("并列柱状堆积图+误差线+显著性.pdf", width = 6, height = 6, dpi = 300)
# 保存最终图形为 PDF，宽度 6 英寸，高度 6 英寸，分辨率 300 dpi