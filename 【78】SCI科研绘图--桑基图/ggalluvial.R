# install.packages("ggplot2")
# install.packages("ggalluvial")

library(ggalluvial)   # 绘制冲积图（alluvial diagram）
library(ggplot2)      # 绘图核心包
library(dplyr)        # 数据处理（本段未直接使用）

inputFile = "input.txt"    # 输入文件
outFile = "图.pdf"         # 输出文件
# setwd("")                # 设置工作目录（已注释）

# ==================== 读取数据并转长格式 ====================
rt = read.table(inputFile, header = T, sep = "\t", check.names = F)
# 读取制表符分隔的数据，不修改列名

corLodes = to_lodes_form(rt, axes = 1:ncol(rt), id = "Cohort")
# to_lodes_form() 来自 ggalluvial 包
# 把所有列（1 到 ncol(rt)）作为"轴"（x 轴上的阶段）
# 生成长格式数据，包含 x、stratum、Cohort 三列
# id = "Cohort" 指定每个样本的标识列名

# ==================== 绘制冲积图 ====================


# 生成颜色向量（16 种颜色重复 15 次，共 240 个颜色）
mycol <- rep(c("#029149","#6E568C","#E0367A","#D8D155","#223D6C","#D20A13",
               "#431A3D","#91612D","#FFD121","#088247","#11AA4D","#58CDD9",
               "#7A142C","#5D90BA","#64495D","#7CC767"), 15)

p<-ggplot(corLodes, aes(x = x, stratum = stratum, alluvium = Cohort,
                     fill = stratum, label = stratum)) +
  # x = 阶段，stratum = 每个阶段的分组，alluvium = 样本标识（用于连线），fill 和 label 都按 stratum
  scale_x_discrete(expand = c(0, 0)) +          # x 轴不留空白
  # aes.flow = "forward"：连线颜色与前一个阶段一致
  # aes.flow = "backward"：连线颜色与后一个阶段一致
  geom_flow(width = 2/10, aes.flow = "forward") +   # 绘制连线（流）
  geom_stratum(alpha = .9, width = 2/10) +          # 绘制柱子（层）
  scale_fill_manual(values = mycol) +               # 手动指定填充色
  geom_text(stat = "stratum", size = 2, color = "black") +  # 在每个层上添加标签
  xlab("") + ylab("") +                             # 去掉轴标签
  theme_bw() +                                      # 黑白主题
  theme(
    axis.line = element_blank(),                    # 不显示轴线
    axis.ticks = element_blank(),                   # 不显示刻度
    axis.text.y = element_blank()                   # 不显示 y 轴文字
  ) +
  theme(panel.grid = element_blank()) +             # 不显示网格线
  theme(panel.border = element_blank()) +           # 不显示面板边框
  ggtitle("") +                                     # 标题为空
  guides(fill = FALSE)                              # 不显示 fill 图例
p
pdf(file = outFile, width = 7, height = 6)   # 打开 PDF 设备
print(p)
dev.off()   # 关闭 PDF 设备
