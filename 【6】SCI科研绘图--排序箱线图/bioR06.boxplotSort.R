

# install.packages("plyr")   # 安装 plyr 包（已注释）
# install.packages("ggpubr") # 安装 ggpubr 包（已注释）

# ========== 引用包 ==========
library(plyr)    # 用于数据汇总（ddply 函数）
library(ggpubr)  # 用于绘制出版级箱线图（ggboxplot 函数）

# 输入文件：包含分组和表达量数据的文本文件
inputFile = "input.txt"

# 输出文件：生成的箱线图 PDF 文件名
outFile = "boxplot.pdf"

# setwd("...")  # 设置工作目录（已注释，根据实际情况启用）

# ========== 读取数据 ==========
# 读取输入文件：
# sep="\t"：制表符分隔
# header=T：第一行为列名
# check.names=F：不自动修改列名中的特殊字符
rt = read.table(inputFile, sep = "\t", header = T, check.names = F)

# 提取第 2 列的列名作为 x 轴标签（分组变量名）
x = colnames(rt)[2]

# 提取第 3 列的列名作为 y 轴标签（表达量变量名）
y = colnames(rt)[3]

# 将数据框的列名统一改为 "id"、"Type"、"expression"
# 方便后续代码统一引用，不依赖原始列名
colnames(rt) = c("id", "Type", "expression")

# ========== 定义排序方式 ==========
# 用 ddply 按 Type 分组，计算每组的表达量中位数（median）
med = ddply(rt, "Type", summarise, med = median(expression))

# 将 Type 转换为因子，并按中位数从大到小排序
# med[order(med[,"med"], decreasing = T), "Type"]：按中位数降序排列组名
rt$Type = factor(rt$Type, levels = med[order(med[, "med"], decreasing = T), "Type"])

# ========== 绘制 ==========
# 生成颜色：按分组数量生成彩虹色
col = rainbow(length(levels(factor(rt$Type))))

# 绘制箱线图：
# ggboxplot()：ggpubr 包的箱线图函数
# rt：数据
# x="Type"：x 轴为分组
# y="expression"：y 轴为表达量
# color="Type"：按分组着色
# palette=col：指定颜色
# ylab=y：y 轴标签（原始列名）
# xlab=x：x 轴标签（原始列名）
# add="jitter"：添加散点（已注释）
# legend="right"：图例放在右侧
p = ggboxplot(rt, x = "Type", y = "expression", color = "Type",
              palette = col,
              ylab = y,
              xlab = x,
              # add = "jitter",   # 绘制每个样品的散点
              legend = "right")

# ========== 输出图片 ==========
# 打开 PDF 设备：宽度 10 英寸，高度 6 英寸
pdf(file = outFile, width = 10, height = 6)

# 打印图形，并将 x 轴文字旋转 60 度（避免重叠）
p + rotate_x_text(60)

# 关闭设备，保存文件
dev.off()


