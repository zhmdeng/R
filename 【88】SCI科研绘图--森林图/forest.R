inputFile = "input.txt"    # 输入文件，包含 HR、HR.95L、HR.95H、pvalue 等列
outFile = "图.pdf"         # 输出文件
# setwd("")                # 设置工作目录

# ==================== 读取数据 ====================
rt = read.table(inputFile, header = T, sep = "\t",
                row.names = 1, check.names = F)
# 读取制表符分隔的文件，第一列作为行名，不修改列名
# 行名通常是基因名，列包含 HR、HR.95L、HR.95H、pvalue

gene = rownames(rt)                         # 提取基因名
hr = sprintf("%.3f", rt$HR)                 # 格式化 HR，保留 3 位小数
hrLow = sprintf("%.3f", rt$HR.95L)          # 格式化 HR 的 95% 置信区间下限
hrHigh = sprintf("%.3f", rt$HR.95H)         # 格式化 HR 的 95% 置信区间上限
Hazard.ratio = paste0(hr, "(", hrLow, "-", hrHigh, ")")
# 拼接成 "HR(下限-上限)" 的字符串，用于左侧表格显示

pVal = ifelse(rt$pvalue < 0.001, "<0.001", sprintf("%.3f", rt$pvalue))
# p 值格式化：小于 0.001 显示 "<0.001"，否则保留 3 位小数

# ==================== 绘图设备 ====================
pdf(file = outFile, width = 6, height = 4.5)   # 打开 PDF 设备

n = nrow(rt)      # 基因数量
nRow = n + 1      # 总行数（基因数 + 1 行用于表头）
ylim = c(1, nRow) # y 轴范围

# 将画布分成左右两部分，左宽右窄（左边放文字，右边放森林图）
layout(matrix(c(1, 2), nc = 2), widths = c(3, 2))
# 注意：原代码写的是 width = c(3,2)，正确参数名是 widths，写错会报错或警告

# ==================== 左半部分：基因信息表 ====================
xlim = c(0, 3)                       # x 轴范围（文字用）
par(mar = c(4, 2, 1.5, 1.5))         # 设置边距（下、左、上、右）
plot(1, xlim = xlim, ylim = ylim, type = "n",
     axes = F, xlab = "", ylab = "")  # 创建空白画布
text.cex = 0.8                       # 字体大小

# 添加基因名（左侧对齐）
text(0, n:1, gene, adj = 0, cex = text.cex)

# 添加 p 值（右侧对齐），列位置在 x = 1.5 - 0.1 = 1.4
text(1.5 - 0.5 * 0.2, n:1, pVal, adj = 1, cex = text.cex)
# 添加表头 "pvalue"（在 n+1 位置）
text(1.5 - 0.5 * 0.2, n + 1, 'pvalue', cex = text.cex, font = 2, adj = 1)

# 添加 Hazard ratio 列（右侧对齐），列位置在 x = 3
text(3, n:1, Hazard.ratio, adj = 1, cex = text.cex)
# 添加表头 "Hazard ratio"
text(3, n + 1, 'Hazard ratio', cex = text.cex, font = 2, adj = 1)
# 注意：原代码末尾多了一个逗号，应该去掉

# ==================== 右半部分：森林图 ====================
par(mar = c(4, 1, 1.5, 1), mgp = c(2, 0.5, 0))  # 设置边距和轴标签位置
xlim = c(0, max(as.numeric(hrLow), as.numeric(hrHigh)))  # x 轴范围
plot(1, xlim = xlim, ylim = ylim, type = "n",
     axes = F, ylab = "", xaxs = "i", xlab = "Hazard ratio")
# 创建空白画布，x 轴从 0 到最大 HR 上限

# 绘制误差线（置信区间）
arrows(as.numeric(hrLow), n:1,
       as.numeric(hrHigh), n:1,
       angle = 90, code = 3, length = 0.03,
       col = "darkblue", lwd = 2.5)
# 从下限到上限画水平线段，两端带箭头

# 添加参考线 HR = 1
abline(v = 1, col = "black", lty = 2, lwd = 2)

# 根据 HR 是否大于 1 设置点的颜色
boxcolor = ifelse(as.numeric(hr) > 1, 'red', 'blue')

# 绘制点（HR 值）
points(as.numeric(hr), n:1, pch = 15, col = boxcolor, cex = 1.3)

axis(1)   # 添加 x 轴刻度

dev.off()  # 关闭 PDF 设备