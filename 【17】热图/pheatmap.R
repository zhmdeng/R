# install.packages("pheatmap")   # 安装 pheatmap 包（已注释）

# ========== 加载所需的包 ==========
library(pheatmap)   # 用于绘制热图
library(grid)       # 用于 grid.newpage() 和 grid.draw() 重绘图形

# ========== 定义输入输出文件 ==========
inputFile = "input.txt"     # 输入文件：表达矩阵（行=基因，列=样本）
groupFile = "group.txt"     # 分组文件：样本属性（行=样本，列=分组信息）
outFile = "heatmap.pdf"     # 输出文件：热图 PDF

# setwd("")   # 设置工作目录（已注释，根据实际情况启用）

# ========== 读取文件 ==========
rt = read.table(inputFile, header = T, sep = "\t",     # 读取表达矩阵
                row.names = 1,                          # 第一列作为行名（基因名）
                check.names = F)                        # 不修改列名中的特殊字符

ann = read.table(groupFile, header = T, sep = "\t",    # 读取样本属性文件
                 row.names = 1,                         # 第一列作为行名（样本名）
                 check.names = F)                       # 不修改列名中的特殊字符

# ========== 绘制热图 ==========
# pheatmap() 会把图形画到当前设备（这里是 RStudio 的 Plots 面板）
# 同时返回一个 pheatmap 对象，赋值给 p，以便后续重绘到 PDF
p <- pheatmap(rt,                                       # 表达矩阵（行=基因，列=样本）
              annotation = ann,                         # 样本注释（如分组、性别等）
              cluster_cols = T,                         # 对列（样本）进行聚类
              color = colorRampPalette(c("blue", "white", "red"))(50),
              # 颜色映射：蓝→白→红，生成 50 个渐变色
              show_colnames = T,                        # 显示列名（样本名）
              scale = "row",                            # 按行标准化（基因间比较更合理）
              fontsize = 8,                             # 全局字体大小
              fontsize_row = 6,                         # 行名字体大小
              fontsize_col = 6)                         # 列名字体大小

# ========== 把同一张图输出到 PDF ==========
pdf(file = outFile, width = 6, height = 5.5)   # 打开 PDF 设备
grid.newpage()                                  # 清空当前页（新建一页空白画布）
grid.draw(p$gtable)                             # 绘制 pheatmap 返回的 gtable 对象
dev.off()                                       # 关闭 PDF 设备，保存文件