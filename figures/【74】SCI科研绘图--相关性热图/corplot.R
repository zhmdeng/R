

library(corrplot)                # 加载 corrplot 包，用于绘制相关性矩阵图

inputFile = "input.txt"          # 定义输入文件名
# setwd("")                      # 设置工作目录

rt = read.table(inputFile,       # 读取 input.txt
                sep = "\t",      # 制表符分隔
                header = T,      # 第一行为列名
                row.names = 1)   # 第一列作为行名
# 读取结果：行=基因/变量，列=样本

rt = t(rt)                       # 转置数据
# 转置后：行=样本，列=基因/变量
# 因为 cor() 计算的是列与列之间的相关性，所以需要把变量放在列上

M = cor(rt)                      # 计算相关系数矩阵
# 默认使用 Pearson 相关系数
# M 的行列都是基因/变量，值是两个变量之间的相关系数

# ==================== 绘制第一张相关性图 ====================
pdf(file = "图1.pdf",            # 输出 PDF 文件名
    width = 7, height = 7)       # PDF 宽高，单位英寸

corrplot(M,                      # 要绘制的相关系数矩阵
         method = "circle",      # 用圆圈表示相关性，圆圈大小代表相关系数绝对值
         order = "hclust",       # 按层次聚类结果重新排序变量
         type = "upper",         # 只显示上三角矩阵
         tl.cex = 0.5,        # 标签字体大小
         tl.col = "black",    # 标签颜色
         tl.srt = 45,         # 列标签旋转 45 度
         tl.offset = 0.3,     # 标签偏移
         cl.cex = 0.5,       # 图例字体大小
         col = colorRampPalette(c("green", "white", "red"))(50)
         # 颜色方案：绿-白-红，生成 50 个渐变颜色
)

dev.off()                        # 关闭 PDF 设备，保存文件

# ==================== 绘制第二张相关性图 ====================
pdf(file = "图2.pdf",            # 输出 PDF 文件名
    width = 8, height = 8)       # PDF 宽高，单位英寸

corrplot(M,                      # 同一个相关系数矩阵
         order = "original",     # 保持原始变量顺序，不聚类排序
         method = "color",       # 用颜色方块表示相关性
         number.cex = 0.7,       # 相关系数数字的字体大小
         addCoef.col = "black",  # 在方块上显示相关系数，颜色为黑色
         diag = TRUE,            # 显示对角线
         tl.cex = 0.5,        # 标签字体大小
         tl.col = "black",    # 标签颜色
         tl.srt = 45,         # 列标签旋转 45 度
         tl.offset = 0.3,     # 标签偏移
         cl.cex = 0.5,       # 图例字体大小
         col = colorRampPalette(c("blue", "white", "red"))(50)
         # 颜色方案：蓝-白-红，生成 50 个渐变颜色
)

dev.off()                        # 关闭 PDF 设备，保存文件