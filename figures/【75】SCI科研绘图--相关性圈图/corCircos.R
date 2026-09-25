# install.packages("corrplot")
# install.packages("circlize")

# ==================== 加载包 ====================
options(stringsAsFactors = F)   # 字符型不自动转因子
library(corrplot)               # 相关性可视化（本代码用于图例辅助）
library(circlize)               # 绘制弦图/圈图

# ==================== 定义输入输出文件 ====================
inputFile = "input.txt"         # 输入文件
outFile = "图.pdf"              # 输出图片
# setwd("")                     # 设置工作目录（已注释）

# ==================== 读取并整理数据 ====================
rt = read.table(inputFile, sep = "\t", header = T,
                check.names = F, row.names = 1)
# 读取数据：制表符分隔，第一行是列名，第一列是行名
rt = t(rt)                      # 转置：行=样本，列=基因

# ==================== 计算基因间相关系数 ====================
cor1 = cor(rt)                  # 计算基因之间的 Pearson 相关系数矩阵

# ==================== 设置图形颜色 ====================
# 生成 64 个渐变颜色：
# 前 32 个是红色，透明度从 1 到 0
# 后 32 个是绿色，透明度从 0 到 1
col = c(rgb(1, 0, 0, seq(1, 0, length = 32)),
        rgb(0, 1, 0, seq(0, 1, length = 32)))

cor1[cor1 == 1] = 0             # 把自身相关（=1）改为 0，避免画自环

# 根据相关系数正负赋予颜色：
# 正相关 → 红色，透明度 = |r|
# 负相关 → 绿色，透明度 = |r|
c1 = ifelse(c(cor1) >= 0,
            rgb(1, 0, 0, abs(cor1)),   # 正相关：红色
            rgb(0, 1, 0, abs(cor1)))   # 负相关：绿色

col1 = matrix(c1, nc = ncol(rt))    # 恢复成矩阵，列数与基因数一致

# ==================== 绘制圈图 ====================
pdf(outFile, width = 7, height = 7)   # 打开 PDF 设备，7×7 英寸
par(mar = c(2, 2, 2, 4))              # 设置边距（下、左、上、右）

circos.par(
  gap.degree = c(3, rep(2, nrow(cor1) - 1)),
  # 圈上相邻扇区之间的间隙角度：
  # 第一个间隙 3 度，其余各 2 度
  start.degree = 180
  # 起始角度为 180 度（从左侧开始）
)

chordDiagram(
  cor1,                              # 相关系数矩阵
  grid.col = rainbow(ncol(rt)),      # 每个基因分配一种彩虹色
  col = col1,                        # 弦的颜色（由 col1 矩阵指定）
  transparency = 0.5,                # 弦的透明度 0.5
  symmetric = T                      # 对称模式（相关系数矩阵是对称的）
)

par(xpd = T)                         # 允许绘图超出绘图区域

# ==================== 添加颜色图例 ====================
colorlegend(
  col,                               # 颜色向量
  vertical = T,                      # 垂直方向
  labels = c(1, 0, -1),              # 图例刻度：1、0、-1
  xlim = c(1.1, 1.3),                # 图例 x 位置
  ylim = c(-0.4, 0.4)                # 图例 y 位置
)
# 原代码注释是乱码（编码问题），这里补充说明：
# 这个图例表示相关系数从 1（红）经过 0 到 -1（绿）

dev.off()                            # 关闭 PDF 设备
circos.clear()                       # 清除 circlize 的绘图参数，避免影响后续绘图