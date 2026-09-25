

# ========== 加载所需的包 ==========
library(reshape2)   # 用于数据变形（melt 函数）
library(ggpubr)     # 提供 ggviolin()、stat_compare_means() 等绘图函数

# ========== 定义输入输出文件 ==========
inputFile = "input.txt"     # 输入文件：表达矩阵（行=样本，列=Type+多个基因）
outFile = "vioplot.pdf"     # 输出文件：小提琴图 PDF

# setwd("")   # 设置工作目录

# ========== 读取文件 ==========
rt = read.table(inputFile, header = T, sep = "\t",     # 制表符分隔，第一行为列名
                check.names = F,                        # 不修改列名中的特殊字符
                row.names = 1)                          # 第一列作为行名（样本名）

# ========== 提取列名 ==========
x = colnames(rt)[1]          # 第 1 列列名作为分组变量名（如 "Type"）
colnames(rt)[1] = "Type"     # 将第 1 列重命名为 "Type"，方便后续处理

# ========== 数据转换为长格式 ==========
data = melt(rt, id.vars = c("Type"))   # 宽转长：Type 为分组列，其余变成长表
colnames(data) = c("Type", "Gene", "Expression")   # 重命名列：Type、Gene、Expression

# ========== 绘制小提琴图 ==========
p = ggviolin(data,
             x = "Gene",                 # x 轴为基因名（每个基因一个小提琴）
             y = "Expression",           # y 轴为表达量
             color = "Type",             # 按 Type 着色（边框和点）
             ylab = "Gene expression",   # y 轴标签
             xlab = x,                   # x 轴标签（原始第 1 列列名）
             legend.title = x,           # 图例标题
             add.params = list(fill = "white"),   # 内部箱线图填充为白色
             palette = c("blue", "red"), # 手动指定两组颜色
             width = 1,                  # 小提琴宽度
             add = "boxplot")            # 在小提琴内部叠加箱线图

# ========== 旋转 x 轴文字 ==========
p = p + rotate_x_text(60)   # x 轴文字旋转 60 度，避免重叠

# ========== 添加显著性标注 ==========
p1 = p + stat_compare_means(
  aes(group = Type),            # 按 Type 分组比较
  method = "wilcox.test",       # 使用 Wilcoxon 秩和检验
  symnum.args = list(           # 自定义显著性符号
    cutpoints = c(0, 0.001, 0.01, 0.05, 1),   # p 值阈值
    symbols = c("***", "**", "*", " ")         # 对应符号：*** / ** / * / 空白
  ),
  label = "p.signif"            # 显示显著性符号（而非具体 p 值）
)

p1   # 在 RStudio 面板中显示

# ========== 输出图片 ==========
pdf(file = outFile, width = 6, height = 5)   # 打开 PDF 设备
print(p1)                                      # 输出到 PDF
dev.off()                                      # 关闭设备，保存文件