# install.packages("ggpubr")   # 安装 ggpubr 包（已注释）

# ========== 加载所需的包 ==========
library(reshape2)   # 用于数据变形（melt 函数）
library(ggpubr)     # 提供 publication-ready 的绘图函数（ggboxplot、stat_compare_means 等）

# ========== 定义输入输出文件 ==========
inputFile = "input.txt"   # 输入文件：基因表达数据
outFile = "plot.pdf"      # 输出文件：箱线图 PDF

# setwd("")   # 设置工作目录（已注释，根据实际情况启用）

# ========== 读取输入文件 ==========
rt = read.table(inputFile, sep = "\t", header = T,     # 制表符分隔，第一行为列名
                check.names = F,                        # 不修改列名中的特殊字符
                row.names = 1)                          # 第一列作为行名

# ========== 准备分组信息 ==========
x = colnames(rt)[1]          # 保存第一列的列名（作为分组变量名，如 "Type"）
colnames(rt)[1] = "Type"     # 将第一列重命名为 "Type"（统一命名，方便后续处理）

# ========== 数据转换为长格式 ==========
data = melt(rt, id.vars = c("Type"))   # 将宽表转为长表：Type 为分组列，其余列变成长表
colnames(data) = c("Type", "Gene", "Expression")   # 重命名列：Type（分组）、Gene（基因名）、Expression（表达量）

# ========== 绘制箱线图 ==========
p = ggboxplot(data,
              x = "Gene",              # x 轴为基因名
              y = "Expression",        # y 轴为表达量
              color = "Type",          # 按 Type 分组着色（边框和点）
              ylab = "Gene expression",# y 轴标签
              xlab = "",               # x 轴标签为空
              legend.title = x,        # 图例标题为原始第一列的列名
              palette = c("blue", "red"),  # 手动指定两组颜色
              width = 0.6,             # 箱体宽度
              add = "none")            # 不添加额外的点或线

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

# ========== 输出图片 ==========
pdf(file = outFile, width = 6, height = 5)   # 打开 PDF 设备
print(p1)                                     # 打印图形到设备
dev.off()                                     # 关闭设备，保存文件