# install.packages("ggpubr")   # 安装 ggpubr 包

# ========== 加载所需的包 ==========
library(ggpubr)   # 提供 ggviolin()、stat_compare_means() 等绘图函数

# ========== 定义输入输出文件 ==========
inputFile = "input.txt"     # 输入文件：数据（三列）
outFile = "vioplot.pdf"     # 输出文件：小提琴图 PDF

# setwd("")   # 设置工作目录

# ========== 读取文件 ==========
rt = read.table(inputFile, header = T, sep = "\t", check.names = F)
# 制表符分隔，第一行为列名，不修改列名中的特殊字符

# ========== 提取列名 ==========
x = colnames(rt)[2]   # 第 2 列列名作为分组变量名（x 轴标签）
y = colnames(rt)[3]   # 第 3 列列名作为数值变量名（y 轴标签）
colnames(rt) = c("id", "Type", "Expression")   # 统一重命名三列

# ========== 设置比较组 ==========
group = levels(factor(rt$Type))   # 提取 Type 的所有水平（如 "Normal", "Tumor"）
rt$Type = factor(rt$Type, levels = group)   # 将 Type 转为因子，固定水平顺序

comp = combn(group, 2)   # 生成所有两两组合（如 Normal vs Tumor）
my_comparisons = list()  # 创建空列表存放比较组
for (i in 1:ncol(comp)) {
  my_comparisons[[i]] <- comp[, i]   # 把每一列组合存入列表
}
# 最终 my_comparisons 形如：list(c("Normal","Tumor"))

# ========== 绘制小提琴图 ==========
p <- ggviolin(rt,
              x = "Type",              # x 轴为分组
              y = "Expression",        # y 轴为数值
              fill = "Type",           # 按 Type 填充颜色
              xlab = x,                # x 轴标签
              ylab = y,                # y 轴标签
              legend.title = x,        # 图例标题
              add = "boxplot",         # 在小提琴内部叠加箱线图
              add.params = list(fill = "white")) +   # 箱线图填充为白色
  stat_compare_means(comparisons = my_comparisons)
# 按 my_comparisons 中的组合做两两比较，默认使用 Wilcoxon 检验

# 如果不想显示具体 p 值，想显示符号，可以用下面的写法：
# stat_compare_means(comparisons = my_comparisons,
#                    symnum.args = list(cutpoints = c(0, 0.001, 0.01, 0.05, 1),
#                                       symbols = c("***", "**", "*", "ns")),
#                    label = "p.signif")

# ========== 输出图片 ==========
pdf(file = outFile, width = 6, height = 5)   # 打开 PDF 设备
print(p)                                       # 输出到 PDF（ggplot 对象必须 print）
dev.off()                                      # 关闭设备，保存文件