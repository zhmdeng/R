# install.packages("ggplot2")   # 安装 ggplot2

# ==================== 加载包 ====================
library(ggplot2)      # 绘图核心包
library(reshape2)     # 提供 melt()，把宽表转成长表

# ==================== 定义输入输出文件 ====================
inputFile = "input.txt"    # 输入文件：行=样本，列=Type + 多个基因
outFile = "vioplot.pdf"    # 输出文件：小提琴图 PDF
# setwd("")                # 设置工作目录

# ==================== 读取数据 ====================
rt = read.table(inputFile, header = T, sep = "\t",
                check.names = F, row.names = 1)
# 读取制表符分隔的数据
# header = T：第一行为列名
# check.names = F：不修改列名中的特殊字符
# row.names = 1：第一列作为行名（样本名）

x = colnames(rt)[1]        # 提取第 1 列的列名（分组变量名，如 "Type"）
colnames(rt)[1] = "Type"   # 把第 1 列重命名为 "Type"，方便后续统一处理

# ==================== 差异分析（逐个基因做检验） ====================
geneSig = c("")            # 初始化一个空字符串向量，用于存放每个基因的显著性符号
for (gene in colnames(rt)[2:ncol(rt)]) {   # 遍历除 Type 外的每个基因列
  rt1 = rt[, c(gene, "Type")]            # 提取当前基因和 Type 两列
  colnames(rt1) = c("expression", "Type") # 重命名，方便公式使用
  p = 1                                   # 初始化 p 值
  if (length(levels(factor(rt1$Type))) > 2) {   # 如果分组数 > 2
    test = kruskal.test(expression ~ Type, data = rt1)  # Kruskal-Wallis 检验
    p = test$p.value
  } else {                                # 如果分组数 = 2
    test = wilcox.test(expression ~ Type, data = rt1)   # Wilcoxon 秩和检验
    p = test$p.value
  }
  # 根据 p 值生成显著性符号：<0.001→***, <0.01→**, <0.05→*, 否则空白
  Sig = ifelse(p < 0.001, "***",
               ifelse(p < 0.01, "**",
                      ifelse(p < 0.05, "*", "")))
  geneSig = c(geneSig, Sig)               # 把当前基因的显著性符号加入向量
}
# 把显著性符号拼接到每个基因名后面（如 "TP53**"），用于分面标签
colnames(rt) = paste0(colnames(rt), geneSig)

# ==================== 把数据转换成 ggplot2 输入格式（长表） ====================
data = melt(rt, id.vars = c("Type"))        # 宽转长：Type 为分组列，其余变成长表
colnames(data) = c("Type", "Gene", "Expression")  # 重命名列：Type、Gene、Expression

# ==================== 绘制小提琴图 + 箱线图 ====================
p1 = ggplot(data, aes(x = Type, y = Expression, fill = Type)) +  # x=分组，y=表达量，填充按分组
  guides(fill = guide_legend(title = x)) +                     # 图例标题为原始分组变量名
  labs(x = x, y = "Gene expression") +                         # 轴标签
  geom_violin() +                                              # 小提琴图
  geom_boxplot(width = 0.2, position = position_dodge(0.9)) +  # 叠加窄箱线图
  facet_wrap(~Gene, nrow = 1) +                                # 按 Gene 分面，一行排列
  theme_bw() +                                                 # 黑白主题
  theme(axis.text.x = element_text(angle = 45, hjust = 1))     # x 轴文字旋转 45 度

p1   # 在 RStudio 面板中显示

# ==================== 输出图片 ====================
pdf(file = outFile, width = 9, height = 5)   # 打开 PDF 设备，9×5 英寸
print(p1)                                     # 输出到 PDF（ggplot 对象必须 print）
dev.off()                                     # 关闭设备，保存文件

# 以下为原代码末尾的注释，无实际代码：
# 把数据转换成ggplot2文件
# 差异分析
# 绘制
# 输出