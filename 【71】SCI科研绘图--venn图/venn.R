# install.packages("VennDiagram")   # 安装 VennDiagram 包（已注释）

# ==================== 加载包 ====================
library(VennDiagram)   # 引用 VennDiagram 包，用于绘制韦恩图

# ==================== 定义输入输出文件 ====================
outFile = "intersectGenes.txt"   # 输出文件：保存交集基因
outPic = "venn图.pdf"            # 输出文件：韦恩图 PDF

# setwd("")   # 设置工作目录（已注释）

# ==================== 获取目录下的 txt 文件 ====================
files = dir()                              # 获取当前目录下所有文件名
files = grep("txt$", files, value = T)     # 筛选以 txt 结尾的文件
# grep() 模式匹配，"txt$" 表示以 txt 结尾
# value = T 返回匹配的文件名（而不是位置索引）

geneList = list()   # 创建空列表，用于存放每个文件的基因列表

# ==================== 读取所有 txt 文件中的基因 ====================
for (i in 1:length(files)) {   # 遍历每个 txt 文件
  inputFile = files[i]        # 当前文件名
  if (inputFile == outFile) { next }   # 跳过输出文件本身（避免把结果当输入）
  rt = read.table(inputFile, header = F)   # 读取文件，无表头
  geneNames = as.vector(rt[, 1])           # 提取第一列基因名
  geneNames = gsub("^ | $", "", geneNames) # 去掉基因名首尾的空格
  # gsub() 全局替换，"^ " 匹配开头空格，" $" 匹配结尾空格
  uniqGene = unique(geneNames)             # 基因去重
  header = unlist(strsplit(inputFile, "\\.|\\-"))   # 用 "." 或 "-" 拆分文件名
  # strsplit() 拆分字符串
  # unlist() 把列表转为向量
  geneList[[header[1]]] = uniqGene         # 以文件名第一部分作为列表名，存入基因
  uniqLength = length(uniqGene)            # 该文件的基因数量
  print(paste(header[1], uniqLength, sep = " "))   # 打印文件名和基因数
}
# 例如文件名为 "group1-genes.txt"，header[1] 就是 "group1"

# ==================== 绘制韦恩图 ====================
venn.plot = venn.diagram(
  geneList,                              # 基因列表（每个元素是一个基因向量）
  filename = NULL,                       # 不直接输出文件，返回图形对象
  fill = rainbow(length(geneList))       # 每个集合用不同颜色填充
)
# venn.diagram() 绘制韦恩图
# filename = NULL 表示不保存文件，而是返回一个图形对象
# rainbow(n) 生成 n 种彩虹色

# ==================== 输出 PDF ====================
pdf(file = outPic, width = 5, height = 5)   # 打开 PDF 设备，5×5 英寸
grid.draw(venn.plot)                         # 绘制韦恩图对象
dev.off()                                    # 关闭设备，保存文件

# ==================== 保存交集基因 ====================
intersectGenes = Reduce(intersect, geneList)   # 求所有集合的交集
# Reduce() 对列表中的元素依次应用函数
# intersect() 求两个向量的交集
# 结果：所有文件共有的基因

write.table(file = outFile,          # 输出文件名
            intersectGenes,           # 要写入的数据
            sep = "\t",               # 制表符分隔
            quote = F,                # 不加引号
            col.names = F,            # 不写列名
            row.names = F)            # 不写行名