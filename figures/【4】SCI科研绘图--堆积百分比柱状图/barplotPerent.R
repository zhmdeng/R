
# 
# inputFile="input.txt"       #输入
# outFile="barplot.pdf"       #输出
# # setwd("D:\\biowolf\\bioR\\04.barplotPerent")    #工作目录
# 
# #读取文件，整理
# data=read.table(inputFile,sep="\t",header=T,row.names=1,check.names=F)
# data=t(data)  
# col=rainbow(nrow(data),s=0.7,v=0.7)
# 
# #绘制
# pdf(outFile,height=10,width=20)
# par(las=1,mar=c(8,5,4,16),mgp=c(3,0.1,0),cex.axis=0.8)        #cex.axis设置轴文字大小
# a1=barplot(data,col=col,yaxt="n",ylab="Relative Percent",xaxt="n",cex.lab=0.5)
# a2=axis(2,tick=F,labels=F)
# axis(2,a2,paste0(a2*100,"%"))
# axis(1,a1,labels=F)
# par(srt=60,xpd=T);text(a1,-0.02,colnames(data),adj=1,cex=0.3);par(srt=0)
# ytick2 = cumsum(data[,ncol(data)])
# ytick1 = c(0,ytick2[-length(ytick2)])
# legend(par('usr')[2]*0.98,par('usr')[4],legend=rownames(data),col=col,pch=15,bty="n",cex=1.5)
# dev.off()


# 输入文件：包含分类群丰度数据的文本文件（制表符分隔）
inputFile = "input.txt"

# 输出文件：生成的柱状图 PDF 文件名
outFile = "barplot.pdf"

# setwd(...)  # 设置工作目录（已注释掉，根据实际情况启用）

# ========== 读取与整理数据 ==========
# 读取输入文件，sep="\t" 表示制表符分隔，header=T 表示第一行为列名，row.names=1 表示第一列为行名
data = read.table(inputFile, sep = "\t", header = T, row.names = 1, check.names = F)

# 转置数据：原来行是分类群、列是样本，转置后行是样本、列是分类群
data = t(data)

# 生成颜色：rainbow() 生成彩虹色，nrow(data) 是样本数，s=0.7 是饱和度，v=0.7 是亮度
col = rainbow(nrow(data), s = 0.7, v = 0.7)

# ========== 绘图 ==========
# 打开 PDF 设备：高度 10 英寸，宽度 20 英寸
pdf(outFile, height = 10, width = 20)

# 设置图形参数：
# las=1：y 轴标签水平显示
# mar=c(8,5,4,16)：边距（下, 左, 上, 右），右边距 16 留出空间给图例
# mgp=c(3,0.1,0)：轴标签、轴刻度、轴线的位置
# cex.axis=0.8：轴文字大小为默认的 0.8 倍
par(las = 1, mar = c(8, 5, 4, 16), mgp = c(3, 0.1, 0), cex.axis = 0.8)

# 绘制堆叠柱状图：
# data：数据矩阵
# col：颜色
# yaxt="n"：不画 y 轴（后面手动加）
# ylab="Relative Percent"：y 轴标签
# xaxt="n"：不画 x 轴（后面手动加）
# cex.lab=0.5：轴标签文字大小
a1 = barplot(data, col = col, yaxt = "n", ylab = "Relative Percent", xaxt = "n", cex.lab = 0.5)

# 手动添加 y 轴刻度：tick=F 表示不画刻度线，labels=F 表示不显示标签
a2 = axis(2, tick = F, labels = F)

# 在 y 轴位置 a2 处添加百分比标签（a2*100 后加 "%"）
axis(2, a2, paste0(a2 * 100, "%"))

# 手动添加 x 轴刻度：a1 是柱子位置，labels=F 表示不显示标签（后面用 text 添加）
axis(1, a1, labels = F)

# 设置文字旋转角度为 60 度，xpd=T 允许文字超出绘图区域
par(srt = 60, xpd = T)

# 在柱子下方添加样本名：
# a1 是柱子位置，-0.02 是 y 坐标（柱子下方）
# colnames(data) 是样本名，adj=1 表示右对齐，cex=0.3 是文字大小
text(a1, -0.02, colnames(data), adj = 1, cex = 0.3)

# 恢复文字旋转角度为 0（水平）
par(srt = 0)

# ========== 添加图例 ==========
# ytick2：每列数据的累积和（用于确定图例的垂直位置）
ytick2 = cumsum(data[, ncol(data)])

# ytick1：累积和的偏移一位（从 0 开始），用于图例的垂直位置
ytick1 = c(0, ytick2[-length(ytick2)])

# 添加图例：
# par('usr')[2]*0.98：x 坐标（绘图区右侧 98% 处）
# par('usr')[4]：y 坐标（绘图区顶部）
# legend=rownames(data)：图例标签（分类群名称）
# col=col：颜色
# pch=15：方块符号
# bty="n"：无边框
# cex=1.5：图例文字大小
legend(par('usr')[2] * 0.98, par('usr')[4], legend = rownames(data), col = col, pch = 15, bty = "n", cex = 1.5)

# 关闭 PDF 设备，保存文件
dev.off()
