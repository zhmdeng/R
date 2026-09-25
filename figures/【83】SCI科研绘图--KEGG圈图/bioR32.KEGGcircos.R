
library(clusterProfiler)   # 富集分析核心包
library(org.Hs.eg.db)      # 人类基因注释数据库
library(enrichplot)        # 富集结果可视化
library(ggplot2)           # 绘图包
library(GOplot)            # 绘制圈图、聚类圈图

# 设置过滤阈值
pFilter = 0.05             # p 值阈值
adjPfilter = 0.05          # 校正后 p 值阈值

# setwd("")                # 设置工作目录（已注释）

# 读取输入文件
rt = read.table("input.txt", sep = "\t", header = T, check.names = F)
# input.txt 至少包含 gene（基因名）和 logFC 两列，第一列通常是基因名

genes = as.vector(rt[, 1])   # 提取基因列表（第一列）

entrezIDs = mget(genes, org.Hs.egSYMBOL2EG, ifnotfound = NA)
# 将 gene symbol 转换为 Entrez ID，返回一个列表，每个元素可能包含多个 ID

entrezIDs = as.character(entrezIDs)
# 将列表强制转为字符向量（可能有问题，如果某个基因有多个 ID，会变成 "c(\"id1\", \"id2\")"）
# 更稳妥的做法是：entrezIDs <- unlist(entrezIDs) 或 sapply(entrezIDs, `[`, 1)

rt = cbind(rt, entrezID = entrezIDs)   # 将 Entrez ID 添加到数据框
rt = rt[is.na(rt[, "entrezID"]) == F, ]  # 去除 Entrez ID 为 NA 的行
gene = rt$entrezID                        # 提取最终的 Entrez ID 向量

# ==================== KEGG 富集分析 ====================
kk = enrichKEGG(gene = gene, organism = "hsa", pvalueCutoff = 1, qvalueCutoff = 1)
# 进行 KEGG 富集分析，这里故意将阈值设为 1 以获取全部结果，后续手动过滤

KEGG = as.data.frame(kk)   # 转为数据框
KEGG = KEGG[(KEGG$pvalue < pFilter & KEGG$p.adjust < adjPfilter), ]
# 按 p 值和校正后 p 值过滤

KEGG$geneID = as.character(sapply(KEGG$geneID, function(x) 
  paste(rt$gene[match(strsplit(x, "/")[[1]], as.character(rt$entrezID))], collapse = "/")))
# 将 KEGG 结果中的 geneID（Entrez ID，用 "/" 分隔）替换为对应的 gene symbol，用 "/" 连接

write.table(KEGG, file = "KEGG.txt", sep = "\t", quote = F, row.names = F)
# 保存富集结果

# 获取 KEGG 信息，构建 GOplot 需要的格式
kegg = data.frame(Category = "ALL", ID = KEGG$ID, Term = KEGG$Description,
                  Genes = gsub("/", ", ", KEGG$geneID), adj_pval = KEGG$p.adjust)
# Category 固定为 "ALL"，Term 为通路描述，Genes 用逗号分隔，adj_pval 为校正 p 值

# 读取基因的 logFC
genelist = data.frame(ID = rt$gene, logFC = rt$logFC)
# 构建基因和 logFC 的数据框

row.names(genelist) = genelist[, 1]   # 将基因名设为行名

circ = circle_dat(kegg, genelist)      # 将 KEGG 结果和基因 logFC 整合为 circ 对象

termNum = 5                            # 限定显示的 Term 数量
termNum = ifelse(nrow(kegg) < termNum, nrow(kegg), termNum)  # 若通路数少于 5，则取实际数量

geneNum = nrow(genelist)               # 基因数量

# ==================== 绘制圈图 ====================
chord = chord_dat(circ, genelist[1:geneNum, ], kegg$Term[1:termNum])
# 提取指定 Term 和基因的数据，生成 chord 对象

pdf(file = "图.pdf", width = 11, height = 11.2)   # 打开 PDF 设备
GOChord(chord, 
        space = 0.001,           # 基因之间的间距
        gene.order = 'logFC',    # 按照 logFC 对基因排序
        gene.space = 0.25,       # 基因名与圆圈的相对距离
        gene.size = 4,           # 基因名字体大小
        border.size = 0.1,       # 线条粗细
        process.label = 7)       # Term 字体大小
dev.off()                        # 关闭 PDF 设备

# ==================== 绘制聚类圈图 ====================
pdf(file = "cluster.pdf", width = 12, height = 9)   # 打开 PDF 设备
GOCluster(circ, as.character(kegg[1:termNum, 3]))   # 绘制聚类圈图，传入 circ 对象和 Term 名称
dev.off()                                            # 关闭 PDF 设备

