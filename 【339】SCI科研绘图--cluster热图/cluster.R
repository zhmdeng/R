library(ClassDiscovery)  # 加载 ClassDiscovery 包，用于外部聚类（提供 distanceMatrix 等函数）
library(pheatmap)        # 加载 pheatmap 包，用于绘制热图
library(gplots)          # 加载 gplots 包，提供 bluered() 等颜色函数

Sys.setenv(LANGUAGE = "en")          # 设置 R 报错信息为英文
options(stringsAsFactors = FALSE)    # 禁止字符型自动转换为因子

# 读取表达矩阵，第一列作为行名，不检查列名合法性
mat <- read.csv("easy_input_expr.csv", row.names = 1, check.names = F, stringsAsFactors = F)
mat[1:3, 1:3]                        # 查看前 3 行 3 列，检查数据

# 读取样本注释信息，第一列作为行名
annCol <- read.csv("easy_input_annotation.csv", row.names = 1, check.names = F, stringsAsFactors = F)
head(annCol)                         # 查看注释信息的前几行

# 将注释中的缺失值或空字符串替换为 "N/A"
annCol[is.na(annCol) | annCol == ""] <- "N/A"

# 为注释的每一列定义颜色映射，用于热图上方的注释条
annColors <- list()                  # 创建空列表存放颜色
annColors[["gender"]] <- c("MALE" = "blue", "FEMALE" = "red", "N/A" = "white")  # 性别颜色
annColors[["vital_status"]] <- c("Alive" = "yellow", "Dead" = "black", "N/A" = "white")  # 生存状态颜色
annColors[["colon_polyps_present"]] <- c("YES" = "red", "NO" = "black", "N/A" = "white")  # 结肠息肉颜色
annColors                            # 查看颜色列表

# 绘制原始热图（未进行样本聚类）
pheatmap(mat,
         scale = "row",              # 按行标准化（Z-score）
         color = bluered(64),        # 使用蓝-白-红渐变（64 个色阶）
         annotation_col = annCol,    # 列注释
         annotation_colors = annColors,  # 注释颜色
         show_rownames = T,          # 显示行名（基因名）
         show_colnames = F,          # 不显示列名（样本名）
         filename = "raw_heatmap.pdf")   # 保存为 PDF

# 对行（基因）进行层次聚类，距离度量为 Pearson 相关，连接方法为 ward.D
hcs <- hclust(distanceMatrix(as.matrix(mat), "pearson"), "ward.D")

# 对列（样本）进行层次聚类，需要对矩阵转置，因为 distanceMatrix 是针对列的
hcg <- hclust(distanceMatrix(t(as.matrix(mat)), "pearson"), "ward.D")

# 将行聚类树切割为 2 类，得到每个基因的类别
group <- cutree(hcs, k = 2)

# 在注释数据框中新增一列 Clust2，标记每个样本所属的聚类（基于行聚类？这里需要注意：hcs 是对行聚类，但 group 的 names 是行名，即基因名；而 annCol 的行名是样本名。此处可能存在逻辑混淆）
annCol$Clust2 <- paste0("C", group[rownames(annCol)])  # 按样本名从 group 中提取类别（但 group 的 names 是基因名，所以这里可能会得到 NA 或错误）
annColors[["Clust2"]] <- c("C1" = "red", "C2" = "green")  # 为新注释列定义颜色
head(annCol)                         # 查看更新后的注释

# 绘制带外部聚类结果的热图
pheatmap(mat,
         scale = "row",
         color = bluered(64),
         cluster_rows = hcg,         # 行聚类使用 hcg（这里 hcg 是列聚类结果，变量名可能反了）
         cluster_cols = hcs,         # 列聚类使用 hcs（同样，变量名可能反了）
         annotation_col = annCol,
         annotation_colors = annColors,
         show_rownames = T, show_colnames = F,
         filename = "heatmap_with_outside_Cluster.pdf")

# 将聚类后的样本顺序保存到文件
sample_order <- data.frame(row.names = seq(1:length(hcs$labels)),  # 创建数据框，行号为 1 到样本数
                           sample = hcs$labels,                   # 样本名（来自 hcs$labels）
                           group = group)                         # 分组
sample_order <- sample_order[hcs$order, ]        # 按聚类后的顺序排序
sample_order$ori.order <- row.names(sample_order)  # 保存原始顺序
write.csv(sample_order, "sample_order.csv", quote = F, row.names = F)  # 写出文件

# 绘制热图，但不显示列树结构（treeheight_col = 0）
pheatmap(mat,
         scale = "row",
         color = bluered(64),
         cluster_rows = hcg,
         cluster_cols = hcs,
         treeheight_col = 0,         # 列树高度设为 0，不显示树
         annotation_col = annCol,
         annotation_colors = annColors,
         show_rownames = T, show_colnames = F,
         filename = "heatmap_with_outside_Cluster_noTree.pdf")

# 获取列聚类后的样本顺序索引
index <- order.dendrogram(as.dendrogram(hcs))
sam_order <- colnames(mat)[index]   # 按该顺序排列样本名

# 绘制热图，手动指定样本顺序，不进行列聚类
pheatmap(mat[, sam_order],          # 输入矩阵按 sam_order 重排
         scale = "row",
         color = bluered(64),
         cluster_cols = F,          # 不进行列聚类
         cluster_rows = hcg,
         # treeheight_col = 0,      # 这行已无效
         annotation_col = annCol[sam_order, ],  # 注释文件也按相同顺序排列
         annotation_colors = annColors,
         show_rownames = T, show_colnames = F,
         filename = "heatmap_with_outside_Cluster_discardTree.pdf")

# 将行聚类树切割为 3 类
group <- cutree(hcs, k = 3)

# 在注释数据框中新增一列 Clust3
annCol$Clust3 <- paste0("C", group[rownames(annCol)])  # 同样，从 group 中按样本名提取（注意 group 的 names 是基因名）
annColors[["Clust3"]] <- c("C1" = "red", "C2" = "green", "C3" = "blue")  # 颜色映射
head(annCol)

# 绘制热图，使用 3 类聚类结果
pheatmap(mat[, sam_order],
         scale = "row",
         color = bluered(64),
         cluster_cols = F,
         cluster_rows = hcg,
         annotation_col = annCol[sam_order, ],
         annotation_colors = annColors,
         show_rownames = T, show_colnames = F,
         filename = "heatmap_with_outside_Cluster_discardTree_twoClusters.pdf")

# 使用欧几里得距离重新对样本聚类
hcs2 <- hclust(distanceMatrix(as.matrix(mat), "euclidean"), "ward.D")
group <- cutree(hcs2, k = 3)        # 切割为 3 类

# 获取新聚类下的样本顺序
index <- order.dendrogram(as.dendrogram(hcs2))
sam_order <- colnames(mat)[index]

# 将新顺序保存到文件
write.csv(sam_order, "sample_order_euclidean.csv", quote = F)

# 新增注释列 Clust3.2
annCol$Clust3.2 <- paste0("C", group[rownames(annCol)])
annColors[["Clust3.2"]] <- c("C1" = "red", "C2" = "green", "C3" = "blue")
head(annCol)

# 绘制热图，使用新的样本顺序和聚类结果
pheatmap(mat[, sam_order],
         scale = "row",
         color = bluered(64),
         cluster_cols = F,
         cluster_rows = hcg,
         annotation_col = annCol[sam_order, ],
         annotation_colors = annColors,
         show_rownames = T, show_colnames = F,
         filename = "heatmap_with_outside_Cluster_discardTree_twoClusters2.pdf")

# 读取之前保存的样本顺序（来自 hcs 聚类）
sam_order <- read.csv("sample_order.csv", header = T)
sam_order <- sam_order$sample

head(annCol)                        # 查看当前注释信息

# 如果不想画某列注释，可以将其设为 NULL，例如：
# annCol$Clust2 <- NULL

# 绘制最终热图，使用之前保存的样本顺序
pheatmap(mat[, sam_order],
         scale = "row",
         color = bluered(64),
         cluster_cols = F,
         cluster_rows = hcg,
         # treeheight_col = 0,      # 无效
         annotation_col = annCol[sam_order, ],
         annotation_colors = annColors,
         show_rownames = T, show_colnames = F,
         filename = "图.pdf")