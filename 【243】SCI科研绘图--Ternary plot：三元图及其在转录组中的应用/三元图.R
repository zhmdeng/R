# setwd("")   # 设置工作目录

# ============================================================
# 第一部分：单细胞转录组三元图
# ============================================================

# 如果没有 devtools，需要先安装
# install.packages("devtools")           # 安装 devtools
devtools::install_github("jinming-cheng/scTernary")   # 从 GitHub 安装 scTernary 包

library(scTernary)    # 加载 scTernary，提供三元图函数
library(Matrix)       # 加载 Matrix，支持稀疏矩阵

# ==================== 加载数据 ====================
data_exp_mat <- example_seu@assays$RNA@counts
# 从 Seurat 对象 example_seu 中提取 RNA assay 的 counts 矩阵（稀疏矩阵）

data_exp_mat <- as.matrix(data_exp_mat)
# 转为普通矩阵（后续处理需要）

dim(data_exp_mat)
# 查看矩阵维度，输出 13750 × 80（13750 个基因，80 个细胞）

# ==================== 加载 signature 数据 ====================
load("data/anno_signature_genes_mouse.rda")
# 加载小鼠的基因注释数据，包含基因 ID、Symbol、染色体、gene_type 等信息

head(anno_signature_genes_mouse)
# 查看前几行，gene_type 列标识基因属于 Basal 等类别

# ==================== 准备三元图数据 ====================
data_for_ternary <- generate_data_for_ternary(
  data_exp_mat = data_exp_mat,             # 表达矩阵
  anno_signature_genes = anno_signature_genes_mouse,  # 基因注释数据
  gene_name_col = "Symbol",                # 基因名所在的列
  gene_type_col = "gene_type",             # 基因类别所在的列
  weight_by_gene_count = TRUE,             # 按基因数加权
  cutoff_exp = 0,                          # 表达量过滤阈值
  prior_count = 1                          # 先验计数（避免除零）
)

# ==================== 绘制三元图 ====================
data_for_ternary <- cbind(data_for_ternary, example_seu@meta.data)
# 把 Seurat 对象的元数据（如 cluster 信息）合并到三元图数据中

scTernary::vcdTernaryPlot(
  data_for_ternary,                        # 数据
  order_colnames = c(2, 3, 1),             # 三个顶点的列顺序
  point_size = 0.5,                        # 点大小
  group = data_for_ternary$seurat_clusters, # 按 cluster 分组着色
  show_legend = TRUE,                      # 显示图例
  scale_legend = 0.8,                      # 图例缩放比例
  legend_position = c(0.2, 0.5),           # 图例位置（相对坐标）
  legend_vertical_space = 1,               # 图例垂直间距
  legend_text_size = 1,                    # 图例文字大小
  facet = FALSE                            # 不分面
)

# ============================================================
# 第二部分：bulk 转录组三元图
# ============================================================

install.packages("ggtern")    # 安装 ggtern（已注释）
library(ggtern)               # 加载 ggtern，用于三元图
library(ggrastr)              # 提供 geom_point_rast()，栅格化散点
library(ggplot2)              # 绘图核心包

# ==================== 加载 bulk 数据 ====================
Bulk_FPKM <- read.csv('Bulk_FPKM.csv', header = T, row.names = 1)
# 读取 FPKM 表达矩阵，第一列作为行名（基因名）

Bulk_FPKM <- Bulk_FPKM[rowSums(Bulk_FPKM) > 0.1, ]
# 过滤掉在所有样本中总表达量低于 0.1 的基因

# ==================== 差异基因分析（已注释） ====================
# meta <- data.frame(E1 = c("E1.1","E1.2","E1.3"),
#                    E18 = c("E18.1","E18.2","E18.3"),
#                    E30 = c("E30.1","E30.2","E30.3"))
# deg <- KS_bulkRNA_MultiGroup_DEGs(exprSet = Bulk_FPKM, meta = meta,
#                                    methods = "limma", separator = ".")
# 这段代码是用 limma 做差异分析，但被注释了，改用下面的手动方法

# ==================== 计算平均表达量 ====================
avg_exp <- data.frame(
  E1  = matrixStats::rowMeans2(as.matrix(Bulk_FPKM[1:3])),   # 前 3 列均值（E1 组）
  E18 = matrixStats::rowMeans2(as.matrix(Bulk_FPKM[4:6])),   # 第 4-6 列均值（E18 组）
  E30 = matrixStats::rowMeans2(as.matrix(Bulk_FPKM[7:9]))    # 第 7-9 列均值（E30 组）
)
# rowMeans2() 来自 matrixStats，比 rowMeans() 更快

avg_exp$gene <- rownames(avg_exp)                      # 基因名作为一列
avg_exp$average <- rowMeans(avg_exp[, 1:3])            # 三个时期的平均表达量

# ==================== 标记基因类型 ====================
avg_exp$gene_type <- 'Other'                           # 默认所有基因为 Other
avg_exp$gene_type <- ifelse(
  avg_exp$gene %in% c("MEPCE","OR2S2","ANKZF1","LRRC61","SOHLH2"), "down",
  # 如果基因属于这 5 个，标记为 down
  ifelse(avg_exp$gene %in% c("GLO1","KIF18A","SNAP29","FHL3","HACD2","CFH"), "up",
         # 如果属于这 6 个，标记为 up
         avg_exp$gene_type)                            # 否则保持 Other
)

# ==================== 绘制三元图 ====================
ggtern(data = avg_exp, aes(x = E1, y = E18, z = E30)) +
  # ggtern 的三元图：x、y、z 是三个顶点的坐标（对应三个时期的表达量）
  
  geom_point_rast(data = avg_exp,                      # 背景散点（栅格化，避免文件过大）
                  aes(size = average),                 # 点大小按平均表达量
                  color = "grey80",                    # 灰色
                  alpha = 0.8,                         # 透明度
                  show.legend = FALSE) +               # 不显示图例
  
  geom_point(data = avg_exp[avg_exp$gene_type == 'up', ],   # 上调基因
             size = 3,                                     # 点大小 3
             color = '#ED645A',                            # 红色
             show.legend = F) +
  
  geom_point(data = avg_exp[avg_exp$gene_type == 'down', ], # 下调基因
             size = 3,
             color = "#4895ef",                            # 蓝色
             show.legend = F) +
  
  geom_text(data = avg_exp[avg_exp$gene_type == 'up', ],    # 上调基因的标签
            aes(label = gene),                              # 显示基因名
            color = 'black', size = 3,                      # 黑色，字号 3
            fontface = 'italic')                            # 斜体

# ==================== 保存 ====================
ggsave("图.pdf", width = 6, height = 6, dpi = 300)