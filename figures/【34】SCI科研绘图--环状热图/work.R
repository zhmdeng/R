# 定义要加载的包列表
package.list = c("tidyverse", "ggtreeExtra", "ggtree", "treeio", 
                 "ggnewscale", "patchwork", "ComplexHeatmap")

# 手动加载所有包（但这样写如果包未安装会直接报错，不会自动安装）
library("tidyverse")       # 数据处理与绘图核心
library("ggtreeExtra")     # 为 ggtree 添加热图等图层
library("ggtree")          # 绘制树图
library("treeio")          # 树数据导入导出
library("ggnewscale")      # 允许同一张图使用多套 fill/color 比例尺
library("patchwork")       # 图形组合
library("ComplexHeatmap")  # 复杂热图与图例绘制

# 下面的循环试图自动安装缺失的包，但 install.packages 对 Bioconductor 包无效
# （ggtree、treeio、ComplexHeatmap 都需要 BiocManager 安装）
for (package in package.list) {
  if (!require(package, character.only = T, quietly = T)) {
    install.packages(package)                 # ❌ 对 Bioconductor 包无效
    library(package, character.only = T)
  }
}

# 读取 data.xls，并转换为长格式
df <- read_tsv("data.xls") %>%
  pivot_longer(-gene) %>%                     # 除 gene 外全部转成长表
  mutate(group = case_when(
    value == 0 ~ "not regulated",             # 值为 0 表示未调控
    name == "CNA" & value == 1 ~ "CNA (direct)",
    name == "mir" & value == 1 ~ "miRNA (inverse)",
    name == "methylationpromoter" & value == 1 ~ "methylation (direct)",
    name == "methylationgenebody" & value == 1 ~ "methylation (direct)",
    name == "methylationanywhere" & value == 1 ~ "methylation (direct)",
    name == "methylationpromoter" | name == "methylationgenebody" |
      name == "methylationanywhere" | value == -1 ~ "methylation (inverse)"
  ))

class(df)   # 查看 df 类型，应为 "tbl_df" 等，而不是 "function"
# 如果这里返回 "function"，说明 df 赋值失败（前面某步报错导致）

# 设置 group 的因子水平，控制图例顺序
df$group <- factor(df$group,
                   levels = c("CNA (direct)", "miRNA (inverse)",
                              "methylation (inverse)", "methylation (direct)",
                              "not regulated"))

# 设置 name 的因子水平（逆序），决定热图 y 轴顺序
df$name <- factor(df$name,
                  levels = rev(c("methylationgenebody", "methylationanywhere",
                                 "methylationpromoter", "mir", "CNA")))

# ==================== 绘制第一个环形树 + 热图 ====================
g1 <- hclust(dist(read_tsv("data.xls") %>% column_to_rownames(var = "gene"))) %>%
  # 对 gene 进行层次聚类，gene 列转为行名
  ggtree(layout = "fan", open.angle = 0, size = 0.3) +   # 环形布局
  # rotate_tree(., angle = 0) +   # ❌ rotate_tree 函数不存在，应删除
  geom_tiplab(size = 3, family = "Times", color = "black", offset = 0.53) +
  # 添加叶节点标签
  new_scale_fill() +             # 开启新的 fill 比例尺（为 geom_fruit 准备）
  geom_fruit(data = df, geom = geom_tile,
             mapping = aes(y = gene, x = name, fill = group),
             color = "grey50", offset = 0.04, size = 0.02) +
  # 在树的外围添加热图，y 对应基因，x 对应 name，填充对应 group
  scale_fill_manual(values = c("#5686C3","#973CB6","#F5A300","#75C500","#D9D9D9")) +
  # 自定义填充色
  theme(legend.position = "none")   # 隐藏图例（"none" 正确，"non" 错误）

# ==================== 绘制第二个环形树 + 热图 ====================
p2 <- read_tsv("data2.xls") %>%
  pivot_longer(-gene) %>%
  mutate(group = case_when(
    value == 0 ~ "not regulated",
    name == "CNA" & value == 1 ~ "CNA (direct)",
    name == "mir" & value == 1 ~ "miRNA (inverse)",
    name == "methprom" & value == 1 ~ "methylation (direct)",
    name == "methbody" & value == 1 ~ "methylation (direct)",
    name == "methanywhere" & value == 1 ~ "methylation (direct)",
    name == "methprom" | name == "methbody" | name == "methanywhere" |
      value == -1 ~ "methylation (inverse)"
  ))

# 设置 p2 的 group 因子水平
p2$group <- factor(p2$group,
                   levels = c("CNA (direct)", "miRNA (inverse)",
                              "methylation (inverse)", "methylation (direct)",
                              "not regulated"))

# 设置 p2 的 name 因子水平（逆序）
p2$name <- factor(p2$name,
                  levels = rev(c("methbody", "methanywhere", "methprom",
                                 "mir", "CNA")))

g2 <- hclust(dist(read_tsv("data2.xls") %>% column_to_rownames(var = "gene"))) %>%
  ggtree(layout = "fan", open.angle = 0, size = 0.3) %>%
  # rotate_tree(., angle = 0) +   # ❌ 同样不存在，删除
  geom_tiplab(size = 3, family = "Times", color = "black", offset = 0.6) +
  new_scale_fill() +
  geom_fruit(data = p2, geom = geom_tile,
             mapping = aes(y = gene, x = name, fill = group),
             color = "grey50", offset = 0.04, size = 0.02) +
  scale_fill_manual(values = c("#5686C3","#973CB6","#F5A300","#75C500","#D9D9D9")) +
  theme(legend.position = "none")   # 同样改为 "none"

# 组合两个图
g1 + g2

# ==================== 添加图例 ====================
# 使用 ComplexHeatmap 的 Legend 创建图例对象
lgd = Legend(labels = c("CNA (direct)", "miRNA (inverse)",
                        "methylation (inverse)", "methylation (direct)",
                        "not regulated"),
             legend_gp = gpar(fill = c("#5686C3","#973CB6","#F5A300",
                                       "#75C500","#D9D9D9")),
             labels_gp = gpar(col = "black", fontsize = 10),
             grid_width = unit(7, "mm"),
             grid_height = unit(3, "mm"))

# 在当前图形设备上绘制图例（位置为相对坐标）
draw(lgd, x = unit(0.55, "npc"), y = unit(0.85, "npc"),
     just = c("right", "top"))

ggsave("图.pdf",width = 6,height = 6,dpi = 300)
