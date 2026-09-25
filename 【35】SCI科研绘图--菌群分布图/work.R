# ========== 加载所需的包 ==========
library(tidyverse)   # 数据处理和绘图（含 ggplot2、dplyr、readr 等）
library(ggh4x)       # 提供 facet_nested() 等扩展功能，用于嵌套分面

# ========== 设置工作目录 ==========
setwd("/Users/mac/Desktop/PowerBI/sci科研绘图/【35】SCI科研绘图--菌群分布图/")

# ========== 读取数据 ==========
sub_merge <- read_tsv("data.txt")   # 读取制表符分隔的数据文件

# ========== 设置 Compartments 的因子顺序 ==========
# 将 Compartments 列转为因子，指定 7 个水平的顺序，并赋予相同的标签
sub_merge$Compartments <- factor(sub_merge$Compartments,
                                 levels = c("BS","RS","RE","VE","SE","LE","P"),
                                 labels = c("BS","RS","RE","VE","SE","LE","P"))

# ========== 设置 Phylum 的因子顺序 ==========
# 将 Phylum 列转为因子，指定 14 个门类的顺序，并赋予相同的标签
# 这样画图时图例和堆叠顺序就按照这个顺序排列
sub_merge$Phylum <- factor(sub_merge$Phylum,
                           levels = c("Abditibacteriota", "Acidobacteriota",
                                      "Actinobacteriota", "Alphaproteobacteria",
                                      "Bacteroidota", "Chloroflexi", "Deinococcota",
                                      "Firmicutes", "Gammaproteobacteria", "Gemmatimonadota",
                                      "Myxococcota", "Nitrospirota", "unclassified", "Others"),
                           labels = c("Abditibacteriota", "Acidobacteriota", "Actinobacteriota",
                                      "Alphaproteobacteria", "Bacteroidota", "Chloroflexi",
                                      "Deinococcota", "Firmicutes", "Gammaproteobacteria",
                                      "Gemmatimonadota", "Myxococcota", "Nitrospirota",
                                      "unclassified", "Others"))

# ========== 定义门类颜色 ==========
# 为 14 个门类分别指定颜色
phy.cols <- c("#FF6A6A","#FF8247","#FFE7BA","#87CEFA","#B0E0E6","#48D1CC","#5F9EA0","#66CDAA",
              "#458B00","#BCEE68","#FFF68F","#EEEE00","#FFFFE0","#8B8682")

# ========== 绘制菌群分布图 ==========
ggplot(sub_merge, aes(x = TreatmentID, y = `Relative abundance (%)`, fill = Phylum)) +
  # x 轴为 TreatmentID，y 轴为相对丰度，填充色按 Phylum 映射
  geom_bar(stat = 'identity', position = "fill") +
  # 绘制堆叠柱状图，position="fill" 让每根柱子高度归一化为 1（即 100%）
  labs(x = "Treatment", y = "Relative abundance") +
  # 设置轴标签
  facet_nested(Soiltype + Site ~ Compartments, drop = T, scale = "free", space = "free") +
  # 嵌套分面：行方向为 Soiltype + Site（两层嵌套），列方向为 Compartments
  # drop=T 删除空分面，scale="free" 各分面轴范围独立，space="free" 各分面宽度按数据量调整
  scale_y_continuous(expand = c(0,0), labels = scales::percent) +
  # y 轴不留空白，标签用百分比格式显示
  theme_bw() +
  # 使用黑白主题
  theme(
    axis.text.x = element_blank(),      # x 轴文字不显示
    axis.ticks.x = element_blank(),     # x 轴刻度线不显示
    axis.text.y = element_text(size = 8, color = "black"),      # y 轴文字：8号、黑色
    axis.title.y = element_text(size = 12, color = "black"),    # y 轴标题：12号、黑色
    axis.title.x = element_text(size = 12),                     # x 轴标题：12号
    legend.title = element_text(size = 12),                     # 图例标题：12号
    legend.text = element_text(size = 10),                      # 图例文字：10号
    legend.position = "bottom",                                 # 图例放在底部
    panel.spacing.x = unit(0, "lines"),                         # 分面之间水平间距为 0
    panel.spacing.y = unit(0.5, "lines")                        # 分面之间垂直间距为 0.5 行
  ) +
  scale_fill_manual(values = phy.cols)
# 手动指定填充颜色为 phy.cols

# ========== 保存图片 ==========
ggsave("biomicro.pdf", width = 6, height = 6)   # 保存为 PDF，6×6 英寸