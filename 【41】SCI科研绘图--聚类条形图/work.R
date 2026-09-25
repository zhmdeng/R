# ========== 加载所需的包 ==========
library(tidyverse)       # 数据处理和绘图
library(RColorBrewer)    # 提供配色方案（如 Paired）
library(ggtree)          # 系统发育/聚类树可视化
library(aplot)           # 用于在图形边缘插入子图
library(grid)            # 图形底层工具
library(ggplotify)       # 将非 ggplot 图形转换为 ggplot 对象
library(cowplot)         # 图形组合工具
library(jjAnno)          # 提供 annoRect() 等注释功能

# ========== 定义 y 轴文字颜色 ==========
# 生成 39 个颜色：13 个橙色、13 个蓝色、13 个棕色，再反转
ycols <- rev(rep(c("#EDB749","#3CB2EC","#9C8D58"), time = c(13,13,13)))

# ========== 绘制堆叠柱状图 p1 ==========
p1 <- read_tsv("data.xls") %>%                                    # 读取数据
  ggplot(aes(Abundance, id, fill = Phylum), color = "black") +    # x=丰度，y=id，填充=门类
  geom_bar(stat = "identity", position = "fill") +                # 堆叠柱状图，归一化为 100%
  labs(x = NULL, y = NULL) +                                      # 无轴标签
  theme_test() +                                                  # 简洁主题
  theme(
    axis.title.x = element_blank(),                               # x 轴标题不显示
    axis.ticks.y = element_blank(),                               # y 轴刻度不显示
    axis.ticks.x = element_blank(),                               # x 轴刻度不显示
    axis.text.y = element_text(face = "bold", size = 10, color = ycols),  # y 轴文字用自定义颜色
    axis.text.x = element_text(color = "black", size = 10),       # x 轴文字黑色
    legend.title = element_blank(),                               # 图例标题不显示
    legend.text = element_text(color = c(rep("#3CB2EC",4), rep("#EDB749",8))),  # 图例文字颜色
    legend.spacing.x = unit(0.2, 'cm'),                           # 图例水平间距
    legend.key = element_blank(),                                 # 图例键背景透明
    legend.key.width = unit(0.5, 'cm'),                           # 图例键宽度
    legend.key.height = unit(0.5, 'cm'),                          # 图例键高度
    plot.margin = margin(1, 0.5, 0.5, 1, unit = "cm")             # 图形边距
  ) +
  scale_fill_brewer(palette = "Paired") +                         # 使用 Paired 配色
  scale_x_continuous(expand = c(0, 0)) +                          # x 轴不留空白
  coord_cartesian(clip = 'off')                                   # 关闭坐标轴裁剪

# ========== 在柱状图左侧添加分组注释框 p2 ==========
p2 <- annoRect(object = p1,                                        # 基于 p1 添加注释
               annoPos = 'left',                                   # 注释位置在左侧
               aesGroup = T,                                       # 按分组自动划分
               aesGroName = 'Group',                               # 分组变量名
               xPosition = c(-0.25, -0.005),                       # x 轴位置范围
               pFill = rev(c("#FCFAD9","#FFF2E7","#E8F2FC",        # 填充色（反转）
                             "#BDE7FF","#EEECE1","#DDD9C3")),
               pCol = rev(c("#FCFAD9","#FFF2E7","#E8F2FC",         # 边框色（反转）
                            "#BDE7FF","#EEECE1","#DDD9C3")),
               rectWidth = 1)                                      # 矩形宽度

# ========== 准备聚类数据 ==========
p <- read_tsv("data.xls") %>%                                     # 读取数据
  select(-Group) %>%                                              # 删除 Group 列
  filter(Phylum != "Other") %>%                                   # 排除 "Other" 门类
  pivot_wider(names_from = "Phylum", values_from = "Abundance") %>%  # 宽表：行=id，列=门类
  column_to_rownames(var = "id")                                  # id 列转为行名

# ========== 提取分组信息 ==========
group <- read_tsv("data.xls") %>% select(id, Group)               # 只保留 id 和 Group 两列

# ========== 构建聚类树 ==========
hr <- hclust(dist(p)) %>%                                         # 对样本做层次聚类
  ggtree(layout = "rectangular", branch.length = "none") %<+% group +  # 绘制矩形聚类树，并关联分组信息
  geom_tippoint(aes(fill = Group, color = Group), shape = 21, size = 4) +  # 叶节点添加散点
  theme(
    legend.title = element_blank(),                               # 图例标题不显示
    legend.key = element_blank(),                                 # 图例键背景透明
    legend.text = element_text(color = c(rep("#EDB749", 3)))      # 图例文字颜色
  ) +
  scale_fill_manual(values = c("#BDE7FF", "#EEECE1", "#DDD9C3")) +   # 填充色
  scale_color_manual(values = c("#BDE7FF", "#EEECE1", "#DDD9C3"))    # 边框色

# ========== 组合图形 ==========
p2 %>% 
  insert_left(hr, width = .5) %>%    # 在 p2 左侧插入聚类树，宽度占 0.5
  as.grob() %>%                       # 转换为 grob 对象
  ggdraw()                            # 用 cowplot 绘制

# ========== 保存图片 ==========
ggsave("heatmap.pdf", width = 6, height = 6, dpi = 300)   # 保存为 PDF，6×6 英寸，300 dpi