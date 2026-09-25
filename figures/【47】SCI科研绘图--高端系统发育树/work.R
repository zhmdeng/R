# ==================== 加载包 ====================
library(tidyverse)      # 数据处理和绘图
library(ggtree)         # 树图绘制
library(ggtreeExtra)    # 在树图外围添加热图、柱状图等注释图层
library(ggnewscale)     # 允许同一张图使用多套 fill 比例尺
library(ggsci)          # 期刊配色（本段未直接使用）
library(cowplot)        # 图形组合

# ==================== 读取数据 ====================
df <- read_csv("data.csv") %>%          # 读取 CSV
  dplyr::rename(tax = `...1`) %>%       # 第一列改名为 tax
  column_to_rownames(var = "tax")       # tax 列转为行名
# df 的行是物种/OTU，列是样本

# 由于我们需要给分类线条添加颜色，通过上图可以看到分为6类因此在此使用cutree(hc,6)
hc <- hclust(dist(df))                  # 对行（物种）做层次聚类
clus <- cutree(hc, 6)                   # 切分为 6 个簇
g <- split(names(clus), clus)           # 按簇号把物种名拆分成列表

# ==================== 绘制基础环形树 ====================
p <- ggtree(hc,                         # 用聚类结果画树
            branch.length = "none",     # 不按枝长比例绘制
            layout = "circular",        # 环形布局
            linetype = 1, size = 0.5,   # 线条类型和宽度
            ladderize = T) +            # 阶梯化排列，让树更整齐
  layout_fan(angle = 90) +              # 扇形展开，开口 90 度
  theme_void()                          # 空白主题

clades <- sapply(g, function(n) MRCA(p, n))
# 对每个簇，找到该簇所有物种在树上的最近共同祖先节点
# 注意：这里 MRCA(p, n) 的用法依赖 ggtree 版本，新版本可能需要 MRCA(p, n) 或 MRCA(p, clade=n)

# ==================== 读取注释数据 ====================
deseq <- read_tsv("DEseq.xls") %>%      # 读取差异分析结果
  mutate(name = as.factor(name))        # name 列转因子

# ==================== 整理第二圈注释数据 ====================
data2 <- df %>%
  rownames_to_column(var = "ID") %>%    # 行名转为 ID 列
  select(ID, starts_with("n")) %>%      # 取 ID 和所有以 n 开头的列
  pivot_longer(-ID)                     # 宽转长：ID、name、value 三列
# ⚠️ select() 可能被其他包覆盖，若报错可改成 dplyr::select()

data3 <- data2 %>% filter(name %in% c("nap", "nif"))
# 只保留 name 为 nap 和 nif 的行，用于最外圈

# ==================== 绘制带注释的环形树 ====================
tree <- groupClade(p, clades, group_name = 'subtree') +   # 按簇给树分组
  aes(color = subtree) +                # 按 subtree 着色
  scale_color_brewer(palette = 'Paired', breaks = 1:6) +  # 6 个簇的颜色
  # ⚠️ groupClade() 的参数在新版 ggtree 中可能是 groupClade(p, clades, group_name=...) 或需要传 node 列表，若报错请查 ?groupClade
  
  new_scale_fill() +                    # 开启新的 fill 比例尺
  
  # ---- 第一圈：条形图（按样本） ----
geom_fruit(data = deseq,              # 注释数据
           geom = geom_bar,           # 用柱状图
           mapping = aes(y = sample,  # y 对应树上的物种/节点
                         x = name,    # x 为 name
                         fill = name),
           orientation = "y",         # 横向
           pwidth = 0,                # 与树之间的间距
           stat = "identity") +       # 直接用数值
  scale_fill_manual(values = c("#3C5488FF", "#00A087FF")) +  # 2 种颜色
  new_scale_fill() +
  
  # ---- 第二圈：热图（按 ID） ----
geom_fruit(data = data2,
           geom = geom_tile,          # 用方块
           mapping = aes(y = ID,
                         x = name,
                         alpha = value,
                         fill = name),
           color = "grey50",          # 方块边框
           offset = 0.03,             # 与树的距离
           size = 0.02) +             # 边框线宽
  scale_fill_manual(values = c("#FFC125","#87CEFA","#7B68EE","#808080","#800080")) +
  new_scale_fill() +
  
  # ---- 最外圈：柱状图（nap / nif） ----
geom_fruit(data = data3,
           geom = geom_bar,
           mapping = aes(y = ID,
                         x = value,
                         fill = name),
           offset = 0.05,
           orientation = "y",
           stat = "identity") +
  scale_fill_manual(values = c("#D15FEE", "#9ACD32")) +
  theme(legend.position = "none")        

# ==================== 绘制条形图 ====================
bar <- data2 %>%
  left_join(., deseq, by = c("ID" = "sample")) %>%   # 关联 deseq 数据
  ggplot(aes(name.x, value, fill = name.y)) +        # x=name.x, y=value, 填充=name.y
  geom_col(width = 0.5) +                            # 柱状图
  scale_fill_manual(values = c("#3C5488FF", "#00A087FF")) +
  labs(x = NULL, y = NULL, fill = NULL) +            # 无轴标签
  scale_y_continuous(expand = c(0, 0)) +             # y 轴不留空白
  theme_classic() +
  theme(
    panel.background = element_blank(),
    axis.line = element_line(color = "black"),
    axis.text = element_text(size = 10, color = "black"),
    axis.text.x = element_text(margin = margin(t = 5)),
    axis.text.y = element_text(size = 10),
    legend.position = "none"                        
  )

# ==================== 拼图 ====================
tree %>% ggdraw() +
  draw_plot(bar, scale = 0.32, x = 0.17, y = -0.23)
# 把 bar 缩小到 0.32 倍，放在 (0.17, -0.23) 位置
# ⚠️ 这些坐标是试出来的，需要根据实际图形反复微调

# ==================== 保存 ====================
ggsave("图.pdf", dpi = 300)
# 未指定宽高，默认使用当前设备尺寸