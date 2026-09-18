library(tidyverse)      # 数据处理和绘图（含 ggplot2、dplyr、stringr 等）
library(readxl)         # 读取 Excel 文件
# devtools::install_github("doehm/ggbrick")   # 安装 ggbrick（已注释）
library(ggbrick)        # 提供 geom_brick()，绘制砖块图
library(scales)         # 提供 label_number() 等数值格式化函数
library(MetBrewer)      # 提供 met.brewer() 艺术配色
library(patchwork)      # 图形组合

sessionInfo()           # 打印当前 R 会话信息（包版本、平台等）

# ==================== 读取第一个数据表 ====================
df <- read_excel("Source Data Figure 3.xlsx", sheet = 3)
# 从 Excel 的第 3 个工作表读取数据

# ==================== 整理数据 ====================
dff <- df %>%
  mutate(Cluster = str_replace_all(Cluster, c("Cluster" = "C")),
         # 把 Cluster 列中的 "Cluster" 替换为 "C"，例如 "Cluster1" → "C1"
         summit = summit / 10000000)
# 把 summit 列除以 10000000（可能为了缩小数值范围）

dff$Cluster <- factor(dff$Cluster,
                      levels = rev(c("C1","C2","C3","C4","C5",
                                     "C6","C7","C8","C9","C10","C11","C12")))
# 将 Cluster 转为因子，水平反转（让 C1 在顶部）

# ==================== 绘制图 plot1 ====================
plot1 <- dff %>%
  ggplot(aes(Cluster, summit)) +                     # x=Cluster，y=summit
  geom_brick(aes(Cluster, summit, fill = Type),      # 砖块图，按 Type 填充
             colour = NA, size = 0.2) +              # 无边框，砖块大小 0.2
  scale_y_continuous(labels = label_number(),        # y 轴数值格式化（千分位）
                     position = "right",             # y 轴放在右侧
                     expand = c(0, 0)) +             # y 轴不留空白
  scale_x_discrete(expand = c(0, 0)) +               # x 轴不留空白
  labs(y = "Number of OCRs", x = NULL, fill = NULL) +  # 轴标签，图例标题为空
  scale_fill_manual(values = c("#788FCE","#E6956F","#A6BA96")) +  # 手动填充色
  coord_flip() +                                     # 翻转坐标轴（横向砖块图）
  theme_classic() +                                  # 经典主题
  theme(
    axis.text.x = element_text(color = "black", size = 8, face = "bold"),  # x 轴文字
    axis.text.y = element_text(color = "black", size = 8, face = "bold"),  # y 轴文字
    plot.background = element_rect(fill = "white", colour = "white"),      # 背景白色
    plot.margin = margin(b = 5, t = 5, r = 5, l = 5),                      # 边距
    legend.text = element_text(color = "black", size = 8, face = "bold"),  # 图例文字
    legend.background = element_blank(),                                   # 图例背景透明
    legend.key.height = unit(0.5, "cm"),                                   # 图例键高度
    legend.key.width = unit(0.5, "cm"),                                    # 图例键宽度
    legend.position = c(0.4, 0.1)                                          # 图例位置
  )

# ==================== 读取第二个数据表 ====================
p2 <- read_excel("Source Data Figure 3.xlsx", sheet = 4)
# 读取第 4 个工作表

df <- p2 %>%
  mutate(`-log10(p.adjust)` = -log10(p.adjust),      # 计算 -log10(p.adjust)
         cluster = str_replace_all(cluster, c("Cluster" = "C"))) %>%
  # 将 cluster 列中的 "Cluster" 替换为 "C"
  select(2, 9, 10, 12)                               # 选择第 2、9、10、12 列

df$cluster <- factor(df$cluster,
                     levels = c("C1","C2","C3","C4",
                                "C6","C7","C9","C10","C11"))
# 设定 cluster 的因子水平（注意缺少 C5、C8、C12）

# ==================== 绘制图 plot2 ====================
plot2 <- df %>%
  ggplot(., aes(cluster, Description,              # x=cluster，y=Description
                size = Count,                      # 点大小按 Count
                fill = `-log10(p.adjust)`)) +      # 填充按 -log10(p.adjust)
  geom_point(shape = 21) +                         # 点形状 21（带填充的圆）
  labs(x = NULL, y = NULL, color = NULL) +         # 不显示轴标签
  scale_fill_gradientn(colors = met.brewer("Cassatt1")) +  # MetBrewer 配色
  theme_bw() +                                     # 黑白主题
  theme(
    axis.text.x = element_text(color = "black"),   # x 轴文字
    axis.text.y = element_text(color = "black"),   # y 轴文字
    panel.background = element_blank(),            # 面板背景透明
    panel.border = element_rect(fill = NA,          # 面板边框
                                color = "grey80",
                                size = 1,
                                linetype = "solid")
  )

# ==================== 组合图形 ====================
plot1 + plot2 + plot_layout(width = c(1.5, 1))
# ❌ 参数名错误：应该是 widths，不是 width
# 正确写法：plot_layout(widths = c(1.5, 1))

# ==================== 保存图片 ====================
ggsave("图.pdf", width = 10, height = 8, dpi = 300)   # 保存为 PDF，10×8 英寸，300 dpi