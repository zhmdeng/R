rm(list = ls())   # 清空当前环境中的所有对象
# setwd('')   # 设置工作目录

# ==================== 加载包 ====================
library(ggplot2)  # 绘图核心包
library(aplot)    # 用于图形拼图，提供 insert_right()、insert_top() 等

# ==================== 读取数据 ====================
df <- read.table("data.txt", header = 1, check.names = F)
# 读取制表符分隔的数据，第一行为列名，不修改列名
# 数据应为长格式：至少包含 group、species、value 三列

df$group <- factor(df$group, levels = c("F", "CK"))
# 将 group 转为因子，固定顺序为 F、CK

df$species <- factor(df$species, levels = c("Pedobacter", "Aridibacter", "Devosia", "Rhizobium",
                                            "Phenylobacterium", "Arthrobacter", "Bradvrhizobium",
                                            "Pseudomonas", "Gemmatimonas", "Sphingomonas"))
# 将 species 转为因子，固定物种顺序（热图 y 轴从上到下按此顺序）

# ==================== 绘制热图 p1 ====================
col <- colorRampPalette(c("#0066b2", "#fdbd10", "#ec1c24"))(50)
# 生成 50 个渐变色，从蓝到黄到红

p1 <- ggplot(df, aes(group, species, fill = value)) +   # x=group，y=species，填充色=value
  geom_tile(color = "black") +                          # 热图方块，黑色边框
  geom_text(aes(label = value), color = 'white', size = 5) +  # 在方块上显示数值，白色
  labs(x = NULL, y = NULL, fill = NULL) +               # 不显示轴标签和图例标题
  scale_fill_gradientn(colours = col) +                 # 使用自定义渐变色
  theme_void() +                                        # 空白主题
  theme(
    axis.text.x = element_text(color = "black", size = 12),       # x 轴文字
    axis.text.y = element_text(color = "black", size = 12, hjust = 1),  # y 轴文字右对齐
    legend.position = "right"                           # 图例放在右侧
  )
p1   # 显示热图

# ==================== 绘制柱状堆积图 p2 ====================
p2 <- ggplot(df, aes(species, value, fill = group)) +   # x=species，y=value，填充=group
  geom_col() +                                          # 柱状图（默认 stat="identity"）
  coord_flip() +                                        # 翻转坐标轴，使柱状图横向
  labs(x = NULL, y = NULL, fill = NULL) +               # 不显示轴标签和图例标题
  scale_fill_manual(values = c("#f0b240", "#62d7f6")) + # 手动填充色
  theme_classic() +                                     # 经典主题
  theme(
    axis.text.x = element_text(color = "black", size = 12),       # x 轴文字
    axis.text.y = element_blank(),                      # y 轴文字不显示（物种名已在热图显示）
    axis.line.x = element_line(color = "black", linewidth = 0.8), # x 轴线
    axis.line.y = element_blank(),                      # 不显示 y 轴线
    axis.ticks.y = element_blank(),                     # 不显示 y 轴刻度
    axis.ticks.length.x = unit(-0.15, "cm"),            # x 轴刻度朝内
    axis.ticks.x = element_line(color = "black", linewidth = 0.8),# x 轴刻度线
    legend.position = "right"                           # 图例放在右侧
  ) +
  scale_y_continuous(expand = c(0, 0), breaks = c(0, 5, 10, 15, 20, 25))
# y 轴从 0 开始，刻度指定为 0,5,10,15,20,25
p2   # 显示柱状图

# ==================== 拼图 ====================
p1 %>% insert_right(p2, width = 2)
# 将 p2 插入到 p1 的右侧，宽度比例为 2（相对于 p1 的宽度 1）
# 注意：这里没有赋值给变量，直接打印了组合图

# ==================== 保存 ====================
ggsave("图.pdf", width = 9, height = 6, dpi = 300)
# 保存最后一次显示的图形（即组合图）为 PDF