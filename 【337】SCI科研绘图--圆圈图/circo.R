library(readxl)              # 读取 Excel 文件
library(magrittr)            # 提供 %<>% 等管道操作符
library(dplyr)               # 数据处理
library(tibble)              # 数据框操作
library(ggplot2)             # 绘图

Sys.setenv(LANGUAGE = "en")  # 设置 R 报错信息为英文
options(stringsAsFactors = FALSE)  # 禁止字符型自动转为因子

## 读取第2个工作表
sheet1 <- read_excel(path = "13059_2016_990_MOESM1_ESM.xls", 
                     sheet = 2, 
                     range = cell_cols("B:D"),   # 只读取 B 到 D 列
                     col_names = TRUE, 
                     guess_max = 5)              # 猜测列类型的最大行数

## 读取第3个工作表
sheet2 <- read_excel(path = "13059_2016_990_MOESM1_ESM.xls", 
                     sheet = 3, 
                     range = cell_cols("B:E"),   # 读取 B 到 E 列
                     col_names = TRUE, 
                     guess_max = 5) %>% 
  .[, -2]                                        # 删除第 2 列

# 重命名变量，原来的变量名太长
colnames(sheet1) <- c("Gene", "log2FC", "P_value")
colnames(sheet2) <- c("Gene", "log2FC", "P_value")

# 根据文献要求，筛选 log2FC >= 0 的基因
sheet1 %<>% filter(log2FC >= 0)                  # 保留 log2FC 非负的行
sheet2 %<>% filter(log2FC >= 0) %>% select(Gene) # 保留 log2FC 非负，只留 Gene 列

# 取交集，保留 GBM 的 Fold-changes 和校正 p 值
sheet_bind <- inner_join(sheet1, sheet2, by = "Gene") %>% 
  distinct(Gene, .keep_all = TRUE)               # 按 Gene 去重

# 添加 GBM 和 GSC 标记列，值全为 1
sheet_bind$GBM <- rep(1, nrow(sheet_bind))
sheet_bind$GSC <- rep(1, nrow(sheet_bind))

# 读取生存信息
surInfo <- read.csv("survival.csv")
head(surInfo)

# 按基因名降序排列
surInfo <- arrange(surInfo, desc(Gene))

# 合并生存信息和基因信息，并按 survival 降序排列
data_bind <- left_join(surInfo, sheet_bind, by = "Gene") %>% 
  arrange(desc(survival))
head(data_bind)

# 写出合并后的数据，便于后续读取
write.csv(data_bind, "easy_input.csv", quote = F, row.names = F)

# 重新读取，指定列类型（第一列字符，其余数值）
data_bind <- read.csv("easy_input.csv", 
                      colClasses = c('character','numeric','numeric','numeric','numeric','numeric'))
head(data_bind)

# 三个环的坐标：设 h 为 log2FC 最大值加 0.2，作为环的起始高度
h <- max(data_bind$log2FC) + 0.2

# 构造 tile 坐标数据框：每个基因对应一个 x 位置（1 到 n），以及三个环的 y 坐标
tile_coord <- data.frame(Gene = data_bind$Gene, 
                         x = 1:nrow(data_bind),      # 中间 x 坐标
                         y1 = h, y2 = h + 2, y3 = h + 4,  # 三个环的 y 坐标
                         stringsAsFactors = FALSE)

Angle_space <- 8      # 角度空隙大小
Angle_just <- 90      # 角度偏移量

# 计算每个基因标签的角度
unit_angle <- 360 / (nrow(tile_coord) + Angle_space + 0.5)  # 单位角度
text_angle <- Angle_just - cumsum(rep(unit_angle, nrow(tile_coord)))  # 累积角度
tile_coord$hjust <- ifelse(text_angle < -90, 1, 0)          # 根据角度调整水平对齐
tile_coord$text_angle <- ifelse(text_angle < -90, text_angle + 180, text_angle)  # 翻转角度

# 将 tile 坐标合并到主数据
data_result <- left_join(data_bind, tile_coord, by = "Gene")
head(data_result)

# ==================== 绘图 ====================
p1 <- ggplot(data_result) + 
  # 第一圈：survival，值为1的用深灰色填充，值为0的用浅灰色
  geom_tile(data = filter(data_result, survival == 1),
            aes(x = x, y = y1, width = 1, height = 2), 
            fill = "grey20", color = "lightgrey", size = .3) + 
  geom_tile(data = filter(data_result, survival == 0),
            aes(x = x, y = y1, width = 1, height = 2),
            fill = "grey96", color = "lightgrey", size = .3) + 
  
  # 第二圈：GSC，值为1的用深灰色
  geom_tile(data = filter(data_result, GSC == 1),
            aes(x = x, y = y2, width = 1, height = 2), 
            fill = "grey20", color = "lightgrey", size = .3) + 
  # 如果 GSC 有 0 值，可取消注释下面三行
  #geom_tile(data = filter(data_result, GSC == 0),
  #          aes(x = x, y = y2, width = 1, height = 2),
  #          fill = "grey96", color = "lightgrey", size = .3) + 
  
  # 第三圈：GBM，值为1的用深灰色
  geom_tile(data = filter(data_result, GBM == 1),
            aes(x = x, y = y3, width = 1, height = 2), 
            fill = "grey20", color = "lightgrey", size = .3) + 
  # 如果 GBM 有 0 值，可取消注释下面三行
  #geom_tile(data = filter(data_result, GBM == 0),
  #          aes(x = x, y = y3, width = 1, height = 2),
  #          fill = "grey96", color = "lightgrey", size = .3) + 
  
  # 基因名标签：survival==1 的为黑色，否则为灰色
  geom_text(data = filter(data_result, survival == 1),
            aes(x = x, y = y3 + 1.6, label = Gene,
                angle = text_angle, hjust = hjust), 
            color = "black", size = 2) + 
  geom_text(data = filter(data_result, survival == 0),
            aes(x = x, y = y3 + 1.6, label = Gene,
                angle = text_angle, hjust = hjust), 
            color = "grey43", size = 2) + 
  
  coord_polar(theta = "x", start = 0, direction = 1) +   # 极坐标
  ylim(-5, 13) +                                         # y 轴范围
  xlim(0, nrow(data_bind) + Angle_space) +               # x 轴范围
  theme_void()                                           # 无主题
p1

# 添加 log2FC 柱状图（从 y=log2FC-1 开始画柱）
p2 <- p1 + geom_col(aes(x = x, y = log2FC - 1), fill = "grey85", color = "grey80")
p2

# 添加左侧的刻度和数字
p3 <- p2 + geom_segment(x = 0, y = 0, xend = 0, yend = 3, color = "black", size = .3) + 
  geom_text(x = -.5, y = 0, label = "1_", vjust = -.3, size = 1, color = "black") + 
  geom_text(x = -.5, y = 1, label = "2_", vjust = -.3, size = 1, color = "black") +
  geom_text(x = -.5, y = 2, label = "3_", vjust = -.3, size = 1, color = "black") +
  geom_text(x = -.5, y = 3, label = "4_", vjust = -.3, size = 1, color = "black")
p3

# 添加 p-value 热图（在 y=-1.2 处）
p4 <- p3 + geom_tile(aes(x = x, y = -1.2, width = 1, height = 2, fill = P_value), 
                     color = "white") + 
  scale_fill_gradient(low = "firebrick3", high = "yellow")   # 颜色渐变
p4

# 添加文本注释
p5 <- p4 + geom_text(x = -2.1, y = h + 4.5, label = "GBM TCGA", size = 3, color = "black") + 
  geom_text(x = -1.1, y = h + 2.5, label = "GSCs", size = 3, color = "black") + 
  geom_text(x = -1.8, y = h + .5, label = "Survival\nReduction", size = 2.5, color = "black") + 
  geom_text(x = -2.1, y = h - 2.4, label = expression('Log'[2]*'FC'), 
            angle = 90, size = 2.2, color = "black") + 
  geom_text(x = -3.3, y = h - 5, label = "p-value", 
            angle = 90, size = 1.5, color = "black") + 
  geom_text(x = 0, y = -5, label = "Upregulated\nRBPs", size = 2.5, color = "black")
p5

# 进一步调整：添加刻度文本和图例设置
p6 <- p5 +
  geom_text(x = -.5, y = -0.2, label = 0, size = 1, color = "black") + 
  geom_text(x = -1.8, y = -2, label = 0.05, size = 1, color = "black") + 
  guides(fill = guide_colorbar(title = "", reverse = TRUE, label = FALSE, 
                               barwidth = 0.1, barheight = 1.1)) + 
  theme(legend.position = c(0.5, 0.585),   # 图例位置
        legend.title = element_blank())
p6

# 保存图片
ggsave("图.pdf")