# ========== 加载所需的包 ==========
library(tidyverse)   # 数据处理和绘图
library(readxl)      # 读取 Excel 文件
library(magrittr)    # 提供管道操作符

# ========== 读取数据 ==========
df <- read_excel("F1.xlsx", sheet = "Fig 1c KEGG module") %>% 
  column_to_rownames(var = "...1")   # 将第一列（列名为 "...1"）设为行名

# ========== 数据整形 ==========
df2 <- df %>% 
  rownames_to_column(var = "ID") %>%    # 行名转为 ID 列
  head(20) %>%                           # 只取前 20 行
  select(1:13) %>%                       # 只取前 13 列
  pivot_longer(-1) %>%                   # 宽转长：除 ID 外的列变成长表
  set_colnames(c("ID", "name", "value")) # 重命名列为 ID、name、value

# ========== 准备热图数据 ==========
p1 <- df2 %>% 
  filter(name %in% c("J043V6", "J035V8", "J009V6")) %>%   # 只保留 3 个特定 name
  mutate(p_value = case_when(value < 0 ~ "**")) %>%        # 当 value < 0 时标记为 "**"
  drop_na() %>%                                             # 删除 NA 行
  mutate(group = case_when(value < -0.5 ~ "latter",         # value < -0.5 → "latter"
                           TRUE ~ "former"))                # 否则 → "former"

# ========== 绘制组合图 ==========
ggplot() +
  # ---- 热图部分 ----
geom_tile(data = p1, aes(ID, name, fill = group)) +       # 用方块填充颜色表示 group
  geom_text(data = p1, aes(ID, name, label = p_value),      # 在方块上叠加文本标注
            color = "white", vjust = 1, size = 5, hjust = 0.5) +
  
  # ---- 气泡图部分 ----
geom_point(data = df2 %>% 
             filter(name %in% c("C422V8","C402V8","C220V8",
                                "1107V4","1075V6","1055V4")),  # 只保留 6 个特定 name
           aes(ID, name, color = value, size = value)) +       # 用点的大小和颜色表示 value
  
  # ---- 坐标轴与图例设置 ----
coord_cartesian(clip = "off") +                           # 关闭坐标轴裁剪（允许元素超出边界）
  labs(x = NULL, y = NULL) +                                # 移除 x 和 y 轴标签
  scale_x_discrete(expand = c(0, 0)) +                      # x 轴不留空白
  scale_fill_manual(values = c("#E6956F", "#788FCE")) +      # 热图填充色（2 组）
  scale_color_gradient2(mid = "#FBFEF9", low = "#0C6291", high = "#A63446") +  # 气泡颜色渐变
  theme_test() +                                             # 使用测试主题（简洁风格）
  theme(
    axis.text.x = element_text(color = "black", size = 8, face = "bold",
                               angle = 90, vjust = 0.5),        # x 轴文字旋转 90 度
    axis.text.y = element_text(color = "black", size = 8, face = "bold",
                               angle = 0, vjust = 0.5),         # y 轴文字水平
    axis.ticks = element_blank(),                               # 移除轴刻度
    legend.title = element_blank(),                             # 移除图例标题
    legend.background = element_blank(),                        # 图例背景透明
    legend.text = element_text(size = 8, color = "black", face = "bold"),  # 图例文字
    legend.position = "right",                                  # 图例放在右侧
    legend.spacing.x = unit(0.05, "in"),                        # 图例水平间距
    plot.margin = ggplot2::margin(10, 30, 10, 10),              # 图形边距
    legend.direction = "vertical",                              # 图例垂直排列
    legend.box = "horizontal"                                   # 图例框水平排列
  )

# ========== 保存图片 ==========
ggsave("组合图.pdf", width = 5, height = 5, dpi = 600)   # 保存为 PDF，5×5 英寸，600 dpi