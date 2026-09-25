library(tidyverse)      # 加载 tidyverse（含 dplyr、tidyr、ggplot2、readr、forcats、stringr 等）
library(systemfonts)    # 加载 systemfonts，管理系统字体（本段未直接使用）
library(colorspace)     # 加载 colorspace，提供 darken()、lighten() 等颜色调整函数

# ==================== 读取数据 ====================
rent <- readr::read_csv('rent.txt')
# 读取 CSV 文件，返回 tibble
# 数据包含 year、city、beds、baths、nhood、price 等列

# ==================== 定义配色 ====================
colors <- wesanderson::wes_palettes$Zissou1
# 从 wesanderson 包中提取 Zissou1 调色板（需先安装 wesanderson）
# 结果是一个颜色向量，如 c("#3B9AB2", "#78B7C5", "#EBCC2A", "#E1AF00", "#F21A00")

# ==================== 筛选 2012 年旧金山 1b1b 数据 ====================
rent_sf_2012 <- rent %>%
  filter(year == 2012, city == "san francisco", beds == 1, baths == 1)
# 只保留 2012 年、旧金山、1 室 1 卫的房源

# ==================== 取出现次数最多的 10 个社区 ====================
top_10_nhoods <- rent_sf_2012 %>%
  count(nhood, sort = TRUE) %>%     # 按社区计数，降序排列
  head(10) %>%                      # 取前 10
  pull(nhood)                       # 提取社区名向量

# ==================== 汇总每个社区的价格分布 ====================
plot_data <- rent_sf_2012 %>%
  filter(nhood %in% top_10_nhoods) %>%   # 只保留 top 10 社区
  group_by(nhood) %>%                    # 按社区分组
  summarise(
    max_price = max(price),              # 最高价
    q1 = quantile(price, 0.25),          # 第一四分位数
    q3 = quantile(price, 0.75)           # 第三四分位数
  ) %>%
  mutate(nhood = fct_reorder(nhood, max_price)) %>%  # 按 max_price 排序社区
  arrange(desc(nhood)) %>%               # 再按社区名降序排列
  mutate(y_mid = 10:1,                   # y 轴位置：10 到 1（手动分配）
         y_start = y_mid - 0.30,         # 区间下边界
         y_end = y_mid + 0.30)           # 区间上边界

# ==================== 硬编码的社区标签 ====================
labels <- c("Pacific Heights", "Marina", "South Beach", "North Beach", "Russian Hill",
            "Nob Hill", "San Francisco", "Inner Sunset", "Outer Sunset", "Inner Richmond")
# 手动指定的标签，必须与 plot_data 中的顺序一一对应

# ==================== 绘图 ====================
ggplot(plot_data) +
  # ---- 1. 最大价格柱 ----
geom_col(aes(max_price, nhood),        # x=max_price，y=nhood
         width = 0.4,                  # 柱宽 0.4
         fill = colors[2]) +           # 填充色：调色板第 2 个
  
  # ---- 2. 中间 50% 区间色块 ----
geom_rect(aes(xmin = q1, xmax = q3,    # x 范围：Q1 到 Q3
              ymin = y_start, ymax = y_end),  # y 范围：区间上下边界
          fill = colors[2], alpha = 1) +
  
  # ---- 3. 左侧延伸的小色块（Q1 向左 30） ----
geom_rect(aes(xmin = q1, xmax = q1 - 30,
              ymin = y_start, ymax = y_end),
          fill = darken(colors[1], 0)) +   # darken 调整亮度，0 表示不变
  
  # ---- 4. 右侧延伸的小色块（Q3 向右 30） ----
geom_rect(aes(xmin = q3, xmax = q3 + 30,
              ymin = y_start, ymax = y_end),
          fill = darken(colors[1], 0)) +
  
  # ---- 5. 社区标签（文字） ----
geom_text(aes(x = 20, y = y_end + 0.05, label = labels),
          # x 固定在 20，y 在区间上边界稍上方
          color = "black", size = 3, hjust = 0) +  # 左对齐
  # ⚠️ labels 是外部向量，长度必须和 plot_data 行数一致，
  # 且顺序必须匹配（否则标签会错位）
  
  # ---- 6. 注释文字 ----
annotate("text", x = 3500, y = 10,     # 位置 (3500, 10)
         label = str_wrap("Price range covering
                            middle 50% of all quotes", 20),
         # str_wrap 将长文本按 20 字符宽度换行
         color = "black", size = 2.5, lineheight = 1) +
  
  # ---- 7. x 轴刻度：千位格式 ----
scale_x_continuous(labels = scales::comma_format(scale = 1/1000, suffix = "k")) +
  # 把 x 轴数值除以 1000，并加千分位和 "k" 后缀，例如 3500 → 3.5k
  
  # ---- 8. 允许元素超出绘图区域 ----
coord_cartesian(clip = "off") +        # 关闭裁剪，标签可超出边界
  
  # ---- 9. 主题 ----
theme_minimal(base_size = 10) +        # 极简主题，基础字号 10
  theme(
    plot.margin = margin(0.2, 0.2, 0.2, 0.2, unit = "in"),  # 图形边距
    plot.background = element_rect(fill = "grey98", color = "grey98"),  # 背景浅灰
    axis.text.x = element_text(face = "bold", color = "black"),  # x 轴文字加粗
    axis.text.y = element_blank(),       # y 轴文字不显示
    axis.title = element_blank(),        # 轴标题不显示
    panel.grid.major.x = element_line(linetype = "longdash"),  # x 方向主网格线
    panel.grid.minor = element_blank(),  # 不显示次网格线
    panel.grid.major.y = element_blank() # 不显示 y 方向主网格线
  )

# ==================== 保存图片 ====================
ggsave("图.pdf", width = 9, height = 5, dpi = 300)
# 保存为 PDF，宽 9 英寸，高 5 英寸，300 dpi