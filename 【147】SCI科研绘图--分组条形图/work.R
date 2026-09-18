library(tidyverse)      # 加载 tidyverse，包含 dplyr、tidyr、ggplot2、readr 等
library(ggbump)         # 加载 ggbump，提供 geom_sigmoid() 等绘制 S 形曲线的函数
library(ggsci)          # 加载 ggsci，提供 futurama 等期刊配色

# setwd("")
# 设置工作目录

# ==================== 读取并整理数据 ====================
dat <- read_tsv("data.xls") %>%          # 读取制表符分隔的数据文件
  select(1, 2, 3, 4) %>%                 # 只保留第 1-4 列（country, continent, year, lifeExp）
  mutate(year = as.character(year)) %>%  # 将 year 转为字符型（用于后续离散坐标）
  as.data.frame() %>%                    # 转为 data.frame（某些操作需要）
  filter(continent %in% c("Europe", "Americas"),   # 只保留欧洲和美洲
         year %in% c("1952", "2007")) %>%          # 只保留 1952 和 2007 年
  group_by(year, continent) %>%          # 按年份和洲分组
  mutate(percent = round(100 * lifeExp / sum(lifeExp), digits = 2)) %>%
  # 计算每个国家在对应洲、对应年份的预期寿命占比（百分比），保留 2 位小数
  top_n(3) %>%                           # 每个分组取前 3 行（默认按最后一列 percent 排序）
  # ⚠️ 注意：top_n(3) 默认使用最后一个变量（percent）降序取前 3。
  # 如果你的本意是按 lifeExp 取前 3，应该写成 top_n(3, lifeExp)。
  arrange(year, continent) %>%           # 按年份和洲排序
  ungroup()                              # 取消分组

# ==================== 绘制分组条形图 ====================
dat %>% ggplot() +                       # 使用 dat 数据创建 ggplot 对象
  # ---- 1. 条形图 ----
geom_bar(aes(fill = country,           # 填充色按国家
             y = percent,              # y 轴为百分比
             x = year),                # x 轴为年份
         position = "fill",            # 堆叠并归一化（总和为 1），但这里 y 已经是百分比，会再次归一化
         stat = "identity",            # 直接使用 y 值作为条形高度
         width = .5,                   # 条形宽度 0.5
         key_glyph = "dotplot") +      # 图例中显示为点图（可能不被 geom_bar 支持，若报错可删除）
  # ---- 2. 条形图内部显示百分比 ----
geom_text(aes(y = percent,             # y 位置为百分比值
              x = year,                # x 位置为年份
              label = paste0(percent, "%")),  # 标签为“xx%”
          size = 4,                    # 字体大小
          position = position_fill(vjust = 0.5),  # 在堆叠内部居中显示
          color = "white") +           # 白色文字
  # ---- 3. 在 x 轴下方添加年份标签 ----
geom_text(aes(x = year, y = -0.05, label = year),  # y 为负数，放在图外
          color = "black", hjust = 1, nudge_y = -0.01, size = 4) +
  # ---- 4. 在每个分面左侧添加洲名 ----
geom_text(aes(x = 1.5, y = 0, label = continent),  # x 固定在 1.5（中间位置）
          hjust = 0.5, nudge_y = -0.4, size = 5,   # 调整位置
          stat = "unique",                         # 每个分面只画一次
          color = "black") +
  # ---- 5. 绘制 S 形连接线（从年份标签指向洲名） ----
geom_sigmoid(aes(x = 2.25, xend = 1.5, y = -0.19, yend = -0.265),
             size = 0.2, direction = "y", color = "black", smooth = 8) +
  # 从 (2.25, -0.19) 到 (1.5, -0.265) 的 S 曲线
  geom_sigmoid(aes(x = 0.75, xend = 1.5, y = -0.19, yend = -0.265),
               size = 0.2, direction = "y", color = "black", smooth = 8) +
  # 从 (0.75, -0.19) 到 (1.5, -0.265) 的 S 曲线
  # ---- 6. 翻转坐标轴 ----
coord_flip(clip = "off", expand = FALSE) +
  # ⚠️ coord_flip() 不接受 expand 参数，expand 会被忽略或导致警告。
  # 若要控制轴扩展，应使用 scale_x_continuous(expand = ...) 或 scale_y_continuous(expand = ...)
  # ---- 7. 填充色 ----
scale_fill_futurama() +               # 使用 futurama 配色（来自 ggsci）
  # ---- 8. 分面 ----
facet_wrap(vars(continent), ncol = 1) +  # 按 continent 分面，1 列
  # ---- 9. 主题 ----
theme_void() +                        # 空白主题（无坐标轴、背景）
  theme(
    plot.margin = margin(30, 30, 30, 50),  # 图形边距：上、右、下、左
    strip.text = element_blank(),          # 分面标签不显示
    panel.spacing.y = unit(2, "lines"),    # 分面垂直间距
    legend.title = element_blank(),        # 图例标题不显示
    legend.key = element_blank(),          # 图例键背景透明
    legend.text = element_text(color = "black", size = 10),  # 图例文字
    legend.spacing.x = unit(0.1, 'cm'),    # 图例水平间距
    legend.key.width = unit(0.6, 'cm'),    # 图例键宽度
    legend.key.height = unit(0.6, 'cm'),   # 图例键高度
    legend.background = element_blank(),   # 图例背景透明
    legend.box.background = element_rect(colour = "black"),  # 图例框黑色边框
    legend.box.margin = margin(0, 2, 0, 0) # 图例框边距
  )

# ==================== 保存图片 ====================
ggsave("图.pdf", width = 8, height = 5, dpi = 300)
# 保存为 PDF，宽 8 英寸，高 5 英寸，分辨率 300 dpi