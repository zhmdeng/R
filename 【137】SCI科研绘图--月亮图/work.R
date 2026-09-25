library(tidyverse)      # 数据处理与绘图核心包
library(gggibbous)      # 提供 geom_moon()，绘制月亮盈亏图
library(ggtext)         # 支持富文本（本段未直接用）
library(ggfx)           # 提供 with_outer_glow()，给图形加外发光效果
library(showtext)       # 使用 Google 字体
library(magick)         # 图像处理（本段未直接用）
library(magrittr)       # 提供 %<>% 复合赋值管道

showtext_auto()         # 开启 showtext，自动使用注册的字体
font_add_google("Karla", "Karla")     # 从 Google Fonts 下载并注册 Karla 字体
font_add_google("Oswald", "Oswald")   # 下载并注册 Oswald 字体

# 读取日照数据
sunshine <- read_csv('sunshine.csv')

# 定义月份顺序
months_order <- c('Jan','Feb','Mar','Apr','May','Jun','Jul','Aug','Sep','Oct','Nov','Dec')

# 选中的城市
selected_cities <- c('Oslo', "Vienna", "Brussels", "Rome", "Milan", "Amsterdam", "Nicosia",
                     "Madrid", "Edinburgh", "Tromsø", "Tórshavn")

# 数据整理
sunshine %<>%                          # %<>% 表示就地修改 sunshine
  filter(City %in% selected_cities) %>%  # 只保留选中城市
  pivot_longer(cols = Jan:Dec,           # 把 1-12 月从宽表转成长表
               values_to = 'sunshine_hours',
               names_to = 'month') %>%
  mutate(total_hours = case_when(        # 计算每月总小时数
    month == 'Feb' ~ 28 * 24,            # 2 月按 28 天
    month %in% c('Jan','Mar','May','Jun','Jul','Aug','Oct','Dec') ~ 31 * 24,  # 31 天月份
    TRUE ~ 30 * 24                        # 其余 30 天
  )) %>%
  mutate(dark_hours = total_hours - sunshine_hours,  # 黑暗小时数
         ratio = sunshine_hours / total_hours) %>%   # 日照比例（0~1）
  mutate(month = factor(month, levels = months_order)) %>%  # 固定月份顺序
  mutate(right = TRUE)                    # geom_moon 需要的方向参数，全部设为 TRUE

# 绘图
g <- sunshine %>%
  ggplot() +
  # 底层：深蓝色圆（作为月亮暗面背景）
  geom_moon(aes(1, 1, ratio = 1),         # x=1, y=1, ratio=1 表示完整圆
            fill = '#023047', color = '#023047',
            size = 15, alpha = 0.9) +
  # 第二层：浅灰色圆
  geom_moon(aes(1, 1, ratio = 1),
            fill = '#e5e5e5', color = '#e5e5e5',
            size = 15, alpha = 0.3) +
  # 第三层：带外发光的月亮，ratio 按日照比例，right=TRUE 控制盈亏方向
  with_outer_glow(
    geom_moon(aes(1, 1, ratio = ratio, right = right),
              fill = '#fca311', color = '#fca311', size = 15),
    color = '#fca311'
  ) +
  # 在圆中心添加日照小时数文字
  geom_text(aes(1, 1, label = round(sunshine_hours)),
            color = 'grey80', size = 3) +
  coord_fixed() +                         # 固定纵横比，保证圆形不变形
  theme_void(base_family = 'Oswald') +    # 无坐标轴主题，基础字体 Oswald
  facet_grid(fct_reorder(City, Year) ~ month,  # 行：按 Year 排序的城市；列：月份
             switch = "y") +              # 分面标签放在 y 轴一侧
  theme(legend.position = "none")         # 不显示图例

g   # 显示图形

# 保存为 PDF
ggsave("图.pdf", width = 12, height = 8, device = cairo_pdf)