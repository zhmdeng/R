library(tidyverse)      # 加载 tidyverse，包含 ggplot2、dplyr、readr 等
library(data.table)     # 加载 data.table，用于高效数据处理（本段未直接使用）
library(rworldmap)      # 加载 rworldmap，提供世界地图数据（本段未直接使用）
library(maps)           # 加载 maps 包，提供 map_data() 获取世界地图边界

# 获取世界地图多边形数据
world_map <- map_data("world")   # 从 maps 包中提取世界地图边界数据，返回数据框

# 创建基础地图
map <- ggplot() +                # 初始化 ggplot 对象
  coord_fixed() +                # 固定纵横比，保证地图不变形
  xlab("x") + ylab("y") +        # 设置 x、y 轴标签（后续会被覆盖）
  geom_polygon(data = world_map, # 添加世界地图多边形
               aes(x = long, y = lat, group = group),  # x=经度，y=纬度，group 用于区分不同国家
               fill = "grey")    # 填充灰色

# 读取数据
db <- read.csv('data.txt', sep = ";")   # 读取分号分隔的数据文件

# 计算外来物种比例
db$alien_all <- with(db, alien_freq / pred_freq)   # 新增列 alien_all = 外来种频次 / 预测频次

# 定义外来物种是否存在的二值变量 PA
db <- db %>% mutate(PA = case_when(alien_freq >= 1 ~ 1,   # 外来种频次 >=1 则 PA=1
                                   alien_freq < 1 ~ 0))   # 否则 PA=0

# 提取所有采样点坐标
all_perdata_all <- db %>% dplyr::select(Longitude, Latitude) %>% distinct()
# 从 db 中选取经纬度，去重，得到所有采样点

# 提取外来物种存在的采样点坐标及比例
all_perdata_alien <- db %>% filter(PA == 1) %>%           # 筛选 PA=1（有外来种）
  dplyr::select(Longitude, Latitude, alien_all) %>% distinct()
# 选取经纬度和 alien_all，去重

# 绘制地图和点
all_a_num <- map +                                        # 基于前面创建的基础地图
  geom_point(data = all_perdata_all,                      # 添加所有采样点
             aes(x = Longitude, y = Latitude),
             shape = 1, color = 'black', stroke = 0.2) +  # 空心圆，黑色边框，线宽 0.2
  geom_point(data = all_perdata_alien,                    # 添加有外来种的采样点
             aes(x = Longitude, y = Latitude,
                 size = alien_all),                       # 点大小按 alien_all 映射
             color = 'blue', shape = 1) +                 # 蓝色空心圆
  scale_y_continuous(name = "Latitude°",                  # y 轴标签
                     breaks = seq(-80, 80, 40)) +         # y 轴刻度：-80 到 80，步长 40
  scale_x_continuous(name = "Longitude°",                 # x 轴标签
                     breaks = seq(-160, 160, 80)) +       # x 轴刻度：-160 到 160，步长 80
  scale_size_continuous(breaks = c(0.05, 0.25, 0.5, 0.75, 1),  # 点大小图例刻度
                        name = "Alien proportion") +      # 图例标题
  theme_bw() +                                            # 黑白主题
  theme(
    panel.border = element_rect(fill = NULL,              # 面板边框：无填充
                                colour = 'black',         # 黑色
                                linetype = 1,             # 实线
                                size = 0.5),              # 线宽 0.5
    axis.ticks.length = unit(0.08, "inch"),               # 刻度线长度 0.08 英寸
    plot.title = element_text(size = 15),                 # 标题字号 15
    legend.text = element_text(colour = "black", size = 10),  # 图例文字
    legend.background = element_blank(),                  # 图例背景透明
    legend.key = element_blank(),                         # 图例键背景透明
    axis.text.x = element_text(colour = "black", size = 12),  # x 轴文字
    axis.text.y = element_text(colour = "black", size = 12),  # y 轴文字
    axis.title.x = element_text(colour = 'black', size = 14, vjust = 1),  # x 轴标题
    axis.title.y = element_text(colour = 'black', size = 14, vjust = 1),  # y 轴标题
    legend.position = c(0.15, 0.33)                       # 图例位置（相对坐标）
  )

ggsave("图.pdf",width = 6,height = 6,dpi = 300)
