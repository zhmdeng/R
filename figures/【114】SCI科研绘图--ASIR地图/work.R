library(ComplexHeatmap)   # 加载 ComplexHeatmap 包（本段代码未直接使用，可能是备用）
library(ggspatial)        # 加载 ggspatial，提供 annotation_north_arrow() 等地图辅助函数
library(sf)               # 加载 sf，用于空间数据处理（本段未直接使用）
library(tidyverse)        # 加载 tidyverse，包含 ggplot2、dplyr、tidyr、stringr 等
library(ggpubr)           # 加载 ggpubr，提供 ggtexttable() 用于绘制表格
library(cowplot)          # 加载 cowplot，提供 ggdraw() 和 draw_plot() 用于组合图形

F6ASIR <- read_tsv("data-ASIR.txt")   # 读取 ASIR 数据文件，制表符分隔

# 从 maps 包获取世界地图数据，并筛选掉南极洲
world_map <- map_data("world") %>% filter(region != "Antarctica") %>%
  dplyr::rename(country = region) %>%   # 将 region 列重命名为 country，便于后续匹配
  # 以下一系列 mutate 用于将地图数据中的国家名称替换为与 ASIR 数据一致的名称
  mutate(country = str_replace(country, "Taiwan", "China")) %>%          # 台湾替换为中国
  mutate(country = str_replace(country, "North Korea", "Democratic People's Republic of Korea")) %>%  # 朝鲜
  mutate(country = str_replace(country, "South Korea", "Republic of Korea")) %>%  # 韩国
  mutate(country = str_replace(country, "Russia", "Russian Federation")) %>%      # 俄罗斯
  mutate(country = str_replace(country, "USA", "United States of America")) %>%   # 美国
  mutate(country = str_replace(country, "Iran", "Iran (Islamic Republic of)")) %>% # 伊朗
  mutate(country = str_replace(country, "Vietnam", "Viet Nam")) %>%              # 越南
  mutate(country = str_replace(country, "Laos", "Lao People's Democratic Republic")) %>%  # 老挝
  mutate(country = str_replace(country, "Syria", "Syrian Arab Republic")) %>%    # 叙利亚
  mutate(country = str_replace(country, "Moldova", "Republic of Moldova")) %>%   # 摩尔多瓦
  mutate(country = str_replace(country, "Czech Republic", "Czechia")) %>%        # 捷克
  mutate(country = str_replace(country, "UK", "United Kingdom")) %>%             # 英国
  mutate(country = str_replace(country, "Ivory Coast", "Cote d'Ivoire")) %>%     # 科特迪瓦
  mutate(country = str_replace(country, "Democratic Republic of the Congo", "Congo")) %>%  # 刚果（金）
  mutate(country = str_replace(country, "Republic of Congo", "Congo")) %>%       # 刚果（布）
  mutate(country = str_replace(country, "Tanzania", "United Republic of Tanzania")) %>%  # 坦桑尼亚
  mutate(country = str_replace(country, "Venezuela", "Venezuela (Bolivarian Republic of)")) %>%  # 委内瑞拉
  mutate(country = str_replace(country, "Bolivia", "Bolivia (Plurinational State of)"))  # 玻利维亚

#-------------------------------------- ASIR-----------------------------------
# ASIR 高：提取 ASIR 值，按降序排列
F6ASIR %>% separate(`2019-2`, into = "ASIR", sep = " ") %>%   # 将列 `2019-2` 按空格拆分为 ASIR 列
  select(location, ASIR) %>% mutate(ASIR = as.numeric(ASIR)) %>% arrange(desc(ASIR))  # 转为数值并按降序排列

# ASIR 低：同上，但按升序排列
F6ASIR %>% separate(`2019-2`, into = "ASIR", sep = " ") %>%
  select(location, ASIR) %>% mutate(ASIR = as.numeric(ASIR)) %>% arrange(ASIR)

# 获取相关国家经纬度，并绘制表格
m1 <- world_map %>% 
  left_join(., F6ASIR %>% separate(`2019-2`, into = "ASIR", sep = " "),  # 将 ASIR 数据与地图数据连接
            by = c("country" = "location")) %>%   # 按国家名连接
  mutate(ASIR = as.numeric(ASIR)) %>%             # ASIR 转为数值
  arrange(desc(ASIR)) %>%                         # 按 ASIR 降序
  filter(country != "Kiribati") %>%               # 排除基里巴斯
  filter(country %in% c("Poland", "Australia", "United States of America",  # 选择低 ASIR 国家
                        "Bangladesh", "Bhutan", "India")) %>%              # 选择高 ASIR 国家
  group_by(country) %>%                           # 按国家分组
  slice(which.max(ASIR)) %>%                      # 每个国家取 ASIR 最大的那一行（确保唯一）
  select(1, 2, country, ASIR) %>%                 # 选取经度、纬度、国家、ASIR
  arrange(desc(ASIR)) %>%                         # 按 ASIR 降序
  select(-1, -2) %>%                              # 删除经纬度列
  as.data.frame() %>%                             # 转为数据框
  ggtexttable(rows = NULL, theme = ttheme("mBlue"))  # 用 ggtexttable 绘制表格，蓝色主题

# ASIR 绘制地图
map1 <- world_map %>% 
  left_join(., F6ASIR %>% separate(`2019-2`, into = "ASIR", sep = " "),  # 再次连接 ASIR 数据
            by = c("country" = "location")) %>% 
  mutate(ASIR = as.numeric(ASIR)) %>%             # ASIR 转为数值
  arrange(desc(ASIR)) %>%                         # 按 ASIR 降序
  filter(country != "Kiribati") %>%               # 排除基里巴斯
  ggplot() +                                      # 开始绘图
  geom_polygon(aes(x = long, y = lat, group = group, fill = ASIR),  # 绘制多边形（国家），填充色按 ASIR
               color = "black", size = 0.2, show.legend = T) +      # 黑色边框，线宽 0.2，显示图例
  annotate("text", x = 13, y = -50, hjust = 0.5, size = 30,        # 在图中添加注释文本
           color = "#999999", label = "2019", alpha = .3) +        # 内容为 "2019"，半透明灰色
  scale_fill_gradientn(colours = colorRampPalette(c("#5eaaf5", "#f4d963", "red"))(10),  # 颜色渐变：蓝-黄-红
                       na.value = "grey80") +                      # 缺失值填充灰色
  theme_void() +                                  # 空白主题
  theme(plot.margin = unit(c(0, 0, 0, 0), units = "cm"),  # 图形边距为 0
        legend.title = element_blank(),           # 图例标题为空
        legend.text = element_text(size = 8),     # 图例文字大小 8
        legend.position = "bottom",               # 图例在底部
        legend.justification = c(0.5, 1)) +       # 图例对齐方式
  annotation_north_arrow(location = "bl",         # 添加指北针，位置在左下角
                         pad_x = unit(0.15, "in"), pad_y = unit(4.6, "in"),  # 内边距
                         style = north_arrow_nautical(fill = c("grey40", "white"),  # 指北针样式
                                                      line_col = "grey20")) +
  guides(fill = guide_colorbar(direction = "horizontal",  # 图例颜色条水平放置
                               reverse = F,                # 不反转
                               barwidth = unit(10, "cm"),  # 颜色条宽度
                               barheight = unit(0.5, "cm")))  # 颜色条高度

ggdraw(map1) +                                    # 以地图为底图
  draw_plot(m1, x = -0.37, y = -0.25, scale = 0.001)  # 将表格 m1 叠加到地图上，位置 (-0.37, -0.25)，缩放比例 0.001

ggsave("图.pdf", width = 9.22, height = 5.75, units = "in", dpi = 300)  # 保存为 PDF，宽 9.22 英寸，高 5.75 英寸，300 dpi