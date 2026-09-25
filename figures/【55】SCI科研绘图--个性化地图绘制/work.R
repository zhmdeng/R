# install.packages("rnaturalearthdata")
# install.packages("tidygeocoder")

# ==================== 加载包 ====================
library(tidygeocoder)   # 地理编码
library(tidyverse)      # 数据处理和绘图
library(sf)             # 空间数据处理（st_polygon、st_sfc、st_sf、st_transform）
library(camcorder)      # 图形录制（本代码未使用）
library(scico)          # 科学配色（本代码未使用）
library(rnaturalearth)  # 提供 ne_countries()，获取世界地图
library(terra)          # 栅格数据处理（本代码未直接使用）
library(tidyterra)      # 让 terra 对象支持 tidyverse 语法（本代码未直接使用）
library(geodata)        # 地理数据下载（本代码未直接使用）
library(patchwork)      # 图形组合（本代码未直接使用）
library(ggsci)          # 期刊配色（NPG、JCO 等）
library(cowplot)        # 提供 ggdraw()、draw_plot()，用于组合图形

# ==================== 构建地图框架数据 ====================
lats <- c(90:-90, -90:90, 90)
# 纬度序列：从 90 到 -90，再从 -90 到 90，最后回到 90（形成闭合多边形）
longs <- c(rep(c(180, -180), each = 181), 180)
# 经度序列：180 和 -180 各重复 181 次，最后加一个 180（形成闭合多边形）
crs_wintri <- "+proj=robin +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs"
# Robinson 投影的 CRS 定义

wintri_outline <- 
  list(cbind(longs, lats)) %>%          # 把经纬度组合成矩阵，放入列表
  st_polygon() %>%                       # 创建多边形
  st_sfc(crs = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs") %>%
  # 加上经纬度坐标系（WGS84）
  st_sf() %>%                            # 转为 sf 数据框
  st_transform(crs = crs_wintri)         # 投影变换到 Robinson
# 结果：全球边界多边形（Robinson 投影）

# ==================== 构建经纬度线条 ====================
grat_wintri <- st_graticule(lat = c(-89.9, seq(-80, 80, 20), 89.9)) %>%
  # 生成经纬网，纬度取 -89.9、-80 到 80（步长 20）、89.9
  st_transform(crs = crs_wintri)   # 投影变换到 Robinson

# ==================== 构建地图信息 ====================
robinson <- crs_wintri              # 直接复用 Robinson 投影

map <- ne_countries(scale = 50, returnclass = 'sf') %>% 
  # 从 rnaturalearth 获取 1:50m 精度的世界地图，返回 sf 对象
  st_transform(crs = robinson)      # 投影变换到 Robinson

# ==================== 构建经纬度文本信息 ====================
g <- st_graticule(ndiscr = 500)
# 生成更密集的经纬网（每个方向 500 个点），用于提取标签位置

# ---- 经度标签 ----
labels_x_init <- g %>% 
  filter(type == "N") %>%             # 筛选 N 类型（南北方向的线，对应经度标签）
  mutate(lab = paste0(degree, "°"))   # 生成标签：度数 + °

labels_x <- st_as_sf(
  st_drop_geometry(labels_x_init),    # 去掉几何列，只保留属性
  lwgeom::st_startpoint(labels_x_init) # 取每条线的起点作为标签位置
)

# ---- 纬度标签 ----
labels_y_init <- g %>% 
  filter(type == "E") %>%             # 筛选 E 类型（东西方向的线，对应纬度标签）
  mutate(lab = paste0(degree, "°"))   # 生成标签

labels_y <- st_as_sf(
  st_drop_geometry(labels_y_init),
  lwgeom::st_startpoint(labels_y_init) # 取每条线的起点
) %>% 
  filter(degree %in% c(180, 120, 80, 40, -180, -120, -80, -40, 0))
# 只保留特定度数的标签，避免太密集

# ==================== 绘制地图 p1 ====================
p1 <- ggplot() +
  geom_sf(data = wintri_outline,        # 全球边界多边形
          fill = "#5BBCD6",              # 填充色：浅蓝
          color = NA,                    # 边框透明
          alpha = 0.5) +                 # 透明度 0.5
  geom_sf(data = grat_wintri,            # 经纬网
          color = "grey",                # 灰色
          linewidth = 0.15) +            # 线宽 0.15
  geom_sf(data = map,                    # 世界地图
          size = 0.1,                    # 边框线宽
          color = "#28282B") +           # 深灰色
  geom_sf(data = map %>% filter(name %in% c("Australia", "South Africa",
                                            "France", "Germany", "Russia", "Egypt",
                                            "Hungary", "Japan", "Brazil", "UK",
                                            "United States", "Canada")),
          # 筛选出需要高亮显示的国家
          size = 0.1, color = "#28282B",
          aes(fill = pop_est),           # 按人口填充颜色
          show.legend = F) +             # 不显示图例
  geom_sf_text(data = labels_x,          # 经度标签
               aes(label = lab),
               nudge_x = -600000,        # x 方向偏移 -600000 米
               size = 2) +               # 字号 2
  geom_sf_text(data = labels_y,          # 纬度标签
               aes(label = lab),
               nudge_y = -600000,        # y 方向偏移 -600000 米
               size = 2) +
  scale_fill_gradientn(colours = alpha(RColorBrewer::brewer.pal(6, "RdBu"), 0.5)) +
  # 填充色：RdBu 配色加透明度
  theme_void() +                         # 空主题
  labs(x = NULL, y = NULL) +             # 不显示轴标签
  theme(plot.margin = margin(0, 0.5, 0, 0.5, unit = "cm"))
# 图形边距：左右各 0.5 cm

# ==================== 绘制地图 p2 ====================
p2 <- ggplot() +
  geom_sf(data = wintri_outline, fill = "#5BBCD6", color = NA, alpha = 0.5) +
  geom_sf(data = grat_wintri, color = "grey", linewidth = 0.15) +
  geom_sf(data = map, size = 0.1, color = "#28282B") +
  geom_sf(data = map %>% filter(name %in% c("Austria", "Belgium", "Bulgaria",
                                            "Croatia", "Cyprus", "Czech Rep.",
                                            "Denmark", "Estonia", "Finland",
                                            "France", "Germany", "Greece")),
          # 这次筛选的是欧洲国家
          size = 0.1, color = "#28282B", aes(fill = pop_est), show.legend = F) +
  geom_sf_text(data = labels_x, aes(label = lab), nudge_x = -600000, size = 2) +
  geom_sf_text(data = labels_y, aes(label = lab), nudge_y = -600000, size = 2) +
  scale_fill_gradientn(colours = alpha(RColorBrewer::brewer.pal(6, "RdBu"), 0.5)) +
  theme_void() +
  labs(x = NULL, y = NULL) +
  theme(plot.margin = margin(0, 0.5, 0, 0.5, unit = "cm"))

# ==================== 绘制 Taxonomic richness ====================
df1 <- read_tsv("data.xls") %>%         # 读取数据
  filter(type == "Taxonomic richness") %>%   # 筛选分类丰富度
  select(5:9) %>%                        # 取第 5-9 列
  group_by(REALM) %>%                    # 按 REALM 分组
  slice_head(n = 1)                      # 每组取第一行

df1$REALM <- factor(df1$REALM,           # REALM 转因子
                    levels = c("Nearctic", "Palearctic", "Indomalayan",
                               "Neotropic", "Afrotropic", "Australasia"))
# 固定 REALM 顺序

plot1 <- df1 %>% ggplot(aes(y = fct_rev(REALM))) +   # y 轴为反转的 REALM
  theme_bw() +                                        # 黑白主题
  geom_errorbarh(aes(xmin = Lower_ci, xmax = Upper_ci),  # 水平误差棒
                 height = 0.1) +                      # 端点高度
  geom_point(aes(x = visregFit, color = REALM),       # 点：x 为 visregFit，颜色按 REALM
             fill = "black",                          # 填充黑色
             size = 3,                                # 点大小 3
             show.legend = F) +                       # 不显示图例
  labs(x = "Taxonomic richness", y = NULL) +          # 轴标签
  scale_color_npg() +                                 # NPG 配色
  theme(axis.ticks.y = element_blank(),               # 不显示 y 轴刻度
        axis.title.y = element_blank(),               # 不显示 y 轴标题
        axis.title.x = element_text(color = "black", size = 8, face = "bold"),
        axis.text.y = element_text(color = "black", size = 8, face = "bold"),
        axis.text.x = element_text(color = "black", size = 8, face = "bold"))

# ==================== 绘制 Functional richness ====================
df2 <- read_tsv("data.xls") %>%
  filter(type == "Functional richness") %>%   # 筛选功能丰富度
  select(5:9) %>%
  group_by(REALM) %>%
  slice_head(n = 1)

df2$REALM <- factor(df2$REALM, 
                    levels = c("Nearctic", "Palearctic", "Indomalayan",
                               "Neotropic", "Afrotropic", "Australasia"))

plot2 <- df2 %>% ggplot(aes(y = fct_rev(REALM))) +
  theme_bw() +
  geom_errorbarh(aes(xmin = Lower_ci, xmax = Upper_ci), height = 0.1) +
  geom_point(aes(x = visregFit, color = REALM), fill = "black",
             size = 3, show.legend = F) +
  labs(x = "Functional richness", y = NULL) +
  scale_color_npg() +
  theme(axis.ticks.y = element_blank(),
        axis.title.y = element_blank(),
        axis.title.x = element_text(color = "black", size = 8, face = "bold"),
        axis.text.y = element_text(color = "black", size = 8, face = "bold"),
        axis.text.x = element_text(color = "black", size = 8, face = "bold"))

# ==================== 组合图形 ====================
plot <- ggdraw() +                        # 创建空白画布
  draw_plot(p1, scale = 0.56, x = -0.25, y = 0.24) +
  # 放置 p1：缩放 0.56，位置 (-0.25, 0.24)
  draw_plot(p2, scale = 0.56, x = 0.25, y = 0.24) +
  # 放置 p2：缩放 0.56，位置 (0.25, 0.24)
  draw_plot(plot1, scale = 0.45, x = -0.27, y = -0.28) +
  # 放置 plot1：缩放 0.45，位置 (-0.27, -0.28)
  draw_plot(plot2, scale = 0.45, x = 0.23, y = -0.28)
# 放置 plot2：缩放 0.45，位置 (0.23, -0.28)

# ==================== 保存图片 ====================
ggsave(plot, file = "个性地图.pdf",       # 保存对象和文件名
       width = 8.65, height = 4.21,       # 宽 8.65 英寸，高 4.21 英寸
       units = "in",                      # 单位：英寸
       dpi = 300)                         # 分辨率 300 dpi