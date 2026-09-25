# ==================== 加载包 ====================
library(tidyverse)     # 数据处理和绘图（含 dplyr、tidyr、ggplot2、readr）
library(readxl)        # 读取 Excel 文件
library(ggsci)         # 提供 jama 等期刊配色
library(patchwork)     # 图形组合（本代码未直接使用）
library(ggnewscale)    # 允许在同一张图中多次使用 scale_*，实现两种配色系统

# ==================== 读取并整理数据 df1 ====================
df1 <- read_excel("41564_2023_1355_MOESM5_ESM.xlsx", sheet = 4) %>%
  # 读取第 4 个工作表
  select(-Abs.cliff, -Cliff.delta, -Trend) %>%   # 删除 3 列不需要的数据
  pivot_longer(-c("id")) %>%                     # 宽转长：除 id 外都转成长表
  mutate(text = case_when(value < 0 ~ "+"))      # value < 0 时标记为 "+"
# 结果：id、name、value、text 四列，text 只有 "+" 或 NA

# ==================== 读取并整理数据 df2 ====================
df2 <- read_excel("41564_2023_1355_MOESM5_ESM.xlsx", sheet = 4) %>%
  select(id, Trend) %>%                          # 只取 id 和 Trend 两列
  mutate(name = "Trend",                         # 新增 name 列，值全为 "Trend"
         value = 0) %>%                          # 新增 value 列，值全为 0
  dplyr::rename(text = "Trend")                  # 把 Trend 列改名为 text
# 结果：id、name、value、text 四列

# ==================== 合并两份数据 ====================
df <- bind_rows(df1, df2)                        # 纵向合并 df1 和 df2

# ==================== 定义 x 轴文字颜色 ====================
x_cols <- rep(c("red", "blue", "black"), times = c(3, 9, 1))
# 前 3 个红色，中间 9 个蓝色，最后 1 个黑色

# ==================== 绘图 ====================
plot <- ggplot() +
  
  # ---- 1. 白色底块 ----
geom_tile(data = df,                           # 全部数据
          aes(name, id),                       # x=name，y=id
          color = "black",                     # 边框黑色
          fill = "white",                      # 填充白色
          size = 0.1) +                        # 边框线宽 0.1
  
  # ---- 2. value < 0 的点（按 value 渐变着色） ----
geom_point(data = df %>% filter(text == "+"),  # 只取 text == "+" 的数据
           aes(name, id,                       # x=name，y=id
               fill = value,                   # 填充按 value 映射
               color = value),                 # 边框按 value 映射
           shape = 22,                         # 形状：22 是方形
           size = 5.7) +                       # 点大小 5.7
  
  # ---- 3. value < 0 的 "+" 文字 ----
geom_text(data = df %>% filter(text == "+"),
          aes(name, id,                        # 位置
              fill = value,                    # 填充（会被覆盖）
              color = value,                   # 颜色
              label = text),                   # 标签为 "+"
          color = "black") +                   # 文字颜色黑色（覆盖前面的 color）
  
  # ---- 4. 渐变配色（RdBu） ----
scale_color_gradientn(colours = RColorBrewer::brewer.pal(11, "RdBu")) +
  scale_fill_gradientn(colours = RColorBrewer::brewer.pal(11, "RdBu")) +
  
  # ---- 5. 开启新的配色系统 ----
new_scale_color() +                            # 重置 color 的 scale
  new_scale_fill() +                             # 重置 fill 的 scale
  
  # ---- 6. 渐变图例设置 ----
guides(color = guide_colorbar(                 # color 图例
  direction = "vertical",                      # 垂直
  reverse = F,                                 # 不反转
  barwidth = unit(.5, "cm"),                   # 图例条宽
  barheight = unit(15, "cm")                   # 图例条高
)) +
  
  # ---- 7. Trend 列的点（按分类着色） ----
geom_point(data = df %>% filter(text != "+"),  # 只取 text != "+" 的数据（即 Trend 列）
           aes(name, id,                       # x=name，y=id
               fill = text,                    # 填充按 text 分类
               color = text),                  # 边框按 text 分类
           shape = 22,                         # 方形
           size = 5.7) +                       # 大小 5.7
  
  # ---- 8. 分类配色（JAMA） ----
scale_fill_jama() +                            # 填充用 JAMA 配色
  scale_color_jama() +                           # 边框用 JAMA 配色
  
  # ---- 9. 主题设置 ----
theme(
  panel.background = element_blank(),          # 面板背景透明
  plot.background = element_blank(),           # 图形背景透明
  axis.ticks = element_blank(),                # 不显示刻度
  axis.title = element_blank(),                # 不显示轴标题
  axis.text.x = element_text(color = x_cols,   # x 轴文字颜色按 x_cols
                             size = 8,         # 字号 8
                             angle = 90,       # 旋转 90 度
                             vjust = 0.5,      # 垂直对齐
                             hjust = 1),       # 水平对齐
  axis.text.y = element_text(color = "black",  # y 轴文字黑色
                             size = 8),        # 字号 8
  legend.background = element_blank(),         # 图例背景透明
  legend.key = element_blank(),                # 图例键背景透明
  legend.spacing.x = unit(0.1, "cm"),          # 图例水平间距
  legend.title = element_blank(),              # 图例标题不显示
  legend.text = element_text(color = "black",  # 图例文字黑色
                             size = 8)         # 字号 8
)

# ==================== 保存图片 ====================
ggsave(plot, file = "热图.pdf",                  # 保存对象和文件名
       width = 4.75, height = 11.84,             # 宽 4.75 英寸，高 11.84 英寸
       unit = "in",                              # 单位：英寸（应为 units）
       dpi = 300)                                # 分辨率 300 dpi