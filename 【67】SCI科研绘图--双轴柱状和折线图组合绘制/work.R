# ==================== 加载包 ====================
library(tidyverse)   # 数据处理和绘图
library(patchwork)   # 图形组合（本代码未直接使用）

# ==================== 读取数据 ====================
gdp <- read_tsv("data.tsv")   # 读取制表符分隔的数据

# ==================== 筛选并整理数据 ====================
df <- gdp %>%
  filter(continent == "Africa") %>%   # 只保留非洲的数据
  select(1, 3, 4, 6) %>%              # 取第 1、3、4、6 列
  mutate(year = as.character(year))   # year 转为字符型（用于离散 x 轴）

# ==================== 绘图 ====================
ggplot() +
  
  # ---- 1. 柱状图：平均预期寿命 ----
stat_summary(data = df,
             aes(year, lifeExp),    # x=年份，y=预期寿命
             fun = "mean",          # 统计函数：均值
             geom = "bar",          # 用柱子显示
             alpha = 0.7,           # 透明度 0.7
             fill = "#00A08A") +    # 填充色：青绿色
  
  # ---- 2. 误差棒：预期寿命的置信区间 ----
stat_summary(data = df,
             aes(year, lifeExp),
             fun.data = "mean_cl_normal",  # 均值 ± 95% 置信区间
             geom = "errorbar",     # 用误差棒显示
             width = .2,            # 误差棒宽度
             color = "#00A08A") +   # 颜色：青绿色
  
  # ---- 3. 误差棒：gdpPercap（缩小 20 倍后画在左轴） ----
stat_summary(data = df %>% mutate(gdpPercap = gdpPercap / 20),
             # 把 gdpPercap 除以 20，让它和 lifeExp 在同一量级
             aes(year, gdpPercap),  # x=年份，y=gdpPercap/20
             fun = mean,            # 统计函数：均值
             geom = "errorbar",     # 用误差棒显示
             width = .2,
             color = "#F98400",     # 橙色
             fun.max = function(x) mean(x) + sd(x) / sqrt(length(x)),  # 上界：均值 + 标准误
             fun.min = function(x) mean(x) - sd(x) / sqrt(length(x)))  # 下界：均值 - 标准误

# ---- 4. 点：gdpPercap 均值 ----
stat_summary(data = df %>% mutate(gdpPercap = gdpPercap / 20),
             aes(year, gdpPercap),
             fun = "mean",          # 均值
             geom = "point",        # 用点显示
             size = 3,              # 点大小
             color = "#F98400") +   # 橙色
  
  # ---- 5. 线：gdpPercap 趋势 ----
stat_summary(data = df %>% mutate(gdpPercap = gdpPercap / 20),
             aes(year, gdpPercap, group = 1),  # group=1 让所有点连成一条线
             fun = "mean",          # 均值
             geom = "line",         # 用线显示
             color = "#F98400") +   # 橙色
  
  # ---- 6. Y 轴设置（双轴） ----
scale_y_continuous(
  expand = c(0, 1),                     # y 轴下方留 1 单位空白
  breaks = scales::pretty_breaks(n = 12),  # 左轴刻度：12 个
  sec.axis = sec_axis(
    ~ . * 20,                           # 右轴 = 左轴 × 20（还原 gdpPercap）
    breaks = scales::pretty_breaks(n = 12),  # 右轴刻度
    name = "gdpPercap"                  # 右轴标题
  )
) +
  
  # ---- 7. 主题设置 ----
theme_test() +                          # 基础主题：test
  theme(
    panel.background = element_blank(),   # 面板背景透明
    
    # ---- 刻度线长度（负值让刻度朝内） ----
    axis.ticks.length.x.bottom = unit(-0.05, "in"),  # x 轴底部刻度朝内
    axis.ticks.length.y.left = unit(-0.05, "in"),    # y 轴左侧刻度朝内
    axis.ticks.length.y.right = unit(-0.05, "in"),   # y 轴右侧刻度朝内
    
    # ---- 刻度线颜色 ----
    axis.ticks.y.right = element_line(color = "#F98400"),  # 右轴：橙色
    axis.ticks.y.left = element_line(color = "#00A08A"),   # 左轴：青绿色
    
    # ---- 轴线颜色 ----
    axis.line.y.left = element_line(color = "#00A08A"),    # 左轴线：青绿色
    axis.line.y.right = element_line(color = "#F98400"),   # 右轴线：橙色
    axis.line.x.bottom = element_line(color = "black"),    # 底轴线：黑色
    axis.line.x.top = element_line(color = "grey80"),      # 顶轴线：灰色
    
    # ---- 轴文字 ----
    axis.text.y.right = element_text(color = "#F98400",    # 右轴文字：橙色
                                     margin = margin(l = 5, r = 10)),  # 边距
    axis.text.y.left = element_text(color = "#00A08A",     # 左轴文字：青绿色
                                    margin = margin(l = 10, r = 5)),   # 边距
    axis.text.x.bottom = element_text(color = "black",     # x 轴文字：黑色
                                      face = "bold",       # 粗体
                                      angle = 90,          # 旋转 90 度
                                      vjust = 0.5),        # 垂直对齐
    
    # ---- 轴标题 ----
    axis.title.y.left = element_text(color = "#00A08A", face = "bold"),  # 左轴标题
    axis.title.y.right = element_text(color = "#F98400", face = "bold"), # 右轴标题
    axis.title.x.bottom = element_blank()  # x 轴标题不显示
  )

# ==================== 保存图片 ====================
ggsave("组合热图.pdf", width = 5, height = 6, dpi = 300)