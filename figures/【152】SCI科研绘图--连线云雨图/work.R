# 加载包
library(tidyverse)    # 数据处理和绘图核心包（含 dplyr、tidyr、ggplot2、readr、purrr 等）
library(ggsignif)     # 添加显著性标注
library(gghalves)     # 提供 geom_half_violin() 绘制半小提琴图
library(ggsci)        # 提供 NPG 等期刊配色

# ==================== 第一段：整体绘图（分面显示所有大陆） ====================
df <- read_tsv("data.xls") %>%                # 读取制表符分隔的文件
  filter(year %in% c(1957, 2007),             # 只保留 1957 和 2007 年
         continent != "Oceania") %>%          # 排除大洋洲
  select(country, year, lifeExp, continent) %>%  # 只保留这四列
  mutate(paired = rep(1:(n()/2), each = 2),   # 创建配对 ID：假设每两个连续行是同一个国家的两个年份
         year = factor(year))                 # 将 year 转为因子（用于离散 x 轴）

df %>%
  ggplot(aes(year, lifeExp)) +                # x = year，y = lifeExp
  geom_half_violin(aes(split = year),         # 半小提琴图：按 year 拆分左右
                   side = 2,                  # side = 2 表示显示右侧半小提琴
                   alpha = 0.8) +             # 透明度 0.8
  stat_boxplot(geom = "errorbar",             # 用 stat_boxplot 计算四分位数并画误差棒（箱线图的须）
               width = 0.1) +                 # 须的宽度 0.1
  geom_boxplot(width = 0.2) +                 # 画箱线图，宽度 0.2
  geom_line(aes(group = paired),              # 按 paired 分组画连线（连接同一个国家的两个年份）
            color = "grey80") +               # 灰色
  geom_point(aes(fill = year,                 # 填充色按 year 映射
                 group = paired,              # 分组按 paired
                 size = lifeExp,              # 点大小按 lifeExp
                 alpha = lifeExp),            # 透明度按 lifeExp
             pch = 21,                        # 形状 21（带填充的圆）
             position = position_dodge(0.2)) +  # 躲避宽度 0.2
  scale_size_continuous(range = c(1, 3)) +    # 点大小范围 1~3
  geom_signif(comparisons = list(c("1957", "2007")),  # 比较 1957 与 2007
              map_signif_level = T,           # 自动显示显著性符号（***、**、*）
              vjust = 0.5,                    # 垂直位置调整
              color = "black",                # 颜色黑色
              textsize = 5,                   # 文字大小 5
              test = wilcox.test,             # 使用 Wilcoxon 检验
              step_increase = 0.1) +          # 显著性标注阶梯递增
  facet_wrap(. ~ continent, nrow = 1) +       # 按 continent 分面，一行排列
  scale_fill_npg() +                          # 填充色使用 NPG 配色
  scale_y_continuous(limits = c(0, 90),       # y 轴范围 0~90
                     minor_breaks = seq(0, 90, 5)) +  # 次刻度间隔 5
  labs(x = NULL, y = NULL) +                  # 不显示轴标签
  theme_test() +                              # 使用 test 主题
  theme(
    plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), units = "cm"),  # ❌ 原代码为 units = ,"cm"，多了逗号，正确应为 units = "cm"
    axis.line = element_line(color = "black", size = 0.4),    # 轴线
    panel.grid.minor = element_blank(),                        # 不显示次网格线
    panel.grid.major = element_line(size = 0.2, color = "#e5e5e5"),  # 主网格线
    axis.text.y = element_text(color = "black", size = 10),    # y 轴文字
    axis.text.x = element_text(margin = margin(t = 2),         # x 轴文字边距
                               color = "black", size = 10),
    legend.position = "none",                                  # 不显示图例
    panel.spacing = unit(0, "lines")                           # 分面间距 0
  ) +
  coord_cartesian()                           # 保持坐标范围（可省略）

# ==================== 第二段：循环为每个大陆单独绘图并保存 ====================
df <- read_tsv("data.xls") %>%                # 重新读取数据
  filter(year %in% c(1957, 2007),             # 同样筛选
         continent != "Oceania") %>%
  select(country, year, lifeExp, continent) %>%
  mutate(paired = rep(1:(n()/2), each = 2),   # 配对 ID
         year = factor(year))                 # year 转因子

continents <- unique(df$continent)            # 提取所有唯一的大陆名

plots <- map(continents, function(continent) {  # 用 map 遍历每个大陆
  df %>%
    filter(continent == continent) %>%       # 只保留当前大陆的数据
    ggplot(aes(year, lifeExp)) +             # 以下绘图代码与上面基本相同
    geom_half_violin(aes(split = year), side = 2, alpha = 0.8) +
    stat_boxplot(geom = "errorbar", width = 0.1) +
    geom_boxplot(width = 0.2) +
    geom_line(aes(group = paired), color = "grey80") +
    geom_point(aes(fill = year, group = paired, size = lifeExp, alpha = lifeExp),
               pch = 21, position = position_dodge(0.2)) +
    scale_size_continuous(range = c(1, 3)) +
    geom_signif(comparisons = list(c("1957", "2007")),
                map_signif_level = T, vjust = 0.5, color = "black",
                textsize = 5, test = wilcox.test, step_increase = 0.1) +
    scale_fill_npg() +
    scale_y_continuous(limits = c(0, 90), minor_breaks = seq(0, 90, 5)) +
    labs(x = NULL, y = NULL) +
    theme_test() +
    theme(
      plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), units = "cm"),  # ❌ 同样错误：units = ,"cm"
      axis.line = element_line(color = "black", size = 0.4),
      panel.grid.minor = element_blank(),
      panel.grid.major = element_line(size = 0.2, color = "#e5e5e5"),
      axis.text.y = element_text(color = "black", size = 10),
      axis.text.x = element_text(margin = margin(t = 2), color = "black", size = 10),
      legend.position = "none",
      panel.spacing = unit(0, "lines")
    ) +
    coord_cartesian()
})

# 保存每个大陆的图
map2(plots, continents, function(plot, continent) {
  ggsave(plot, filename = paste0("plot_", continent, ".pdf"))
  # ❌ ggsave 的第一个参数是 filename，第二个才是 plot。
  # 正确写法：ggsave(filename = paste0("plot_", continent, ".pdf"), plot = plot)
})

# 最后这行保存的是当前设备中的图形，但前面 map2 并没有保留图形对象，
# 所以这里可能保存空白或报错。建议删除，或改为保存某个特定图。
ggsave("图.pdf", width = 8, height = 6, dpi = 300)