library(tidyverse)    # 数据处理和绘图（含 dplyr、tidyr、ggplot2、readr）
library(ggsignif)     # 添加显著性标注
library(ggsci)        # 提供 NPG 等期刊配色
library(ggprism)      # 提供 GraphPad Prism 风格主题和坐标轴

# ==================== 读取并整理数据 ====================
df <- read_tsv("data.xls") %>%                # 读取制表符分隔的文件
  filter(year %in% c(1957, 2007),             # 只保留 1957 和 2007 年
         continent != "Oceania") %>%          # 排除大洋洲
  select(country, year, lifeExp, continent) %>%  # 只保留四列
  mutate(paired = rep(1:(n() / 2), each = 2),    # 两两配对编号
         # ⚠️ 这里假设数据按国家严格两两排列（同一国家 1957、2007 相邻）
         year = factor(year))                 # year 转因子（离散 x 轴）

# ==================== 绘图 ====================
df %>%
  ggplot(aes(year, lifeExp)) +                # x = year，y = lifeExp
  
  # ---- 1. 箱线图的须（误差棒） ----
stat_boxplot(geom = "errorbar",             # 用 stat_boxplot 计算四分位数并画须
             position = position_dodge(width = 0.2),  # 与箱体对齐
             width = 0.1) +                 # 须的宽度 0.1
  
  # ---- 2. 箱线图 ----
geom_boxplot(position = position_dodge(width = 0.2),  # 躲开宽度 0.2
             width = 0.4) +                 # 箱体宽度 0.4
  
  # ---- 3. 配对连线 ----
geom_line(aes(group = paired),              # 按 paired 分组，连接同一国家的两个年份
          position = position_dodge(0.2),   # 与点对齐
          color = "grey80") +               # 灰色
  
  # ---- 4. 散点 ----
geom_point(aes(fill = year,                 # 填充按 year
               group = paired,              # 分组按 paired
               size = lifeExp,              # 点大小按 lifeExp
               alpha = lifeExp),            # 透明度按 lifeExp
           pch = 21,                        # 形状 21（带填充的圆）
           position = position_dodge(0.2)) +  # 躲开
  scale_size_continuous(range = c(1, 3)) +    # 点大小范围 1~3
  
  # ---- 5. 显著性标注 ----
geom_signif(comparisons = list(c("1957", "2007")),  # 比较 1957 与 2007
            map_signif_level = T,           # 自动显示 ***/**/* 
            vjust = 0.5,                    # 垂直位置
            color = "black",                # 颜色
            textsize = 5,                   # 文字大小
            test = wilcox.test,             # Wilcoxon 检验
            step_increase = 0.1) +          # 标注阶梯递增
  
  # ---- 6. 分面 ----
facet_wrap(. ~ continent, nrow = 1) +       # 按 continent 分面，一行排列
  
  # ---- 7. 配色 ----
scale_fill_npg() +                          # 填充色使用 NPG 配色
  
  # ---- 8. 坐标轴 ----
scale_x_discrete(guide = "prism_bracket") +  # x 轴用 Prism 风格的括号
  scale_y_continuous(limits = c(0, 90),        # y 轴范围 0~90
                     minor_breaks = seq(0, 90, 5),  # 次刻度间隔 5
                     guide = "prism_offset_minor") +  # Prism 风格次刻度
  
  # ---- 9. 标签 ----
labs(x = NULL, y = NULL) +                  # 不显示轴标签
  
  # ---- 10. 主题 ----
theme_prism(base_line_size = 0.5) +         # Prism 主题，基础线宽 0.5
  theme(
    plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), units = "cm"),
    # ❌ 原代码写的是 units = ,"cm"，中间多了个逗号，会直接报语法错误
    axis.line = element_line(color = "black", size = 0.4),      # 轴线
    panel.grid.minor = element_blank(),                          # 不显示次网格线
    panel.grid.major = element_line(size = 0.2, color = "#e5e5e5"),  # 主网格线
    axis.text.y = element_text(color = "black", size = 10),      # y 轴文字
    axis.text.x = element_text(margin = margin(t = -5),          # x 轴文字上边距（负值让文字靠近轴）
                               color = "black", size = 10),
    legend.position = "none",                                    # 不显示图例
    panel.spacing = unit(0, "lines")                             # 分面间距 0
  ) +
  coord_cartesian()                           # 无参数调用，可省略

# ==================== 保存图片 ====================
ggsave("图.pdf", width = 8, height = 6, dpi = 300)