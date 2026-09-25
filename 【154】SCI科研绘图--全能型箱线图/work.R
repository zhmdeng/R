library(tidyverse)    # 数据处理和绘图（含 dplyr、tidyr、ggplot2、readr）
library(gapminder)    # 提供 gapminder 数据集
library(ggsci)        # 提供 NPG 等期刊配色
library(ggprism)      # Prism 风格主题和坐标轴
library(rstatix)      # 提供 wilcox_test()、adjust_pvalue() 等管道友好的统计函数
library(ggpubr)       # 提供 stat_pvalue_manual()、stat_cor() 等
library(ggpmisc)      # 提供 stat_regline_equation() 等

# ==================== 整理数据 ====================
df <- gapminder %>%
  filter(year %in% c(1957, 2002, 2007),        # 只保留 1957、2002、2007 年
         continent != "Oceania") %>%           # 排除大洋洲
  select(country, year, lifeExp, continent) %>% # 只保留四列
  mutate(paired = rep(1:(n() / 3), each = 3),   # 每 3 行配对（因为每年一个国家有 3 个年份）
         # ⚠️ 这里假设数据按国家严格每 3 行一组排列。如果数据排序不同，配对会错
         year = factor(year))                   # year 转因子（离散 x 轴）

# ==================== 组内多重比较 ====================
df_p_val1 <- df %>%
  group_by(continent) %>%                       # 按大陆分组
  wilcox_test(lifeExp ~ year) %>%               # 对每个大陆做 Wilcoxon 检验（年份两两比较）
  adjust_pvalue(p.col = "p", method = "bonferroni") %>%  # 用 Bonferroni 校正 p 值
  add_significance(p.col = "p.adj") %>%         # 添加显著性符号（***、**、*、ns）
  add_xy_position(x = "year", dodge = 0.8)      # 计算显著性标注的位置，dodge=0.8 与箱线图对齐

# ==================== 绘图 ====================
df %>%
  ggplot(aes(year, lifeExp)) +                  # x=year，y=lifeExp
  # ---- 1. 箱线图的须 ----
stat_boxplot(geom = "errorbar",               # 用 stat_boxplot 计算四分位数并画须
             position = position_dodge(width = 0.2),  # 与箱体对齐
             width = 0.1) +                   # 须的宽度
  # ---- 2. 箱线图 ----
geom_boxplot(position = position_dodge(width = 0.2),  # 躲开宽度 0.2
             width = 0.4) +                   # 箱体宽度
  # ---- 3. 配对连线（已注释掉） ----
# geom_line(aes(group = paired), position = position_dodge(0.2), color = "grey80") +
# ---- 4. 散点 ----
geom_point(aes(fill = year,                   # 填充按 year
               group = paired,                # 分组按 paired
               size = lifeExp,                # 点大小按 lifeExp
               alpha = lifeExp),              # 透明度按 lifeExp
           pch = 21,                          # 形状 21（带填充的圆）
           position = position_dodge(0.2)) +  # 躲开
  # ---- 5. 显著性标注 ----
stat_pvalue_manual(df_p_val1,                 # 检验结果
                   label = "p.adj.signif",    # 标签为校正后的显著性符号
                   label.size = 5,            # 文字大小
                   hide.ns = F) +             # 不隐藏不显著的结果
  scale_size_continuous(range = c(1, 3)) +      # 点大小范围 1~3
  # ---- 6. 线性回归拟合线 ----
geom_smooth(method = "lm",                    # 线性回归
            formula = NULL,                   # ⚠️ formula=NULL 可能无效，建议写成 formula = y ~ x
            size = 1,                         # ⚠️ 新版 ggplot2 中 size 已弃用，改为 linewidth
            se = T,                           # 显示置信区间
            color = "black",                  # 黑色
            linetype = "dashed",              # 虚线
            aes(group = 1)) +                 # group=1 让所有点连成一条线
  # ---- 7. 相关性标注 ----
stat_cor(label.y = 25,                        # 相关性标签的 y 位置
         aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~"),  # ⚠️ ..rr.label.. 是老语法，新版要用 after_stat()
             group = 1),                      # 分组
         color = "black",                     # 颜色
         label.x.npc = "left") +              # x 位置（相对坐标）
  # ---- 8. 回归方程 ----
stat_regline_equation(label.y = 19,           # 方程标签的 y 位置
                      aes(group = 1),          # 分组
                      color = "red") +         # 红色
  # ---- 9. 分面 ----
facet_wrap(. ~ continent, nrow = 1) +         # 按 continent 分面，一行排列
  # ---- 10. 配色 ----
scale_fill_npg() +                            # NPG 配色
  # ---- 11. 坐标轴 ----
scale_x_discrete(guide = "prism_bracket") +   # x 轴用 Prism 风格括号
  scale_y_continuous(limits = c(0, 95),         # y 轴范围 0~95
                     minor_breaks = seq(0, 95, 5),  # 次刻度
                     guide = "prism_offset_minor") +  # Prism 风格
  # ---- 12. 标签 ----
labs(x = NULL, y = NULL) +                    # 不显示轴标签
  # ---- 13. 主题 ----
theme_prism(base_line_size = 0.5) +           # Prism 主题
  theme(
    plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), units = "cm"),
    # ❌ 原代码写的是 units = ,"cm"，多了个逗号，会直接报语法错误
    strip.text = element_text(size = 12),       # 分面文字大小
    axis.line = element_line(color = "black", size = 0.4),      # 轴线
    panel.grid.minor = element_blank(),                          # 不显示次网格线
    panel.grid.major = element_line(size = 0.2, color = "#e5e5e5"),  # 主网格线
    axis.text.y = element_text(color = "black", size = 10),      # y 轴文字
    axis.text.x = element_text(margin = margin(t = -5),          # x 轴文字上边距
                               color = "black", size = 10),
    legend.position = "none",                                    # 不显示图例
    panel.spacing = unit(0, "lines")                             # 分面间距 0
  ) +
  coord_cartesian()                           # 无参数调用，可省略

# ==================== 保存 ====================
ggsave("图.pdf", width = 8, height = 6, dpi = 300)