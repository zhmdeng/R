library(tidyverse)                 # 加载 tidyverse（含 ggplot2、dplyr、readr 等）

data <- read_tsv("data.xls")       # 读取制表符分隔的数据
# 数据应包含：country（国家名）、diff（差值，用于排序和位置）、balance（分组，用于着色）

# ==================== 绘图 ====================
ggplot(data, aes(y = reorder(country, diff),   # y 轴为国家，按 diff 升序排列
                 x = diff,                     # x 轴为差值
                 color = balance)) +           # 颜色按 balance 映射
  
  # ---- 1. 主棒棒糖线段：从原点延伸到各点 ----
geom_segment(aes(yend = country),            # 起点 y=country，终点 y=country（水平线）
             xend = 0,                        # 线段终点 x=0
             size = 5) +                      # 线宽（新版本 ggplot2 建议用 linewidth）
  # ---- 2. 主端点 ----
geom_point(size = 4.5) +                      # 每个国家的数据点
  # ---- 3. 零参考线 ----
geom_vline(xintercept = 0,                    # x=0 处的垂直线
           size = 1, color = 'grey35') +
  # ---- 4. 扩展 x 轴范围 ----
expand_limits(x = c(-3, 3.75)) +              # 保证 x 轴范围够宽，容纳标签和图例
  # ---- 5. x 轴刻度 ----
scale_x_continuous(breaks = c(-2, -1, 1, 2)) +  # 只显示这几个刻度
  # ---- 6. 负值国家的文字标签（左侧） ----
geom_text(data = data %>% filter(diff < 0),   # 只取 diff < 0 的行
          aes(label = country),                # 标签为国家名
          x = -3.2,                            # x 坐标固定在 -3.2
          hjust = 0,                           # 左对齐
          size = 3.5, color = "black") +
  # ---- 7. 正值国家的文字标签（右侧） ----
geom_text(data = data %>% filter(diff > 0),
          aes(label = country),
          x = 2.2, hjust = 0, size = 3.5, color = "black") +
  # ---- 8. 负值国家的引导线段（从 0 指向左侧标签） ----
geom_segment(data = data %>% filter(diff < 0),
             aes(yend = country),
             xend = -3.2,                     # 终点在 x=-3.2
             x = 0,                           # 起点在 x=0
             alpha = 0.2, size = 5) +
  # ---- 9. 正值国家的引导线段（从 0 指向右侧标签） ----
geom_segment(data = data %>% filter(diff > 0),
             aes(yend = country),
             xend = 4.5, x = 0,
             alpha = 0.2, size = 5) +
  # ---- 10. 颜色映射 ----
scale_color_manual(values = c("#BA7A70", "steelblue4")) +  # 两个分组的颜色
  # ---- 11. 关闭图例 ----
guides(color = "none", y = "none") +          # 不显示 color 和 y 的图例
  # ---- 12. 轴标签 ----
labs(x = NULL, y = NULL) +                    # 不显示轴标签
  # ---- 13. 主题 ----
theme_minimal() +                             # 极简主题（白色背景）
  theme(
    plot.background = element_rect(fill = "Aliceblue", color = "Aliceblue"),
    # 图形背景设为 Aliceblue
    panel.grid = element_blank(),               # 不显示网格
    axis.line.x = element_line(color = "grey3", size = 0.5),   # x 轴黑色细线
    panel.grid.major.x = element_line(linetype = "dashed", color = "grey3"),
    # x 方向主网格线为灰色虚线
    axis.text.x = element_text(size = 10, color = "black")     # x 轴文字
  ) +
  # ---- 14. 在右下角添加自定义图例框 ----
geom_rect(xmax = 3.8, xmin = 2, ymin = 0, ymax = 3,          # 矩形范围
          fill = "aliceblue", color = "steelblue4") +        # 填充和边框色
  # ---- 15. 图例框内：More 示例 ----
geom_segment(y = 1, yend = 1, x = 2.6, xend = 3.6,           # 短线
             size = 5, alpha = 0.5, color = 'steelblue4') +
  geom_point(y = 1, x = 2.6, size = 4.5, color = "steelblue4") +  # 端点
  geom_text(label = "More", y = 1, x = 2.1,                    # 文字
            hjust = 0, color = "steelblue4", size = 3.5) +
  # ---- 16. 图例框内：Less 示例 ----
geom_segment(y = 2.3, yend = 2.3, x = 2.6, xend = 3.6,
             size = 5, alpha = 0.5, color = '#BA7A70') +
  geom_point(y = 2.3, x = 2.6, size = 4.5, color = "#BA7A70") +
  geom_text(label = "Less", y = 2.3, x = 2.1,
            hjust = 0, color = "#BA7A70", size = 3.5)

# ==================== 保存 ====================
ggsave("图.pdf", width = 8, height = 6, dpi = 300)