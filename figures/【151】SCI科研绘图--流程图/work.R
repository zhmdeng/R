library(tidyverse)      # 数据处理和绘图
library(igraph)         # 网络图/树图数据结构与布局算法
library(showtext)       # 支持系统字体
library(thematic)       # 提供配色方案（本段用到了 okabe_ito）
# install.packages("thematic")   # 安装 thematic（已注释）

# ==================== 构造决策树数据 ====================
dat <- tribble(         # tribble() 手动创建 tibble，按行输入
  ~from, ~to,           # 两列：from（起点）和 to（终点）
  'Are you a horse?', 'No',
  'Are you a horse?', 'Yes',
  'Are you a horse?', 'Maybe',
  'Maybe', 'How many legs\ndo you walk on?',   # \n 用于文字换行
  'Yes', 'How many legs\ndo you walk on?',
  'No', 'You\'re not a horse',                 # \' 转义单引号
  'How many legs\ndo you walk on?', 'Two',
  'How many legs\ndo you walk on?', 'Four',
  'Two', 'You\'re not a horse_2',
  'Four', 'Really?',
  'Really?', 'No_2',
  'Really?', 'Yes_2',
  'No_2', 'Can you read\nand write?',
  'Yes_2', 'Can you read\nand write?',
  'Can you read\nand write?', 'Yes_3',
  'Can you read\nand write?', 'No_3',
  'Yes_3', 'You\'re not a horse_3',
  'No_3', 'You\'re reading this,\naren\'t you?',
  'You\'re reading this,\naren\'t you?', 'Yes_4',
  'Yes_4', 'You\'re not a horse_4'
)

# ==================== 创建图对象与布局 ====================
graph <- graph_from_data_frame(dat, directed = TRUE)
# 用 from-to 数据框创建有向图

coords <- graph %>%
  layout_as_tree() %>%                    # 树形布局，返回 n×2 矩阵
  as_tibble(.name_repair = ~c('x', 'y'))  # 转为 tibble，并强制列名为 x、y
# .name_repair 用公式指定列名，避免自动命名为 V1、V2

output <- coords %>%
  mutate(
    step = vertex_attr(graph, 'name'),    # 节点名称（对应 from/to 里的值）
    label = str_remove(step, '\\_.+'),    # 去掉 "_数字" 后缀，用于显示
    x = -2.5 * x,                         # x 坐标缩放（放大间距）
    y = 5 * y,                            # y 坐标缩放
    type = case_when(
      str_detect(label, '\\?') ~ "Question",                    # 含 ? 的为问题节点
      str_detect(step, 'You\'re not a horse') ~ 'Outcome',      # 含 "You're not a horse" 的为结果节点
      T ~ 'Answer'                                              # 其余为答案节点
    )
  )

# ==================== 计算矩形边界 ====================
box_width <- 1.2      # 矩形半宽
box_height <- 1.25    # 矩形半高

boxes <- output %>%
  mutate(
    xmin = x - box_width,               # 左边界
    xmax = x + box_width,               # 右边界
    ymin = case_when(                   # 下边界：长文本节点用更高的矩形
      str_detect(step, '(legs|reading|write)') ~ y - 1.5 * box_height,
      T ~ y - box_height
    ),
    ymax = case_when(                   # 上边界
      str_detect(step, '(legs|reading|write)') ~ y + 1.5 * box_height,
      T ~ y + box_height
    )
  )

# ==================== 构造边数据 ====================
edges <- dat %>%
  mutate(id = row_number()) %>%         # 给每条边编号
  pivot_longer(cols = c("from", "to"),  # 把 from 和 to 两列转成长表
               names_to = "s_e",        # 新列名：s_e（起点或终点）
               values_to = "step") %>%
  left_join(boxes, by = "step") %>%     # 关联节点的位置信息
  select(-c(label, type, y, xmin, xmax)) %>%   # 删除不需要的列
  mutate(y = ifelse(s_e == "from", ymin, ymax)) %>%
  # 起点用矩形下边界，终点用上边界
  select(-c(ymin, ymax)) %>%
  mutate(x = case_when(                 # 对特定边调整 x 位置（避免连线重叠）
    s_e == 'to' & id %in% c(5, 14) ~ x - box_width,
    T ~ x
  ))

# ==================== 配色 ====================
base_colors <- thematic::okabe_ito(2)
# ⚠️ 这里可能报错：okabe_ito() 实际上来自 colorblindr 或 scico 包，不在 thematic 里
# 如果报 "could not find function"，可以改成：
# install.packages("colorblindr")  # 或
# base_colors <- c("#0072B2", "#D55E00")   # 手动指定 Okabe-Ito 两种色

# ==================== 绘图 ====================
ggplot() +
  # ---- 1. 绘制带箭头的路径 ----
geom_path(data = edges, aes(x, y, group = id),
          arrow = arrow(length = unit(0.25, 'cm'))) +
  # geom_path 按 group 连接点，形成从起点到终点的线
  
  # ---- 2. 绘制矩形框 ----
geom_rect(data = boxes,
          aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, fill = type)) +
  
  # ---- 3. 绘制节点文字 ----
geom_text(data = boxes, aes(x = x, y = y, label = label),
          lineheight = 1) +           # 行距
  
  # ---- 4. 主题 ----
theme_void() +                        # 空白主题
  theme(
    legend.position = 'none',           # 不显示图例
    plot.background = element_rect(fill = 'white', colour = NA)  # 白色背景
  ) +
  
  # ---- 5. 填充色 ----
scale_fill_manual(values = c(
  'Question' = base_colors[1],                                   # 问题节点
  'Answer' = colorspace::lighten(base_colors[1], 0.5),           # 答案节点：淡化
  'Outcome' = colorspace::lighten(base_colors[2], 0.1)           # 结果节点
)) +
  
  # ---- 6. 坐标范围 ----
coord_cartesian(xlim = c(-4.5, 5))    # 限定 x 轴显示范围

# ==================== 保存图片 ====================
ggsave("图.pdf", width = 9, height = 5, dpi = 300)