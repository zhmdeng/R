library(ggplot2)   # 加载 ggplot2，用于绘图
library(dplyr)     # 加载 dplyr，用于数据处理
setwd("")
# ==================== 构造示例数据 ====================
value <- c(24.2, 21.9, 7.6, 5.2, 4.3, 3.2, 2.6, 2.6, 1.8, 1.8, 24.8)
# 各疾病的数值（例如死亡率、占比等）
disease <- c("Heart disease", "Cancer", "injuries", "CPD",
             "Stroke", 'Type2 diabetes', "AD", "Suicide",
             "IP", "Chronic liver disease", "Other")
# 疾病名称
Group <- c("male", "male", "male", "male", "male", "male",
           "male", "male", "male", "male", "male")
# 分组（这里全是 male，没有实际作用）
A <- data.frame(Group, disease, value)   # 构建数据框
A$disease <- factor(A$disease, levels = A$disease)
# 将 disease 转为因子，水平按原始顺序（即数据中出现的顺序）

# ==================== 第一张图：简单堆叠柱状图 ====================
ggplot(A, aes(x = "", y = value, fill = disease)) +
  # x 为空字符串，所有柱子在同一 x 位置，y 为 value，填充按 disease
  geom_bar(width = 1, stat = "identity", color = "white")
# 绘制堆叠柱状图，宽度 1，白色边框，stat="identity" 表示直接用 value 作为高度

# ==================== 计算扇形图所需的百分比和位置 ====================
A <- A %>%
  mutate(prop = value / sum(value)) %>%    # 计算每个疾病占比
  arrange(desc(disease)) %>%               # 按疾病名称降序排序（注意：这改变了行顺序）
  mutate(pos = cumsum(prop) - 0.5 * prop)  # 计算每个扇形在 y 轴上的中心位置

# ⚠️ 注意：arrange(desc(disease)) 按字母降序重排了行，但 A$disease 的因子水平仍然是原始顺序。
# 在 ggplot 中，fill 的堆叠顺序由因子水平决定，而不是数据框的行顺序。
# 因此 pos 的计算顺序与绘图时的堆叠顺序不一致，会导致指示线位置错误。
# 正确做法：不要改变因子水平，或者根据因子水平来排序（例如 arrange(disease) 保持与因子水平一致）。

A$xend <- ifelse(A$prop < 0.2, 2, 1.8)
# 根据占比设置指示线终点的 x 坐标：占比小于 0.2 的标签放得更远（2），否则放 1.8

# ==================== 第二张图：饼图（带标签，geom_label） ====================
ggplot(A, aes(x = "", y = prop, fill = disease)) +
  # 用 prop 作为 y 值，fill 为 disease
  geom_bar(width = 1, stat = "identity", color = "white") +
  # 堆叠柱状图，宽度 1，白色边框
  coord_polar("y", start = 0, clip = "off") +
  # 转为极坐标，start=0 从 12 点方向开始，clip="off" 允许元素画到绘图区域外
  geom_segment(aes(x = 1.5,                # 指示线起点 x 固定为 1.5
                   y = pos,                 # 起点 y 为每个扇形的中心位置
                   xend = xend,             # 终点 x 根据占比变化
                   yend = pos),             # 终点 y 与起点相同（水平线）
               size = 0.5, color = 'black') +   # 线宽 0.5，黑色
  # ⚠️ size 在新版 ggplot2 中已弃用，应改用 linewidth
  
  geom_label(aes(y = pos,                   # 标签位置 y
                 x = xend,                  # 标签位置 x
                 label = paste(disease, scales::percent(prop))),
             # 标签内容：疾病名 + 百分比（用 scales::percent 格式化）
             size = 4, color = "white") +   # 文字大小 4，白色
  # 使用 geom_label 会带一个背景框，文字颜色白色可能在某些背景上看不清
  
  theme(legend.position = "none") +         # 不显示图例
  theme(panel.grid.major = element_blank(), # 不显示主网格线
        panel.grid.minor = element_blank(), # 不显示次网格线
        axis.ticks = element_blank(),       # 不显示刻度
        axis.text.y = element_blank(),      # 不显示 y 轴文字
        axis.text.x = element_blank(),      # 不显示 x 轴文字
        legend.title = element_blank(),     # 不显示图例标题
        panel.border = element_blank(),     # 不显示面板边框
        panel.background = element_blank()) + # 不显示面板背景
  xlab("") + ylab('') +                     # 轴标题为空
  scale_fill_manual(values = c("#aeae5c", "#FB8072", "#1965B0", "#7BAFDE",
                               "#882E72", "#B17BA6", "#FF7F00", "#FDB462",
                               "#E7298A", "#E78AC3", "#33A02C"))
# 手动指定 11 种填充色

# ==================== 第三张图：饼图（带标签，geom_text） ====================
ggplot(A, aes(x = "", y = prop, fill = disease)) +
  geom_bar(width = 1, stat = "identity", color = "white") +
  coord_polar("y", start = 0, clip = "off") +
  geom_segment(aes(x = 1.5,
                   y = pos,
                   xend = xend,
                   yend = pos),
               size = 0.5, color = 'black') +
  geom_text(aes(y = pos,                    # 用 geom_text 代替 geom_label
                x = xend,
                label = paste(disease, scales::percent(prop))),
            size = 4,
            color = "black") +              # 文字黑色，无背景框
  theme(legend.position = "none") +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.ticks = element_blank(),
        axis.text.y = element_blank(),
        axis.text.x = element_blank(),
        legend.title = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank()) +
  xlab("") + ylab('') +
  scale_fill_manual(values = c("#aeae5c", "#FB8072", "#1965B0", "#7BAFDE",
                               "#882E72", "#B17BA6", "#FF7F00", "#FDB462",
                               "#E7298A", "#E78AC3", "#33A02C"))
# 与上一张图基本一致，仅标签几何对象不同

ggsave("图.pdf",width = 5,height = 5,dpi = 300)

