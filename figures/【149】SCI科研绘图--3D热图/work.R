library(rayshader)   # 2D 转 3D 渲染（plot_gg、render_snapshot 等）
library(tidyverse)   # 数据处理和绘图
library(scales)      # 提供 rescale() 等数值工具

# 读取数据，只保留零件数 > 1 的套装
sets <- readr::read_csv('sets.csv') %>% dplyr::filter(num_parts > 1)

# ==================== 绘制 2D 底图 ====================
gg <- ggplot(sets, aes(x = num_parts, y = year)) +   # x=零件数，y=年份
  geom_bin2d(bins = 30, color = NA) +                # 二维分箱热图，30×30 个箱子
  labs(x = NULL, y = NULL) +                         # 不显示轴标签
  scale_x_log10(expand = c(0, 0)) +                  # x 轴 log10 变换，不留空白
  scale_y_continuous(expand = c(0, 0)) +             # y 轴不留空白
  scale_fill_gradientn(                              # 填充色：自定义渐变
    colours = c("#6c98c9","#0A69AE","#328349","#A5BC45",
                "#E4CD9E","#F2CD37","#C91A09"),
    values = scales::rescale(c(0, 0.05, 0.1, 0.2, 0.35, 0.5, 1))
    # 把 0-1 的 values 映射到颜色位置，控制渐变节点
  ) +
  theme_test() +                                     # test 主题
  theme(
    axis.text = element_text(color = "black"),       # 轴文字黑色
    plot.background = element_rect(color = NA, fill = "#ffffff"),   # 图形背景白色
    panel.background = element_rect(color = NA, fill = "#ffffff"),  # 面板背景白色
    plot.margin = margin(t = 0.2, r = 0.2, l = 0.2, b = 0.2, unit = "cm"),  # 边距
    panel.grid.major = element_blank(),              # 不显示主网格线
    panel.grid.minor = element_blank(),              # 不显示次网格线
    legend.title = element_blank(),                  # 图例标题不显示
    legend.text = element_text(size = 8, color = "black")   # 图例文字
  ) +
  guides(fill = guide_colorbar(                      # 图例设置
    direction = "vertical",                          # 垂直方向
    reverse = F,                                     # 不反转
    barwidth = unit(.5, "cm"),                       # 图例条宽
    barheight = unit(10, "cm")                       # 图例条高
  ))

# ==================== 2D 转 3D ====================
rayshader::plot_gg(
  gg,                       # 传入 ggplot 对象
  multicore = TRUE,         # 多核渲染（⚠️ macOS/Windows 上可能不支持）
  shadow_intensity = 0.5,   # 阴影强度
  width = 5,                # 渲染宽度
  height = 5,               # 渲染高度
  scale = 60,               # 高度缩放比例
  preview = TRUE,           # 预览模式（会打开 rgl 窗口）
  raytrace = TRUE,          # 光线追踪（渲染更真实但更慢）
  triangulate = FALSE,      # 不三角化
  offset_edges = TRUE       # 边缘偏移
)

# ==================== 保存图片 ====================
ggsave("图.pdf", width = 8, height = 8, dpi = 300)