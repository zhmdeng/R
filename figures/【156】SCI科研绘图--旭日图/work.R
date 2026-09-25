# ========== 加载所需的包 ==========
library(tidyverse)      # 数据处理和绘图
library(geomtextpath)   # 提供 geom_textpath()，让文字沿路径排列
library(ggsci)          # 提供 JCO、NPG 等期刊配色
library(showtext)       # 让 R 支持系统字体（绕过 PDF 字体限制）

# ========== 加载数据 ==========
load("da.Rdata")   # 加载包含 houses、parts 等数据对象的 RData 文件

# ========== 注册系统字体 ==========
font_add("Gill Sans", "/System/Library/Fonts/GillSans.ttc")
# 把 Gill Sans 字体注册到 R 中，命名为 "Gill Sans"
# 注意：这里假设 GillSans.ttc 在当前工作目录，或路径正确
# macOS 上更稳妥的写法是：font_add("Gill Sans", regular = "/System/Library/Fonts/GillSans.ttc")

# ========== 开启 showtext ==========
showtext_auto()
# 开启 showtext，之后绘图时 R 就会用注册的系统字体渲染文字
# 这解决了 PDF 设备不支持系统字体导致的 "invalid font type" 错误

# ========== 绘制图形 ==========
ggplot() +
  # ---- 第 1 层：白色背景矩形（内圈空白） ----
geom_rect(data = data.frame(xmin = 0, xmax = 1, ymin = 0, ymax = .75),
          mapping = aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
          fill = "white") +
  # 在中心画一个白色矩形，作为内圈空白区域
  
  # ---- 第 2 层：内圈环（houses） ----
geom_rect(data = houses,
          mapping = aes(xmin = start_lim, xmax = end_lim,
                        ymin = 1, ymax = 1.75, fill = fill_col),
          color = "white") +
  # 内圈环：用矩形填充，颜色按 fill_col 映射，白色边框分隔
  
  # ---- 第 3 层：外圈环（parts） ----
geom_rect(data = parts,
          mapping = aes(xmin = start_lim, xmax = end_lim,
                        ymin = 1.90, ymax = 2.75, fill = fill_col),
          color = "white") +
  # 外圈环：用矩形填充，颜色按 fill_col 映射，白色边框分隔
  
  # ---- 内圈文字：黑色描边（先画黑色底层） ----
geom_textpath(data = houses,
              mapping = aes(x = mid_pt, y = 1.35, label = toupper(speaker_type)),
              color = "black", size = 3,
              text_only = TRUE, family = "Gill Sans") +
  # 在内圈画黑色文字，作为描边底层
  
  # ---- 内圈文字：白色填充（再画白色上层） ----
geom_textpath(data = houses %>% filter(speaker_type != "Neutral"),
              mapping = aes(x = mid_pt, y = 1.35, label = toupper(speaker_type)),
              upright = FALSE, color = "white", size = 3,
              text_only = TRUE, family = "Gill Sans") +
  # 非 Neutral 部分再画白色文字，形成描边效果
  
  # ---- 外圈文字：非 Neutral 部分（白色） ----
geom_textpath(data = parts %>% filter(speaker_type != "Neutral" & perc >= 0.02),
              mapping = aes(x = mid_pt, y = 2.35,
                            label = paste0(str_replace(speaker, " ", "\n"),
                                           "\n", round(perc * 100, 1), "%")),
              color = "white", size = 3, text_only = TRUE) +
  # 外圈非 Neutral 部分显示白色文字（百分比 ≥ 2% 才显示，避免重叠）
  
  # ---- 外圈文字：Neutral 部分（黑色） ----
geom_textpath(data = parts %>% filter(speaker_type == "Neutral" & perc >= 0.025),
              mapping = aes(x = mid_pt, y = 2.35,
                            label = paste0(str_replace(speaker, " ", "\n"),
                                           "\n", round(perc * 100, 1), "%")),
              color = "black", size = 3, text_only = TRUE) +
  # 外圈 Neutral 部分显示黑色文字（百分比 ≥ 2.5% 才显示）
  
  # ---- 配色方案 ----
scale_fill_jco() +    # 填充色使用 JCO 期刊配色
  scale_color_jco() +   # 边框色使用 JCO 期刊配色
  
  # ---- 极坐标转换（把矩形变成环形） ----
coord_polar() +
  # 把直角坐标转为极坐标，矩形就变成环形
  
  # ---- 主题设置 ----
theme(
  legend.position = "none",                          # 不显示图例
  panel.grid = element_blank(),                      # 不显示网格线
  axis.title = element_blank(),                      # 不显示轴标题
  axis.ticks = element_blank(),                      # 不显示轴刻度
  axis.text = element_blank(),                       # 不显示轴文字
  panel.background = element_rect(fill = "white")    # 面板背景白色
)

# ========== 保存图片 ==========
ggsave("plot.pdf", width = 6, height = 6, dpi = 600, device = cairo_pdf)
# 保存为 PDF，6×6 英寸，600 dpi