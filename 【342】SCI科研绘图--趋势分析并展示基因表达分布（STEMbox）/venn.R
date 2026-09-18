library(VennDiagram)        # 加载 VennDiagram 包，用于快速检验数据（画标准韦恩图）
library(colorfulVennPlot)   # 加载 colorfulVennPlot 包，用于绘制彩色韦恩图
library(ggplot2)            # 加载 ggplot2（本代码未直接使用，可能是习惯性加载）
library(dplyr)              # 数据处理
library(magrittr)           # 提供管道操作符
library(readr)              # 读取文本文件
library(purrr)              # 函数式编程（map、map_chr、map_int）
library(RColorBrewer)       # 提供配色方案
library(grDevices)          # 图形设备（颜色、PDF 等）
library(Cairo)              # 提供 CairoPDF()，支持更好的字体渲染
library(stringr)            # 字符串处理（str_pad 等）
library(tibble)             # 提供 tibble 数据框
library(tidyr)              # 数据变形（unnest 等）

Sys.setenv(LANGUAGE = "en")        # 设置 R 报错信息为英文
options(stringsAsFactors = FALSE)  # 禁止字符型自动转为因子

# ==================== 读取数据 ====================
filenames <- paste0("easy_input_", 1:4, ".txt")
# 生成 4 个文件名：easy_input_1.txt 到 easy_input_4.txt

data_ls <- filenames %>%
  map(., ~read_table(.x, col_names = FALSE)) %>%   # 逐个读取文件，不设列名
  map(., ~.x$X1)                                    # 提取第一列（基因列表）
# data_ls 是一个列表，每个元素是一个字符向量（基因名）

names(data_ls) <- paste0("G", 1:length(data_ls))
# 给列表元素命名：G1、G2、G3、G4

str(data_ls)   # 查看列表结构

# ==================== 用 VennDiagram 快速检验 ====================
venn.diagram(x = data_ls, filename = "venn_test.png")
# 用 VennDiagram 画标准韦恩图，检验数据是否正确
# x 是列表，filename 是输出文件名

# ==================== 计算各子区域 ====================
number_area <- 2^length(data_ls) - 1
# 子区域总数：2^4 - 1 = 15（不含“都不属于”的区域）

# 自定义函数：将整数转为二进制向量
intToBin <- function(x){
  if (x == 1)
    1
  else if (x == 0)
    NULL
  else {
    mod <- x %% 2
    c(intToBin((x - mod) %/% 2), mod)   # 递归：先处理高位，再拼接低位
  }
}

x_area <- seq(number_area) %>%          # 生成 1 到 15 的序列
  map(., ~intToBin(.x)) %>%             # 把每个整数转为二进制向量
  map_chr(., ~paste0(.x, collapse = "")) %>%   # 拼接成字符串
  map_chr(., ~str_pad(.x, width = length(data_ls),
                      side = "left", pad = "0"))
# 将二进制字符串左侧补零到长度为 4，例如 "1" → "0001"

# ==================== 计算并集 ====================
G_union <- data_ls$G1 %>%
  union(data_ls$G2) %>%
  union(data_ls$G3) %>%
  union(data_ls$G4)
# 取四个基因列表的并集，得到所有出现过的基因

# ==================== 自定义函数：计算某个子区域的元素 ====================
area_calculate <- function(data_ls, character_area){
  character_num <- 1:4 %>%
    map_chr(., ~substr(character_area, .x, .x)) %>%   # 取子区域字符串的第 i 位
    as.integer() %>%                                   # 转整数
    as.logical()                                       # 转逻辑值（1 表示属于该组）
  
  element_alone <- G_union                             # 从并集开始
  for (i in 1:4) {                                     # 遍历 4 个集合
    element_alone <-
      if (character_num[i]) {
        intersect(element_alone, data_ls[[i]])         # 如果该位为 1，取交集
      } else {
        setdiff(element_alone, data_ls[[i]])           # 如果该位为 0，取差集
      }
  }
  return(element_alone)                                # 返回属于该子区域的基因
}

# ==================== 调用函数，求各子区域的元素 ====================
element_ls <- map(x_area, ~area_calculate(data_ls = data_ls,
                                          character_area = .x))
# element_ls：每个子区域对应的基因列表

# ==================== 计算各子区域元素数量 ====================
quantity_area <- map_int(element_ls, length)
# 每个子区域中基因的数量

# ==================== 计算百分比 ====================
percent_area <- (quantity_area / sum(quantity_area)) %>% round(3)
# 每个子区域数量占总数的比例，保留 3 位小数
percent_area <- (percent_area * 100) %>% paste0("%")
# 转为百分比字符串，如 "12.3%"

# ==================== 生成颜色 ====================
length_pallete <- max(quantity_area) - min(quantity_area) + 1
# 颜色数量：数量最大值与最小值之差 + 1
color_area <- colorRampPalette(brewer.pal(n = 7, name = "YlGn"))(length_pallete)
# 用 YlGn 配色生成渐变颜色（可换成 Blues、Reds 等）

color_tb <- tibble(quantity = seq(min(quantity_area),
                                  max(quantity_area), by = 1),
                   color = color_area)
# 颜色表：每个数量对应一种颜色

# ==================== 整理数据 ====================
nest1 <- tibble(quantity = quantity_area,
                percent = percent_area,
                area = x_area) %>%
  group_by(quantity) %>%          # 按数量分组
  nest() %>%                      # 嵌套
  left_join(color_tb, by = "quantity") %>%  # 关联颜色
  arrange(quantity) %>%           # 按数量排序
  unnest()
# nest1 包含 quantity、percent、area、color 四列

# ==================== 绘制数值版韦恩图 ====================
regions <- nest1$quantity
names(regions) <- nest1$area
# 将数量向量命名为各子区域编号，供 plotVenn4d 使用

CairoPDF(file = "venn_num.pdf", width = 8, height = 6)
# 打开 CairoPDF 设备，输出 8×6 英寸 PDF
plot.new()                        # 新建画布
plotVenn4d(regions,
           Colors = nest1$color,  # 每个子区域的颜色
           Title = "",            # 标题为空
           labels = paste0("G", 1:4))  # 四个集合的标签
dev.off()                         # 关闭设备，保存文件

# ==================== 绘制百分比版韦恩图 ====================
regions <- nest1$percent
names(regions) <- nest1$area
# 将百分比向量命名为各子区域编号

CairoPDF(file = "venn_percent.pdf", width = 8, height = 6)
plot.new()
plotVenn4d(regions,
           Colors = nest1$color,
           Title = "",
           labels = paste0("G", 1:4))
dev.off()