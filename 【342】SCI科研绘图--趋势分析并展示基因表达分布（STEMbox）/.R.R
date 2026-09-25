library(readr)                # 读取数据文件
library(dplyr)                # 数据操作
library(ggplot2)              # 绘图
library(stringr)              # 字符串处理
library(magrittr)             # 管道操作符 %<>%
library(purrr)                # 函数式编程，map 等
library(tidyr)                # 数据整理，gather/nest/unnest 等
library(tibble)               # 数据框增强
library(ggpubr)               # 添加显著性标记
library(RColorBrewer)         # 调色板
library(Cairo)                # 高质量图形设备
library(grid)                 # 图形布局

Sys.setenv(LANGUAGE = "en")   # 设置报错信息为英文
options(stringsAsFactors = FALSE)  # 禁止字符型自动转为因子

# 读取数据
exp_rep1 <- read.delim("g27_1.txt", check.names = F, row.names = 1,
                       colClasses = c("SPOT" = "character"))
# 读取重复1的表达数据，第一列作为行名，SPOT列强制为字符型
exp_rep2 <- read.delim("g27_2.txt", check.names = F, row.names = 1,
                       colClasses = c("SPOT" = "character"))
# 读取重复2的表达数据

# 时间点
tp <- colnames(exp_rep1)[2:ncol(exp_rep1)]
# 提取时间点名称，即除第一列外的所有列名

# profile 表
profiletable <- read.delim("defaultsGuilleminSample_profiletable.txt", check.names = F)
# 读取 profile 表，包含 Profile ID、Profile Model、聚类信息等

# gene 表
genetable <- read.delim("defaultsGuilleminSample_genetable.txt", check.names = F)
# 读取基因表，包含基因与 Profile 的对应关系
colnames(genetable)[4:ncol(genetable)] <- as.character(seq(1:length(tp) - 1))
# 将基因表第4列及之后的列名改为 "0", "1", "2", ... 对应时间点

# 合并表达矩阵
exp_all <- rbind(exp_rep1, exp_rep2)
# 将两个重复的表达矩阵按行合并
colnames(exp_all)[2:ncol(exp_all)] <- as.character(seq(1:length(tp) - 1))
# 将表达矩阵第2列及之后的列名改为 "0", "1", "2", ...

# 进入 STEM profile 的 GeneSymbol
GeneSymbolp <- base::intersect(genetable$`Gene Symbol`, exp_all$`Gene Symbol`)
# 取基因表和表达矩阵中共有的基因符号

# 只保留进入 profile 的 Gene Symbol
exp_all <- exp_all[exp_all$`Gene Symbol` %in% GeneSymbolp, ]
# 过滤表达矩阵，只保留在 profile 中的基因
exp_all <- aggregate(. ~ `Gene Symbol`, exp_all, median)  # 多个 SPOT 取中值
# 对同一基因的多个 SPOT 取中位数，合并为一行

# 添加 profile 信息
exp_all.pro <- genetable %>% 
  select(`Gene Symbol`, Profile) %>%   # 从基因表中选择基因符号和 Profile
  right_join(exp_all, by = "Gene Symbol")  # 右连接表达矩阵，保留所有表达基因

# 处理 profiletable：拆分 Profile Model 列
profiletable %<>% 
  separate(col = `Profile Model`, 
           into = as.character(seq(1:length(tp) - 1)), 
           sep = ",", convert = TRUE) %>% 
  # 将 Profile Model 列按逗号拆分为多个列，列名为 "0", "1", "2", ...，并转换为数值
  select(Profile = `Profile ID`, 
         `Cluster (-1 non-significant)`, 
         `# Genes Assigned`, 
         `p-value`, 
         as.character(seq(1:length(tp) - 1)))
# 选择并重命名列：Profile ID 改为 Profile，保留聚类、基因数、p值，以及拆分后的时间点列

# 定义函数：宽转长，并将 x 转为数值
myfun <- function(df) {
  df %<>% 
    gather(key = "x", value = "y", as.character(seq(1:length(tp) - 1))) %>% 
    # 将指定列（"0", "1", ...）从宽格式转为长格式，新列名为 x 和 y
    mutate(x1 = as.numeric(x)) %>%   # 将 x 列转换为数值型
    select(-x) %>%                   # 删除原 x 列
    rename(x = x1)                   # 将 x1 重命名为 x
  return(df)
}

# 保存显著性等列，稍后合并
sig_num <- profiletable %>% 
  select(Profile, `Cluster (-1 non-significant)`, `# Genes Assigned`, `p-value`)
# 提取 Profile、聚类、基因数、p值，用于后续合并

# 关键修改：用 rowwise + list 把 myfun 结果存为列表列
profiletable %<>% 
  select(-`Cluster (-1 non-significant)`, -`# Genes Assigned`, -`p-value`) %>% 
  # 去掉不需要的列，只保留 Profile 和时间点列
  group_by(Profile) %>%              # 按 Profile 分组
  nest() %>%                         # 嵌套为列表列 data
  rowwise() %>%                      # 逐行处理
  mutate(red = list(myfun(data))) %>% # 对每个 data 应用 myfun，结果存入 red 列表列
  ungroup() %>%                      # 取消 rowwise
  select(Profile, red) %>%           # 只保留 Profile 和 red
  left_join(sig_num, by = "Profile") # 左连接回显著性信息

# 查看结果
head(profiletable)

genetable %<>% 
  group_by(Profile) %>%              # 按 Profile 分组
  nest() %>%                         # 嵌套
  rowwise() %>%                      # 逐行
  mutate(grey = list(myfun(data))) %>% # 对每个 data 应用 myfun，存入 grey
  ungroup() %>%                      # 取消 rowwise
  select(Profile, grey)              # 保留 Profile 和 grey

head(genetable)

exp_all.pro %<>% 
  group_by(Profile) %>%              # 按 Profile 分组
  nest() %>%                         # 嵌套
  rowwise() %>%                      # 逐行
  mutate(box = list(myfun(data))) %>% # 对每个 data 应用 myfun，存入 box
  ungroup() %>%                      # 取消 rowwise
  select(Profile, box)               # 保留 Profile 和 box

head(exp_all.pro)


nest <- genetable %>% left_join(profiletable, by = "Profile") %>% 
  left_join(exp_all.pro, by = "Profile") %>%
  arrange(`Cluster (-1 non-significant)`) # 合并三个表，并按聚类列排序

# 将 `Profile` 变量转变成因子，以便分面后一一对应。  
Profile <- nest$Profile %>% as.character()   # 提取 Profile 为字符
Profile_2 <- as.factor(Profile)              # 转为因子
levels(Profile_2) <- Profile                 # 设置因子水平为原始顺序
nest$Profile <- Profile_2 %>% sort()         # 按因子水平排序

nest_part <- nest[nest$`Cluster (-1 non-significant)` != (-1),]
# 过滤掉聚类为 -1（不显著）的 Profile
nrow(nest_part)                              # 查看剩余行数

plot_line <- 
  ggplot(data = nest_part %>% select(Profile, grey, `# Genes Assigned`) %>% unnest()) + 
  # 数据：nest_part 中 grey 列展开，包含所有基因的表达
  geom_line(aes(x = x, y = y, group = `Gene Symbol`), color = "grey") + 
  # 绘制灰色线，每个基因一条线
  geom_line(data = nest_part %>% select(Profile, red, `# Genes Assigned`) %>% unnest(), 
            aes(x = x, y = y), color = "red") + 
  # 绘制红色线，每个 Profile 的典型表达模式
  facet_grid(rows = vars(Profile)) +          # 按 Profile 分面
  scale_x_continuous(breaks = seq(1:length(tp) - 1), 
                     labels = tp) +           # x 轴刻度标签为时间点
  theme(panel.background = element_rect(fill = "white", color = "black"),
        axis.title = element_blank(), 
        axis.text.x = element_text(angle = 45, hjust = .5, vjust = .5),
        strip.background = element_blank(),   # 隐藏分面标题背景
        strip.text = element_blank(),         # 隐藏分面标题文字
        axis.line = element_blank(),
        panel.border = element_rect(linetype = "solid", color = "black", fill = NA)) 
# 主题设置

# 自定义差异性数据，如果你的时间点超过5个，就按排列组合规律继续添加
compaired <- list(
  c("1", "2"), c("1", "3"), c("1", "4"), c("1", "5"),
  c("2", "3"), c("2", "4"), c("2", "5"),
  c("3", "4"), c("3", "5"),
  c("4", "5")
)
# 定义要比较的时间点对，用于箱线图显著性标记

nest_part_new <- nest_part
nest_part_new %<>% select(Profile, box) %>% unnest() 
# 提取 box 列并展开，得到每个基因的表达值
x2 <- as.character(nest_part_new$x) %>% as.factor()
# 将 x 转为字符再转为因子
levels(x2) <- seq(1:length(tp) - 1)
# 设置因子水平为 "0", "1", ...
nest_part_new$x2 <- x2

plot_box <- 
  nest_part_new %>% 
  ggplot(aes(x = x2, y = y, fill = x2, group = x2)) + 
  # 绘制箱线图，x 为时间点因子，y 为表达值，填充按时间点
  geom_boxplot(size = 0.25, 
               outlier.color = NA,       # 隐去异常点
               show.legend = FALSE) +    # 不显示图例
  
  # 标注差异显著性
  stat_compare_means(comparisons = compaired,
                     bracket.size = 0.25, size = 2,
                     label = "p.signif", hide.ns = TRUE, # 不显著不显示
                     symnum.args = list(
                       cutpoints = c(0, 0.001, 0.05, 1), # 显著性阈值
                       symbols = c("**", "*", " "))) +   # 符号
  facet_grid(rows = vars(Profile)) +          # 按 Profile 分面
  scale_x_discrete(breaks = seq(1:length(tp) - 1), 
                   labels = tp) +             # x 轴标签
  scale_fill_brewer(palette = "Set2") +       # 填充配色
  theme(panel.background = element_rect(fill = "white", color = "black"),
        axis.title = element_blank(), 
        axis.text.x = element_text(angle = 45, hjust = .5, vjust = .5),
        axis.line.x.bottom = element_line(color = "black"),
        strip.background = element_blank(),
        strip.text = element_blank(),
        axis.line = element_blank(),
        panel.border = element_rect(linetype = "solid", color = "black", fill = NA)) 


plot_text_1 <- 
  nest_part %>% select(Profile, `# Genes Assigned`) %>% unnest() %>% 
  # 提取 Profile 和基因数
  add_column(., x = rep(1, nrow(.))) %>%   # 添加 x 列，全为 1
  add_column(., y = rep(1, nrow(.))) %>%   # 添加 y 列，全为 1
  ggplot() + 
  geom_text(mapping = aes(x = x, y = x, label = paste0("U", Profile, "\n(", `# Genes Assigned`, ")"))) + 
  # 绘制文本，显示 "U" + Profile + 基因数
  facet_grid(rows = vars(Profile)) +       # 按 Profile 分面
  theme(panel.background = element_blank(),
        axis.title = element_blank(), 
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        strip.background = element_blank(),
        strip.text = element_blank()) 

#plot_text_1

# 折线图的 y 轴 title
plot_text_2 <- nest_part %>% select(Profile, `# Genes Assigned`) %>% unnest() %>% 
  add_column(., x = rep(1, nrow(.))) %>% 
  add_column(., y = rep(1, nrow(.))) %>% 
  ggplot() + 
  geom_text(mapping = aes(x = x, y = x), 
            size = 3, 
            label = expression('Log'[2]*'FC'), angle = 90) + 
  # 绘制 y 轴标题 "Log2FC"，旋转 90 度
  facet_grid(rows = vars(Profile)) + 
  theme(panel.background = element_blank(),
        axis.title = element_blank(), 
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        strip.background = element_blank(),
        strip.text = element_blank()) 

#plot_text_2

# box plot 的 y 轴 title
plot_text_3 <- nest_part %>% select(Profile, `# Genes Assigned`) %>% unnest() %>% 
  add_column(., x = rep(1, nrow(.))) %>% 
  add_column(., y = rep(1, nrow(.))) %>% 
  ggplot() + 
  geom_text(mapping = aes(x = x, y = x), 
            size = 3,
            label = expression('Log'[2]*'(signal)'), angle = 90) + 
  # 绘制 y 轴标题 "Log2(signal)"，旋转 90 度
  facet_grid(rows = vars(Profile)) + 
  theme(panel.background = element_blank(),
        axis.title = element_blank(), 
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        strip.background = element_blank(),
        strip.text = element_blank()) 

myfilter <- function(df) {
  Gene_four <- df$`Gene Symbol` %>% unique() %>%
    sample(size = 4, replace = TRUE) %>%   # 随机抽取 4 个基因（允许重复）
    paste0(collapse = "\n")                # 拼接为长度为 1 的字符串，换行分隔
  return(Gene_four)
}

# 调用函数对每个 Profile 筛选基因
Gene_part <- nest_part %>% select(Profile, grey) %>% 
  mutate(., Gene_four = map(.$grey, myfilter)) %>% 
  # 对每个 grey 数据框应用 myfilter，得到 4 个基因名
  select(Profile, Gene_four) %>% unnest() 
# 展开列表列

plot_text_4 <- Gene_part %>% 
  add_column(., x = rep(1, nrow(.))) %>% 
  add_column(., y = rep(1, nrow(.))) %>% 
  ggplot() + 
  geom_text(mapping = aes(x = x, y = x, label = Gene_four), 
            size = 3) + 
  # 绘制文本，显示随机选取的基因名
  facet_grid(rows = vars(Profile)) + 
  theme(panel.background = element_blank(),
        axis.title = element_blank(), 
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        strip.background = element_blank(),
        strip.text = element_blank()) 


CairoPDF(file = "STEMbox.pdf", width = 6, height = 14)
# 打开 Cairo PDF 设备，输出文件 STEMbox.pdf

grid.newpage() # 新建画布
layout_1 <- grid.layout(nrow = 1, ncol = 6, 
                        widths = c(0.3, 0.2, 1, 0.2, 1, 0.5)) # 设置 6 列，宽度比例
pushViewport(viewport(layout = layout_1)) # 推出视窗
print(plot_text_1, vp = viewport(layout.pos.col = 1)) # 第 1 列：Profile 标签
print(plot_text_2, vp = viewport(layout.pos.col = 2)) # 第 2 列：折线图 y 轴标题
print(plot_line, vp = viewport(layout.pos.col = 3))   # 第 3 列：折线图
print(plot_text_3, vp = viewport(layout.pos.col = 4)) # 第 4 列：箱线图 y 轴标题
print(plot_box, vp = viewport(layout.pos.col = 5))    # 第 5 列：箱线图
print(plot_text_4, vp = viewport(layout.pos.col = 6)) # 第 6 列：基因名文本

dev.off() # 关闭图形设备，保存 PDF