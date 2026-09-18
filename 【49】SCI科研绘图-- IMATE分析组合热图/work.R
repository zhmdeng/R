# ==================== 加载包 ====================
library(tidyverse)      # 数据处理和绘图
library(linkET)         # 提供 qcorrplot()、geom_couple() 等，用于绘制相关性网络
library(RColorBrewer)   # 配色方案
library(ggtext)         # 提供 element_markdown()，支持富文本
library(magrittr)       # 提供管道操作符
library(psych)          # 提供 corr.test()，计算相关性
library(reshape)        # 提供 melt()，宽转长

# ==================== 读取数据 ====================
table1 <- read.delim("env.xls", header = T, sep = "\t",
                     row.names = 1, check.names = F)
# 读取环境因子数据：行=样本，列=环境因子

table2 <- read.delim("genus.xls", header = T, sep = "\t",
                     row.names = 1, check.names = F) %>% 
  t() %>% as.data.frame()
# 读取物种丰度数据：转置后行=样本，列=物种

# ==================== 计算相关性 ====================
pp <- corr.test(table1, table2, method = "pearson", adjust = "fdr")
# 计算环境因子和物种之间的 Pearson 相关性，p 值用 FDR 校正

cor <- pp$r        # 提取相关系数矩阵
pvalue <- pp$p     # 提取 p 值矩阵

# ==================== 数据整理 ====================
df <- melt(cor) %>% 
  mutate(pvalue = melt(pvalue)[, 3],     # 把 p 值矩阵也转长，取第 3 列
         p_signif = symnum(pvalue,        # 根据 p 值生成显著性符号
                           corr = FALSE, na = FALSE,
                           cutpoints = c(0, 0.001, 0.01, 0.05, 0.1, 1),
                           symbols = c("***", "**", "*", "", " "))) %>% 
  set_colnames(c("env", "genus", "r", "p", "p_signif"))
# 结果：env、genus、r、p、p_signif 五列

# ==================== 合并物种注释信息 ====================
cordata <- df %>% 
  left_join(., read_tsv('annotation.xls'), by = c("genus")) %>% 
  # 关联物种的门/纲注释
  select(group, env:p, -genus) %>% 
  # 保留 group、env、r、p
  set_colnames(c("spc", "env", "r", "p")) %>% 
  # 重命名列
  mutate(
    rd = cut(r, breaks = c(-Inf, 0, 0.4, Inf),
             labels = c("< 0", "0 - 0.4", ">= 0.4")),   # 按 r 值分组
    pd = cut(p, breaks = c(-Inf, 0.01, 0.05, Inf),
             labels = c("< 0.01", "0.01 - 0.05", ">= 0.05"))  # 按 p 值分组
  )

# ==================== 绘图 ====================
qcorrplot(correlate(table1, method = "spearman"), diag = F, type = "upper") +
  # 绘制相关性热图（环境因子之间的 Spearman 相关）
  # diag=F：不显示对角线
  # type="upper"：只显示上三角
  geom_tile() +                            # 热图方块
  geom_mark(size = 2.5, sig.thres = 0.05, sep = "\n") +
  # 在热图上标注显著性（sig.thres=0.05）
  geom_couple(aes(colour = pd, size = rd),  # 按 pd 着色、rd 控制线宽
              data = cordata,              # 数据源
              label.colour = "black",      # 标签颜色
              curvature = nice_curvature(0.15),  # 曲线弯曲度
              nudge_x = 0.2,               # x 方向偏移
              label.fontface = 2,          # 标签粗体
              label.size = 4,              # 标签字号
              drop = T) +                  # 丢弃不显著的相关
  scale_fill_gradientn(colours = RColorBrewer::brewer.pal(11, "RdBu")) +
  # 填充色：RdBu 配色
  scale_size_manual(values = c(0.5, 1, 2)) +   # 线宽：3 个等级
  scale_colour_manual(values = c("#D95F02", "#1B9E77", "#A2A2A288")) +
  # 线条颜色：3 个等级
  guides(
    size = guide_legend(title = "cor",         # size 图例标题
                        override.aes = list(colour = "grey35"),
                        order = 2),
    colour = guide_legend(title = "P_value",   # colour 图例标题
                          override.aes = list(size = 3),
                          order = 1),
    fill = guide_colorbar(title = "spearman's r",  # fill 图例标题
                          order = 3)
  ) +
  theme(
    plot.margin = unit(c(0, 0, 0, -1), units = "cm"),  # 图形边距
    panel.background = element_blank(),         # 面板背景透明
    axis.text = element_markdown(color = "black", size = 10),  # 轴文字
    legend.background = element_blank(),        # 图例背景透明
    legend.key = element_blank()                # 图例键背景透明
  )