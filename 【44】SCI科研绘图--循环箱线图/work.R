library(tidyverse)   # 数据处理和绘图（含 ggplot2、dplyr、readr、purrr 等）
library(ggsci)       # 提供 NPG 等期刊配色
library(purrr)       # 函数式编程，提供 map、map2 等

# 设置工作目录
setwd("/Users/mac/Desktop/PowerBI/sci科研绘图/【44】SCI科研绘图--循环箱线图/")

# 读取数据，并去掉 gene_id 为 H2BC4 的行
df <- read_tsv("data.txt") %>% filter(gene_id != "H2BC4")

# ==================== 第一段：一次性画出所有基因的分面图 ====================
df %>%
  ggplot(aes(WHO_temp_severity, logCPM)) +          # x=严重程度，y=logCPM
  geom_violin(aes(fill = WHO_temp_severity)) +      # 小提琴图，按严重程度填充
  stat_boxplot(geom = "errorbar", width = 0.1) +    # 箱线图的须（误差棒）
  geom_boxplot(width = 0.15, fill = "white") +      # 叠加白色窄箱线图
  facet_wrap(. ~ gene_id, scale = "free") +         # 按基因分面，y 轴独立
  labs(x = NULL, y = NULL) +                        # 去掉轴标签
  scale_fill_npg() +                                # NPG 配色
  theme_test() +                                    # test 主题
  theme(
    legend.position = "none",                       # 不显示图例
    plot.title = element_text(color = "black", size = 10,
                              vjust = 0.5, hjust = 0.5),  # 标题居中
    axis.text = element_text(color = "black", face = "bold", size = 8),  # 轴文字
    strip.background = element_blank(),             # 分面标签背景透明
    strip.text.x = element_text(color = "black", face = "bold", size = 11)  # 分面文字
  )

# ==================== 第二段：用 for 循环逐个基因保存 PDF ====================
df <- read_tsv("data.txt") %>% filter(gene_id != "H2BC4")   # 重新读数据

gene_ids <- unique(df$gene_id)   # 提取所有唯一的基因名

plots <- list()                  # 创建空列表，存放每个基因的图

for (gene in gene_ids) {         # 遍历每个基因
  p <- df %>%
    ggplot(aes(WHO_temp_severity, logCPM)) +
    geom_violin(aes(fill = WHO_temp_severity)) +
    stat_boxplot(geom = "errorbar", width = 0.1) +
    geom_boxplot(width = 0.15, fill = "white") +
    ggtitle(gene) +              # 标题为基因名
    labs(x = NULL, y = NULL) +
    scale_fill_npg() +
    theme_test() +
    theme(
      legend.position = "none",
      plot.title = element_text(color = "black", size = 10,
                                vjust = 0.5, hjust = 0.5),
      axis.text = element_text(color = "black", face = "bold", size = 8)
    )
  
  plots[[gene]] <- p             # 把图存入列表
  
  ggsave(paste0("plot_", gene, ".pdf"),   # 输出文件名
         plot = p,                         # 要保存的图
         dpi = 300, width = 3.9, height = 2.9, units = "in")  # 尺寸
}

save(plots, file = "图.RData")   # 把所有图对象保存为 RData
load("图.RData")                 # 重新加载（此处只是演示，实际可注释掉）

# ==================== 第三段：用 purrr::map 循环绘图并保存 ====================
df <- read_tsv("data.txt") %>% filter(gene_id != "H2BC4")
gene_ids <- unique(df$gene_id)

# 用 map 遍历每个基因，生成图形对象列表
plot_list <- map(gene_ids, function(gene_ids) {
  df %>%
    ggplot(aes(WHO_temp_severity, logCPM)) +
    geom_violin(aes(fill = WHO_temp_severity)) +
    stat_boxplot(geom = "errorbar", width = 0.1) +
    geom_boxplot(width = 0.15, fill = "white") +
    ggtitle(gene_ids) +
    labs(x = NULL, y = NULL) +
    scale_fill_npg() +
    theme_test() +
    theme(
      legend.position = "none",
      plot.title = element_text(color = "black", size = 10,
                                vjust = 0.5, hjust = 0.5),
      axis.text = element_text(color = "black", face = "bold", size = 8)
    )
})
#组合图片，同时放在一个页面
combined <- wrap_plots(plot_list , ncol = 3, nrow = 3)
ggsave(plot = combined ,"组合.pdf",width = 9,height = 9,dpi = 300)
# 用 map2 同时遍历图形列表和基因名，逐个保存
map2(plot_list, gene_ids, function(plot, gene_ids) {
  ggsave(plot, filename = paste0("plot_", gene_ids, ".pdf"),
         dpi = 300, width = 3.9, height = 2.9, units = "in")
})