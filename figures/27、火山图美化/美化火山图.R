##观看课程【limma包获取差异基因及火山图绘制】获取分析数据

# setwd("")
## 加载R包
library(dplyr)
library(ggfun)
library(Seurat)
library(ggplot2)
library(RColorBrewer)
library(patchwork)
library(viridis)
library(tidyverse)
library(ggrepel)

#读取文件
{
deg_result <- read.csv(file = "差异分析结果（新）.csv") %>%
  dplyr::mutate(rank = -1 * rank(avg_log2FC, ties.method = "max"))
}

## 手动选择的基因“CTSL”
selected_gene <- deg_result %>%
  dplyr::filter(SYMBOL == "CTSL")

# ## 上调 top2
# top_2 <- deg_result %>%
#   dplyr::filter(!is.na(SYMBOL)) %>%
#   dplyr::arrange(desc(avg_log2FC)) %>%
#   dplyr::slice_head(n = 2)
# 
# ## 下调2
# tail_2 <- deg_result %>%
#   dplyr::filter(!is.na(SYMBOL)) %>%
#   dplyr::arrange(desc(avg_log2FC)) %>%
#   dplyr::slice_tail(n = 3)

## 绘制火山图
deg_result %>%
  ggplot() + 
  geom_point(aes(x = rank, y = avg_log2FC, color = Pvalue, size = abs(avg_log2FC))) + 
  
  scale_color_gradient2(low = '#2C7BB6', high = '#D7191C', mid = '#FFFFBF', 
                        midpoint = 0.5, name = "Pvalue") +  
  geom_hline(yintercept = 0, linetype = "solid", color = "black") +  
  
  ## 添加手动选择的基因“CTSL”标签
  geom_text_repel(data = selected_gene,
                  aes(x = rank, y = avg_log2FC, label = SYMBOL),
                  box.padding = 0.5, nudge_x = 10,
                  nudge_y = 0.2, segment.curvature = -0.1,
                  segment.ncp = 3, segment.angle = 20,
                  direction = "y", hjust = "left", color = "blue", size = 5) +
  
  # geom_text_repel(data = top_2,
  #                 aes(x = rank + 50, y = avg_log2FC, label = SYMBOL),
  #                 box.padding = 0.5, nudge_x = 10,
  #                 nudge_y = 0.2, segment.curvature = -0.1,
  #                 segment.ncp = 3, segment.angle = 20,
  #                 direction = "y", hjust = "left") + 
  # 
  # geom_text_repel(data = tail_2,
  #                 aes(x = rank + 10, y = avg_log2FC, label = SYMBOL),
  #                 box.padding = 0.5, nudge_x = 10, nudge_y = -0.2,
  #                 segment.curvature = -0.1, segment.ncp = 3,
  #                 segment.angle = 20, direction = "y", hjust = "right") + 

  scale_size(range = c(1, 7), name = "log2(Fold Change)") + 
  labs(x = "Rank of differentially expressed genes",
       y = "log2FoldChange") + 
  coord_cartesian(ylim = c(-max(abs(deg_result$avg_log2FC)), max(abs(deg_result$avg_log2FC)))) +  ## x轴放在中间
  theme_bw() + 
  theme(
    legend.background = element_rect(fill = "#FFFFFF", color = "#808080", linetype = 1),
    axis.text = element_text(color = "#000000", size = 12),
    axis.title = element_text(color = "#000000", size = 15),
    axis.ticks.x = element_blank(),   
    axis.text.x = element_blank()        
  )
  
# 尝试用较大的尺寸保存你的网络图
pdf("图.pdf", width = 6, height = 6)  # 尺寸设置得大一些
print(p6+ theme(legend.position = "bottom",legend.box = "vertical"))  # 或 p7，替换成你实际的图形对象
dev.off()
