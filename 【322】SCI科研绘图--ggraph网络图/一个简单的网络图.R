
setwd("C:/Users/34790/Desktop/ggraph网络图-一个简单的网络图")


library(ggraph)
library(tidygraph)
library(ggnewscale)
df <- read.csv('net.csv', header = T)#这是一个基因互作关系网络文件
#接下来，为了让我们的网络图更加丰富，我们人为对这些基因进行分组等等
#事实上，如果是你有用的数据，可以提前整理好文件读入

#节点基因，分组添加
from = unique(df$from)
to = unique(df$to)
genes <- data.frame(unique(c(from, to)))
colnames(genes) <- 'gene'
genes$pathway <- c(rep("MAPK",6), rep("Wnt",5), rep("JAK",6),rep("Toll",5))#这里的分组是虚构的数据
genes$regulation <- c(sample(c(rep("up",12), rep("down",10))))#随机分下上下调


#构建作图数据
data <- tbl_graph(nodes = genes, edges = df)




#绘图,ggraph是ggplot的拓展包，所以当你构建好ggraph作图数据后，剩下的和你在利用ggplot2作图没什么分别
ggraph(data,layout='linear',circular = TRUE) +
  geom_node_point(aes(size=8,#节点
                      fill = pathway),shape=21) +
  scale_fill_manual(values = c('#4CA85F','orange','#4A90BD','#C387B8'))+
  geom_node_point(aes(size=8,#节点
                      color = regulation),shape=21,stroke=2)+
  scale_color_manual(values = c('black','#B11E23'))+
  scale_size_continuous(range = c(30, 1))+
  geom_node_text(aes(x = x*1.15, #添加节点文字
                     y=y*1.15, #x.y坐标
                     label=gene,#label
                     angle=-((-node_angle(x, y) + 90) %% 180) + 90),#调整文字角度
                 size=3,#文字大小
                 hjust='outward')+#调整文字朝向
  coord_cartesian(xlim=c(-1.5,1.5),ylim = c(-1.5,1.5))+#限制xy范围，避免图片超出画板
  geom_edge_arc(aes(width=score),color="lightblue")+#连线
  scale_edge_width_continuous(range = c(0.5,1))+#连线粗细
  theme_graph()

  
  
  
  
  
  

















