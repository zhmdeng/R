#需要输入两个文件：ssGSEA_output.csv
##easy_input_expr.txt   第一行样品名称，第二行基因

library(ggstatsplot)
library(data.table)
library(dplyr)
library(tidyr)
library(ggplot2)
Sys.setenv(LANGUAGE = "en") #显示英文报错信息
options(stringsAsFactors = FALSE) #禁止chr转成factor

rm(list = ls())
tcga_gsva <- read.csv("ssGSEA_output.csv",row.names = 1)
tcga_gsva[1:3,1:3]

#rownames(tcga_gsva) <- gsub("\\.","-",rownames(tcga_gsva))
tcga_gsva[1:3,1:3]

tcga_expr <- read.table("easy_input_expr.txt", row.names = 1, header = T, check.names = F)
tcga_expr[,1:3]

tcga_gsva <- tcga_gsva[colnames(tcga_expr),]
index <- rownames(tcga_expr) #基因名
y <- as.numeric(tcga_expr)
head(y)
colnames <- colnames(tcga_gsva)
data <- data.frame(colnames)
for (i in 1:length(colnames)){
  test <- cor.test(as.numeric(tcga_gsva[,i]),y,method = "spearman")
  data[i,2] <- test$estimate                                            
  data[i,3] <- test$p.value
}
names(data) <- c("symbol","correlation","pvalue")

help(cor.test)
#画图
data %>% 
  #filter(pvalue <0.05) %>% # 如果不想把p值大于0.05的放在图上，去掉最前面的#号
  ggplot(aes(correlation,forcats::fct_reorder(symbol,correlation))) +
  geom_segment(aes(xend=0,yend=symbol)) +
  geom_point(aes(col=pvalue,size=abs(correlation))) +
  scale_colour_gradientn(colours=c("#7fc97f","#984ea3")) +
  #scale_color_viridis_c(begin = 0.5, end = 1) +
  scale_size_continuous(range =c(2,8))  +
  theme_minimal() +
  ylab(NULL)
ggsave("gene_Xcell.pdf")


imucell <- "Macrophages"
# 合并免疫数据和表达量数据
plot_df <- data.frame(gene=y,imucell=tcga_gsva[,imucell])
head(plot_df)


#图2
pdf("gene_1cell_ggscatterstats.pdf")
ggscatterstats(data = plot_df, type = "spearman",
               x = gene,
               y = imucell,
               centrality.para = "mean",
               margins = "both",
               xfill = "#CC79A7",
               yfill = "#009E73",
               marginal.type = "histogram")

dev.off()

#图3
corr_eqn <- function(x,y,digits=2) {
  test <- cor.test(x,y,method ="spearman")
  paste(paste0("n = ",length(x)),
        paste0("r = ",round(test$estimate,digits),"(Spearman)"),
        paste0("p.value= ",round(test$p.value,digits)),
        sep = ", ")
}
#可以测试一下
corr_eqn(plot_df$gene,plot_df$imucell)
plot_df %>% 
  ggplot(aes(gene,imucell)) +
  geom_point(col="#984ea3") +
  geom_smooth(method=lm, se=T,na.rm=T, fullrange=T,size=2,col="#fdc086") +
  geom_rug(col="#7fc97f") +
  theme_minimal() +
  xlab(paste0(index," (TPM)")) +
  ylab(paste0(imucell," (immune infiltration)")) +
  labs(title = paste0(corr_eqn(plot_df$gene,plot_df$imucell))) +
  theme(plot.title = element_text(hjust = 0.5))
ggsave("gene_1cell_ggplot2.pdf")


##安装
##options("repos"= c(CRAN="https://mirrors.tuna.tsinghua.edu.cn/CRAN/"))
##options(BioC_mirror="http://mirrors.ustc.edu.cn/bioc/")
##install.packages("ggstatsplot")
