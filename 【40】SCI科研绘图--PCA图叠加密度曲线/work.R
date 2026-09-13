library(readxl)
library(tidyverse)
library(aplot)
library(vegan)
library(patchwork)
library(cowplot)
library(grid)
library(ggplotify)

# library(pairwiseAdonis)
setwd("/Users/mac/Desktop/PowerBI/sci科研绘图/【40】SCI科研绘图--PCA图叠加密度曲线/nmicrobiology图表复现之PCA图叠加密度曲线")
dat.taxa <- read_excel("F1.xlsx", sheet = "Fig 1a taxonomy PCA")
pca <- dat.taxa

Fig1a.taxa.pca <- ggplot(pca,aes(PC1,PC2))+
  geom_point(size=2,aes(color=Disease,shape=Cohort),show.legend = F)+ 
  scale_color_manual(values=c("#5686C3","#75C500"))+
  scale_shape_manual(values=c(16,15)) +
  stat_ellipse(aes(color = Disease),fill="white",geom = "polygon",
               level=0.95,alpha = 0.01,show.legend = F)+
  labs(x="PC1 (23.3%)",y="PC2 (9.1%)")+
  theme_classic()+
  theme(axis.line=element_line(colour = "black"),
        axis.title=element_text(color="black",face = "bold"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.text = element_text(color="black",size=10,face = "bold"))

pca$Group = paste(pca$Disease,"|", pca$Cohort,sep = "")

Fig1a.taxa.pc1.density <-
  ggplot(pca) +
  geom_density(aes(x=PC1, group=Group,fill=Disease,linetype=Cohort),
               color="black", alpha=0.6,position = 'identity',
               show.legend = F) +
  scale_fill_manual(values=c("#5686C3","#75C500")) +
  scale_linetype_manual(values = c("solid","dashed"))+
  scale_y_discrete(expand = c(0,0.001))+
  labs(x=NULL,y=NULL)+
  theme_classic() +
  theme(axis.text.x=element_blank(),
        axis.ticks.x = element_blank())

Fig1a.taxa.pc2.density <-
  ggplot(pca) +
  geom_density(aes(x=PC2, group=Group, fill=Disease, linetype=Cohort),
               color="black", alpha=0.6, position = 'identity',show.legend = F) +
  scale_fill_manual(values=c("#5686C3","#75C500")) +
  theme_classic()+
  scale_linetype_manual(values = c("solid","dashed"))+
  scale_y_discrete(expand = c(0,0.001))+
  labs(x=NULL,y=NULL)+
  theme(axis.text.y=element_blank(),
        axis.ticks.y=element_blank())+
  coord_flip()

p1 <- Fig1a.taxa.pca %>% 
  insert_top(Fig1a.taxa.pc1.density,height = 0.3) %>% 
  insert_right(Fig1a.taxa.pc2.density,width=0.3) %>% 
  as.ggplot()

#------------------------------------------------------------------------------

dat.funct <- read_excel("F1.xlsx", sheet = "Fig 1a metagenome PCA")
pca <- dat.funct

Fig1a.function.pca <- ggplot(pca,aes(PC1,PC2))+
  geom_point(size=2, aes(col=Disease,shape=Cohort))+
  scale_shape_manual(values=c(16,15)) +
  scale_color_manual(values=c("#5686C3","#75C500")) +
  stat_ellipse(aes(color = Disease),fill="white",geom = "polygon",
               level=0.95,alpha = 0.01,show.legend = F)+
  labs(x="PC1 (17.3%)",y="PC2 (7.3%)")+
  theme_classic()+
  theme(axis.line = element_line(colour = "black"),
        axis.title=element_text(color="black",face = "bold"),
        axis.text=element_text(color="black",face = "bold",size=10),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank())

pca$Group = paste(pca$Disease,"|", pca$Cohort,sep = "")
Fig1a.function.pca

Fig1a.function.pc1.density <-
  ggplot(pca) +
  geom_density(aes(x=PC1, group=Group, fill=Disease, linetype=Cohort),
               color="black", alpha=0.6, position = 'identity',
               show.legend = F) +
  scale_fill_manual(values=c("#5686C3","#75C500")) +
  theme_classic()+
  scale_linetype_manual(values = c("solid","dashed"))+
  scale_y_discrete(expand = c(0,0.001))+
  labs(x=NULL,y=NULL)+
  theme_classic() +
  theme(axis.text.x=element_blank(),
        axis.ticks.x = element_blank())


Fig1a.function.pc2.density <-
  ggplot(pca) +
  geom_density(aes(x=PC2, group=Group,fill=Disease,linetype=Cohort),
               color="black", alpha=0.6,position='identity',
               show.legend = F) +
  scale_fill_manual(values=c("#5686C3","#75C500")) +
  scale_linetype_manual(values = c("solid","dashed"))+
  scale_y_discrete(expand = c(0,0.001))+
  labs(x=NULL,y=NULL)+
  theme_classic()+
  coord_flip()+
  theme(axis.text.y=element_blank(),
        axis.ticks.y=element_blank())

p2 <- Fig1a.function.pca %>% 
  insert_top(Fig1a.function.pc1.density,height = 0.3) %>% 
  insert_right(Fig1a.function.pc2.density,width=0.3) %>% 
  as.ggplot()

(p1|p2)+plot_layout(ncol=2,width = c(0.8,1))


p1
