library(tidyverse)        # 加载 tidyverse，包含 ggplot2、dplyr、readr 等
library(MetBrewer)        # 加载 MetBrewer，提供艺术配色方案
library(ggsignif)         # 加载 ggsignif，用于添加显著性标注
library(patchwork)        # 加载 patchwork，用于组合多个图形
library(survival)         # 加载 survival，用于生存分析
library(survminer)        # 加载 survminer，提供 ggsurvplot() 绘制生存曲线

# 自定义主题函数
theme_niwot <- function(){
  theme(panel.background = element_blank(),                      # 面板背景透明
        panel.grid.minor = element_blank(),                      # 不显示次网格线
        axis.title.y = element_text(margin = margin(r =3),       # y 轴标题：右边距 3
                                    size = 11, face="bold",      # 字号 11，粗体
                                    color="black"),              # 黑色
        axis.text = element_text(color="black", size=11),        # 轴文字：黑色，字号 11
        panel.border = element_rect(linetype = "solid",          # 面板边框：实线
                                    colour = "black",            # 黑色
                                    fill = "NA",                 # 无填充
                                    size = 1),                   # 线宽 1
        legend.position = "non")                                 # 图例位置：不显示（"non" 是笔误，应为 "none"）
}

# ==================== 图 p1 ====================
p1 <- read_tsv("F6-b.txt") %>%                                   # 读取 F6-b.txt
  select(1,3) %>%                                                # 选择第 1、3 列
  left_join(., read_tsv("F6-a.txt"),                             # 左连接 F6-a.txt
            by = c("sampleID" = "Patient_ID")) %>%               # 连接键：sampleID 与 Patient_ID
  select(Subtype, `Non-silent per Mb`) %>%                       # 选择 Subtype 和 `Non-silent per Mb` 列
  mutate(Subtype = as.character(Subtype)) %>%                    # Subtype 转为字符型
  ggplot(aes(Subtype, `Non-silent per Mb`, fill = Subtype)) +    # x=Subtype，y=突变负荷，填充按 Subtype
  geom_boxplot(outlier.shape = NA,                               # 箱线图：不显示离群点
               linetype = "dashed",                              # 线型：虚线
               width = 0.5,                                      # 箱宽 0.5
               color = "black") +                                # 边框黑色
  stat_boxplot(aes(ymin = ..lower.., ymax = ..upper..),          # 自定义箱线图上下边缘
               outlier.shape = NA, width = 0.5) +                # 不显示离群点，宽度 0.5
  stat_boxplot(geom = "errorbar",                                # 添加误差棒
               aes(ymin = ..ymax..),                             # 上须
               width = 0.2) +                                    # 误差棒宽度 0.2
  stat_boxplot(geom = "errorbar",                                # 添加误差棒
               aes(ymax = ..ymin..),                             # 下须
               width = 0.2) +                                    # 宽度 0.2
  stat_summary(geom = "crossbar",                                # 添加均值横线
               fun = "mean",                                     # 统计函数：均值
               linetype = "dotdash",                             # 线型：点划线
               width = 0.5,                                      # 宽度 0.5
               color = "black") +                                # 黑色
  ylim(0, 20) +                                                  # y 轴范围 0-20
  scale_fill_manual(values = c("#6A3D9A", "#1F78B4", "#33A02C")) +  # 手动填充色
  labs(x = NULL) +                                               # x 轴标签为空
  theme_niwot()                                                  # 应用自定义主题

# ==================== 图 p2 ====================
p2 <- read_tsv("F6-b.txt") %>%                                   # 读取 F6-b.txt
  mutate(Subtype = as.character(Subtype)) %>%                    # Subtype 转字符
  ggplot(aes(Subtype, HRD, fill = Subtype)) +                    # x=Subtype，y=HRD，填充按 Subtype
  geom_boxplot(outlier.shape = NA,                               # 箱线图：不显示离群点
               linetype = "dashed",                              # 虚线
               width = 0.5,                                      # 箱宽 0.5
               color = "black") +                                # 边框黑色
  stat_boxplot(aes(ymin = ..lower.., ymax = ..upper..),          # 自定义上下边缘
               outlier.shape = NA, width = 0.5) +                # 不显示离群点
  stat_boxplot(geom = "errorbar",                                # 上须
               aes(ymin = ..ymax..), width = 0.2) +              # 宽度 0.2
  stat_boxplot(geom = "errorbar",                                # 下须
               aes(ymax = ..ymin..), width = 0.2) +              # 宽度 0.2
  stat_summary(geom = "crossbar",                                # 均值横线
               fun = "mean",                                     # 均值
               linetype = "dotdash", width = 0.5,                # 点划线，宽度 0.5
               color = "black") +                                # 黑色
  geom_signif(comparisons = list(c("1","3"), c("2","3")),        # 显著性比较：1 vs 3，2 vs 3
              map_signif_level = T,                              # 自动显示显著性符号
              vjust = 0.5,                                       # 垂直位置
              color = "black",                                   # 颜色
              textsize = 6,                                      # 文字大小
              test = wilcox.test,                                # 检验方法：Wilcoxon
              step_increase = 0.1) +                             # 增加步长
  scale_fill_manual(values = c("#6A3D9A", "#1F78B4", "#33A02C")) +  # 填充色
  labs(x = NULL) +                                               # x 轴标签为空
  theme_niwot()                                                  # 自定义主题

# ==================== 数据准备 df（用于 p3） ====================
df <- read_tsv("F6-b.txt") %>% select(1,3) %>%                   # 读取 F6-b.txt，选第 1、3 列
  left_join(., read_tsv("F6-e.txt"),                             # 左连接 F6-e.txt
            by = c("sampleID" = "Sample")) %>%                   # 键：sampleID 与 Sample
  left_join(., read_tsv("F6e-CYT.txt"),                          # 左连接 F6e-CYT.txt
            by = c("sampleID" = "Sample")) %>%                   # 键：sampleID 与 Sample
  left_join(., read_tsv("F6e-MHC.txt") %>%                       # 左连接 F6e-MHC.txt
              dplyr::rename("MHC Score" = "Score"),              # 将 Score 重命名为 MHC Score
            by = c("sampleID" = "Sample")) %>%                   # 键：sampleID 与 Sample
  select(-Project.x, -Project.y, -Project, -sampleID) %>%        # 删除多余列
  mutate(Subtype = as.factor(Subtype)) %>%                       # Subtype 转因子
  pivot_longer(-Subtype)                                         # 宽转长：除 Subtype 外都转成长表

df$name <- factor(df$name,                                       # 将 name 转为因子
                  levels = c("Immune Score", "CYT-Score", "MHC Score"))  # 固定顺序

# ==================== 图 p3 ====================
p3 <- ggplot(df, aes(x = value, color = Subtype)) +              # x=value，颜色按 Subtype
  facet_wrap(. ~ name, scales = "free_x") +                      # 按 name 分面，x 轴独立
  stat_ecdf() +                                                  # 绘制累积分布函数
  labs(y = "cumulative distribution funcition (CDF)", x = NULL) +  # y 轴标签（原拼写错误）
  scale_color_manual(values = c("#6A3D9A", "#1F78B4", "#33A02C")) +  # 手动颜色
  theme(panel.background = element_blank(),                      # 面板背景透明
        panel.grid.minor = element_blank(),                      # 不显示次网格线
        axis.title.y = element_text(margin = margin(r =5),       # y 轴标题
                                    size = 11, color = "black",  # 字号 11，黑色
                                    face = "bold"),              # 粗体
        axis.text = element_text(color = "black", size = 11),    # 轴文字
        panel.border = element_rect(linetype = "solid",          # 面板边框实线
                                    colour = "black",            # 黑色
                                    fill = "NA", size = 1),      # 无填充，线宽 1
        strip.text.x = element_text(colour = "black", size = 11),  # 分面文字
        strip.background = element_rect(fill = "#00A08A"),       # 分面背景色
        panel.spacing.x = unit(0.1, "cm"),                       # 分面水平间距
        legend.title = element_blank(),                          # 图例标题为空
        legend.key = element_blank(),                            # 图例键背景透明
        legend.text = element_text(color = "black", size = 10),  # 图例文字
        legend.spacing.x = unit(0.1, 'cm'),                      # 图例水平间距
        legend.background = element_blank(),                     # 图例背景透明
        legend.position = c(0, 1),                               # 图例位置左上
        legend.justification = c(0, 1))                          # 图例对齐左上

# ==================== 生存数据 df 与拟合 ====================
df <- read_tsv("F5-f.txt") %>% select(SurvivalTime, vital_status, Subtype) %>%  # 读取生存数据
  mutate(Subtype = as.factor(Subtype),                           # Subtype 转因子
         vital_status = as.logical(case_when(                    # vital_status 转逻辑值
           vital_status == "alive" ~ "TRUE",                     # alive → TRUE
           vital_status == "dead" ~ "FALSE")))                   # dead → FALSE

fit <- survfit(Surv(SurvivalTime, vital_status) ~ Subtype, data = df)  # 按 Subtype 拟合生存曲线

# ==================== 图 p4（生存曲线） ====================
p4 <- ggsurvplot(fit,                                            # 生存拟合对象
                 risk.table = F,                                 # 不显示风险表
                 risk.table.col = "strata",                      # 风险表颜色按分层
                 conf.int.style = "step",                        # 置信区间样式：阶梯
                 pval = "Log-rank P = 0.02",                     # p 值文本
                 pval.coord = c(4000, 0.99),                     # p 值位置
                 size = 0.8,                                     # 线条大小
                 font.legend = 8,                                # 图例字体大小
                 palette = c("#6A3D9A", "#1F78B4", "#33A02C"),   # 颜色
                 ggtheme = theme_bw() +                          # 基础主题
                   theme(panel.border = element_rect(fill = NA,  # 面板边框
                                                     color = "black",
                                                     size = 1,
                                                     linetype = "solid"),
                         axis.line = element_line(colour = "black"),  # 轴线
                         axis.text = element_text(hjust = 0.5,        # 轴文字
                                                  size = 11,
                                                  colour = "black"),
                         axis.title = element_text(face = "bold",     # 轴标题
                                                   size = 11,
                                                   colour = 'black',
                                                   margin = margin(r =3)),
                         legend.title = element_blank(),              # 图例标题
                         legend.key = element_blank(),                # 图例键
                         legend.text = element_text(color = "black",  # 图例文字
                                                    size = 10),
                         legend.spacing.x = unit(0.1, 'cm'),          # 图例间距
                         legend.background = element_blank(),         # 图例背景
                         legend.position = c(0, 1),                   # 图例位置
                         legend.justification = c(0, 1)),             # 图例对齐
                 legend.labs = c(paste0("C1", "(", fit$n[1], ")"),    # 图例标签
                                 paste0("C2", "(", fit$n[2], ")"),
                                 paste0("C3", "(", fit$n[3], ")")))

# ==================== 组合图形 ====================
(p1 + p2 + p4$plot + plot_layout(ncol = 3, width = c(1, 1, 2))) / p3 +
  plot_annotation(tag_levels = 'A')                              # 添加 A、B、C 标签

# ==================== 保存图片 ====================
ggsave("图.pdf", width = 6, height = 6, dpi = 300)               # 保存为 PDF