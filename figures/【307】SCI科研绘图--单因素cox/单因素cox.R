# ==================== 加载包 ====================
library(tidyverse)      # 数据处理和绘图
library(ggplot2)        # 绘图
library(ggstatsplot)    # 提供 theme_ggstatsplot()
library(survival)       # 提供 coxph()、Surv()
library(stringr)        # 字符串处理
library(viridis)        # 提供 viridis 配色
library(scales)         # 提供 muted() 等颜色函数

# ==================== 建立疾病全称与缩写的对应关系 ====================
samplepair <- read.delim("samplepair.txt", as.is = T)   # 读取样本配对文件
tissue <- samplepair$TCGA                                # 提取 TCGA 缩写列
names(tissue) <- samplepair$Detail                       # 用 Detail 列作为名字
# 结果：tissue 是一个命名向量，名字是疾病全称，值是缩写

# ==================== 读取 TCGA 临床数据 ====================
tcgacase <- read.delim(file = "TCGA_phenotype_denseDataOnlyDownload.tsv",
                       header = T, as.is = T)            # 读取 TCGA 表型数据
tcgacase$tissue <- tissue[tcgacase$X_primary_disease]    # 匹配疾病缩写
tcgacase$type <- ifelse(tcgacase$sample_type == 'Solid Tissue Normal',
                        paste(tcgacase$tissue, "normal_TCGA", sep = "_"),   # 正常样本
                        paste(tcgacase$tissue, "tumor_TCGA", sep = "_"))    # 肿瘤样本
tcgacase$type2 <- ifelse(tcgacase$sample_type == 'Solid Tissue Normal',
                         "normal", "tumor")              # 简化标签
tcgatable <- tcgacase[, c(1, 5:7)]                       # 提取需要的列
head(tcgatable)
lung <- filter(tcgatable, tissue == "LUAD", type2 == "tumor")   # 筛选 LUAD 肿瘤样本

# ==================== 读取生存数据 ====================
surivival <- read.delim("Survival_SupplementalTable_S1_20171025_xena_sp",
                        as.is = T)                       # 读取生存数据
colnames(surivival)                                      # 查看列名
LUADsur <- filter(surivival, cancer.type.abbreviation == 'LUAD')   # 筛选 LUAD
LUADsur <- LUADsur[, c(1, 2, 3, 4, 14, 26, 27)]          # 提取需要的列
rownames(LUADsur) <- LUADsur$sample                      # 行名设为样本

# ==================== 取交集 ====================
a <- intersect(LUADsur$sample, lung$sample)              # 生存数据和肿瘤样本的交集
luadsurvival_tumor <- LUADsur[a, ] %>%
  filter(!is.na(OS.time))                                # 过滤缺失生存时间的样本
surdata <- luadsurvival_tumor[, c(1, 2, 6, 7)]           # 整理生存资料
surdata[1:5, 1:4]

# ==================== 读取表达数据 ====================
tcgadatatpm <- read.csv("easy_input_expr.csv", row.names = 1)   # 读取 TPM 表达矩阵
tcgadatatpm[1:5, 1:5]

# ==================== 匹配样本 ====================
luadsample <- intersect(row.names(tcgadatatpm), luadsurvival_tumor$sample)
# 找有生存信息的样本
row.names(luadsurvival_tumor) <- luadsurvival_tumor$sample
luadsurvival_tumortime <- luadsurvival_tumor[luadsample, ]
luadsurvival_tumortime <- luadsurvival_tumortime[, c(1, 6, 7)]   # 只保留生存相关列
tcgadatatpm$sample <- row.names(tcgadatatpm)             # 添加 sample 列

# ==================== 合并数据 ====================
realdata <- merge(luadsurvival_tumortime, tcgadatatpm, by = "sample")
# 按 sample 合并生存和表达数据
row.names(realdata) <- realdata$sample                   # 行名设为样本
realdata <- realdata[, -1]                               # 删除 sample 列
# realdata$OS.time <- realdata$OS.time/365              # 可选：天转年
realdata[1:5, 1:5]

write.csv(realdata, "easy_input_4cox.csv")               # 保存合并数据
realdata <- read.csv("easy_input_4cox.csv", row.names = 1)   # 重新读取
realdata[1:3, 1:6]

# ==================== 单因素 Cox 回归 ====================
Coxoutput <- data.frame()                                # 创建空数据框

for (i in colnames(realdata[, 3:ncol(realdata)])) {      # 遍历每个基因
  cox <- coxph(Surv(OS.time, OS) ~ realdata[, i], data = realdata)
  # 对单个基因做 Cox 回归
  coxSummary <- summary(cox)                             # 回归结果摘要
  
  Coxoutput <- rbind(Coxoutput,
                     cbind(gene = i,                     # 基因名
                           HR = coxSummary$coefficients[, "exp(coef)"],   # 风险比
                           z = coxSummary$coefficients[, "z"],            # z 值
                           pvalue = coxSummary$coefficients[, "Pr(>|z|)"], # p 值
                           lower = coxSummary$conf.int[, 3],              # 95% 下限
                           upper = coxSummary$conf.int[, 4]))             # 95% 上限
}

for (i in c(2:6)) {                                      # 遍历数值列
  Coxoutput[, i] <- as.numeric(as.vector(Coxoutput[, i]))  # 转为数值型
}

Coxoutput <- arrange(Coxoutput, pvalue) %>%              # 按 p 值排序
  filter(pvalue < 0.05)                                  # 只保留显著的基因

write.csv(Coxoutput, 'cox_output.csv', row.names = F)    # 保存结果
Coxoutput <- read.csv("cox_output.csv")                  # 重新读取
head(Coxoutput)

# ==================== 筛选用于绘图的基因 ====================
plotCoxoutput <- filter(Coxoutput, HR <= 0.92 | HR >= 1.15)
# 选择 HR 极端（保护性或风险性）的基因

# ==================== 森林图 1：用 ggstatsplot 主题 ====================
ggplot(data = plotCoxoutput, aes(x = HR, y = gene, color = pvalue)) +
  geom_errorbarh(aes(xmax = upper, xmin = lower),        # 误差棒（水平）
                 color = "black", height = 0, size = 1.2) +  # 黑色，无端点，线粗 1.2
  geom_point(aes(x = HR, y = gene), size = 3.5, shape = 18) +  # 菱形点
  geom_vline(xintercept = 1, linetype = 'dashed', size = 1.2) +  # HR=1 参考线
  scale_x_continuous(breaks = c(0.75, 1, 1.30)) +        # x 轴刻度
  coord_trans(x = 'log2') +                              # x 轴 log2 变换
  ylab("Gene") +                                         # y 轴标签
  xlab("Hazard ratios of AA in LUAD") +                  # x 轴标签
  labs(color = "P value", title = "Univariate Cox regression analysis") +
  scale_color_viridis() +                                # viridis 配色
  theme_ggstatsplot() +                                  # ggstatsplot 主题
  theme(panel.grid = element_blank())                    # 去除网格线

ggsave('plot1.pdf', width = 8, height = 9)

# ==================== 森林图 2：误差线跟着变色 ====================
ggplot(data = plotCoxoutput, aes(x = HR, y = gene, color = pvalue)) +
  geom_errorbarh(aes(xmax = upper, xmin = lower, color = pvalue),  # 误差棒按 p 值着色
                 height = 0, size = 1.2) +
  geom_point(aes(x = HR, y = gene), size = 3.5, shape = 18) +  # 菱形点
  geom_vline(xintercept = 1, linetype = 'dashed', size = 1.2) +  # HR=1 参考线
  scale_x_continuous(breaks = c(0.75, 1, 1.30)) +
  coord_trans(x = 'log2') +
  ylab("Gene") +
  xlab("Hazard ratios of AA in LUAD") +
  labs(color = "P value", title = "") +
  scale_color_viridis() +
  theme_ggstatsplot() +
  theme(panel.grid = element_blank())

ggsave('plot2.pdf', width = 8, height = 9)

# ==================== 森林图 3：自定义主题和樱花色 ====================
ggplot(data = plotCoxoutput, aes(x = HR, y = gene, color = pvalue)) +
  geom_errorbarh(aes(xmax = upper, xmin = lower),        # 误差棒
                 color = 'black', height = 0, size = 1.2) +
  geom_point(aes(x = HR, y = gene), size = 3.5, shape = 18) +  # 菱形点
  geom_vline(xintercept = 1, linetype = 'dashed', size = 1.2) +  # HR=1 参考线
  scale_x_continuous(breaks = c(0.75, 1, 1.30)) +
  coord_trans(x = 'log2') +
  ylab("Gene") +
  xlab("Hazard ratios of AA in LUAD") +
  labs(color = "P value", title = "") +
  scale_color_gradient2(low = muted("skyblue"),          # 低 p 值：天蓝色
                        mid = "white",                   # 中间：白色
                        high = muted('pink'),            # 高 p 值：粉色
                        midpoint = 0.025) +              # 中间点
  theme_bw(base_size = 12) +                             # 基础主题
  theme(panel.grid = element_blank(),                    # 去除网格线
        axis.text.x = element_text(face = "bold", color = "black", size = 9),
        axis.text.y = element_text(face = "bold", color = "black", size = 9),
        axis.title.x = element_text(face = "bold", color = "black", size = 11),
        axis.title.y = element_text(face = "bold", color = "black", size = 11),
        legend.text = element_text(face = "bold", color = "black", size = 9),
        legend.title = element_text(face = "bold", color = "black", size = 11),
        panel.border = element_rect(colour = 'black', size = 1.4))   # 边框加粗

ggsave('plot3.pdf', width = 8, height = 9)

# ==================== 复现原文的 AA 森林图 ====================
df <- read.csv("suptable2LUAD.csv", sep = '\t')          # 读取补充表

AAtop20 <- filter(df, splice_type == 'AA')               # 提取 AA 类型
AAtop20 <- AAtop20[1:20, ]                               # 取前 20 个

ggplot(data = AAtop20, aes(x = HR, y = symbol, color = pvalue)) +
  geom_errorbarh(aes(xmax = upper, xmin = lower),        # 误差棒
                 color = "black", height = 0, size = 1.2) +
  geom_point(aes(x = HR, y = symbol), size = 3.5, shape = 18) +  # 菱形点
  geom_vline(xintercept = 1, linetype = 'dashed', size = 1.2) +  # HR=1 参考线
  scale_x_continuous(breaks = c(0.5, 1, 2)) +            # x 轴刻度
  coord_trans(x = 'log2') +                              # log2 变换，让 0.5-1 和 1-2 等距
  ylab("Gene") +
  xlab("Hazard ratios of AA in LUAD") +
  labs(color = "P value", title = "") +
  theme_ggstatsplot() +
  theme(panel.grid = element_blank())

ggsave('sameplot2.pdf', width = 8, height = 9)