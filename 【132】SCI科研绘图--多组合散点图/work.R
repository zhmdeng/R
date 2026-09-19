package.list=c("tidyverse","aplot","RColorBrewer","ggtree")  # 需要加载的包列表

for (package in package.list) {                               # 遍历包列表
  if (!require(package,character.only=T, quietly=T)) {        # 检查包是否已安装并加载
    install.packages(package)                                 # 未安装则从 CRAN 安装
    library(package, character.only=T)                        # 再加载
  }
}
# ⚠️ ggtree 是 Bioconductor 包，install.packages("ggtree") 会失败。
# 应改用 BiocManager::install("ggtree")，或提前手动安装好。

data <- read_tsv('data.txt',col_names=F) %>%                  # 读取制表符文件，无列名
  select(1,2,6) %>%                                           # 取第1、2、6列（X1, X2, X6）
  group_by(X1,X6) %>%                                         # 按 X1 和 X6 分组
  count(X2) %>%                                               # 统计每个 X2 出现的次数
  ungroup() %>% select(-2) %>%                                # 取消分组，删除第2列(X6)
  pivot_wider(., names_from =X2,values_from = n) %>%          # 宽表：X2 的值变成列名，n 为值
  mutate_all(~replace(.,is.na(.), 0)) %>%                     # 所有 NA 替换为 0
  # ⚠️ mutate_all 已不推荐，可用 mutate(across(everything(), ~replace_na(., 0)))
  pivot_longer(-X1) %>%                                       # 再转长表，除 X1 外全部变成 name/value
  mutate(value=as.character(value),signif=value,              # value 转字符，signif 复制 value
         signif=case_when(value=="0" ~ " ",TRUE ~ as.character(value)),  # 0 显示为空格
         name=case_when(name=="Box II -like sequence" ~       # 给特定名称加两个空格
                          "Box II -like sequence  ",
                        TRUE ~ as.character(name)),
         n=str_remove(X1,"gene") %>% as.numeric()) %>%        # 从 X1 中删除 "gene" 并转数值
  arrange(n)                                                  # 按 n 排序

df <- data %>% select(name) %>% distinct() %>%                # 提取唯一的 name
  mutate(group= rep(LETTERS[1:3],times=c(8,6,9)               # 手动分配 A/B/C 组，数量 8/6/9
  )) %>% left_join(.,data,by="name")           # 再合并回 data
# ⚠️ 这里硬编码了每组数量，若 name 总数不是 23 会报错

df$X1 <- factor(df$X1,levels=df$X1 %>% rev() %>% as.data.frame() %>%  # 将 X1 转为因子，水平反转
                  distinct() %>% pull())

df$name <- factor(df$name,levels = unique(df$name))           # name 转为因子，保持原顺序

p1 <- df %>% mutate(value=as.numeric(value)) %>%              # value 转数值
  filter(value==0) %>%                                        # 只保留 value 为 0 的点
  ggplot(aes(name,X1))+                                       # x=name, y=X1
  geom_point(aes(fill=group),color="white",size=3,pch=21)+    # 点图，填充按 group，白色边框
  labs(x=NULL,y=NULL)+                                        # 无轴标签
  scale_fill_manual(values=c("#12618D","#EDB749","#3CB2EC"))+ # 自定义填充色
  theme(axis.text.x=element_text(angle = 45,hjust=1,vjust=1,color="black"), # x 轴文字旋转
        axis.text.y=element_blank(),                          # y 轴文字不显示
        axis.line = element_line(color = "white",size = 0.4), # 白色轴线
        axis.ticks.y=element_blank(),                         # y 轴刻度不显示
        panel.grid.minor = element_blank(),                   # 次网格线不显示
        panel.grid.major.x = element_line(size = 0.8,color = "grey60"), # x 主网格线
        panel.background = element_blank(),                   # 背景透明
        legend.position = "none")                            
p1

p2 <- pivot_wider(df %>% select(name,X1,value),names_from=name,  # 宽表：name 为列，value 为值
                  values_from = value) %>% 
  column_to_rownames(var="X1") %>%                            # X1 设为行名
  mutate(across(where(is.character),as.numeric)) %>%          # 字符列转数值
  summarize(across(is.numeric,sum)) %>%                       # 每列求和
  rownames_to_column(var="id") %>%                            # 行名转 id 列
  pivot_longer(-id) %>%                                       # 再转长表
  ggplot(aes(name,value))+                                    # x=name, y=value
  geom_col(fill="grey50",size=0.5)+                           # 柱状图
  # ⚠️ size 在新版 ggplot2 中应改为 linewidth
  geom_text(aes(label=value),hjust=0.5,vjust=0,color="black",size=3.5) + # 柱上显示数值
  scale_y_continuous(limits = c(0,110),expand = c(0,1)) +     # y 轴范围 0~110
  labs(x=NULL,y=NULL)+                                        # 无轴标签
  theme_classic()+                                            # 经典主题
  theme(axis.text.x=element_blank(),                          # x 轴文字不显示
        axis.text.y=element_text(color="black"),              # y 轴文字
        axis.ticks.x =element_blank())                        # x 轴刻度不显示

hr <- pivot_wider(df %>% select(name,X1,value),names_from=name,  # 宽表（用于聚类）
                  values_from = value) %>% 
  column_to_rownames(var="X1") %>%                            # X1 设为行名
  mutate(across(where(is.character),as.numeric))              # 字符转数值

p3 <- hclust(dist(hr)) %>%                                    # 对行聚类
  ggtree(layout="rectangular", branch.length="none")+         # 用 ggtree 画聚类树
  theme_void()                                                # 空主题

p4 <- hr %>% rownames_to_column(var="gene") %>%               # 行名转 gene 列
  rowwise() %>%                                               # 按行操作
  mutate(sum = sum(across(where(is.numeric)))) %>%            # 每行数值求和
  select(gene,sum) %>%                                        # 只保留 gene 和 sum
  mutate(group="A") %>%                                       # 添加 group 列
  ggplot(aes(group,gene,fill=sum))+                           # x=group, y=gene, 填充=sum
  geom_tile()+                                                # 热图方块
  scale_color_gradientn(colours = rev(RColorBrewer::brewer.pal(11,"RdBu")))+ # 颜色映射（实际用 fill）
  scale_fill_gradientn(colours = rev(RColorBrewer::brewer.pal(11,"RdBu")))+  # 填充渐变
  theme_void()+                                               # 空主题
  theme(legend.position = "none")                              

p1 %>% insert_top(p2,height=0.2) %>%                          # 在 p1 顶部插入 p2
  insert_left(p4,width=0.1) %>%                               # 在左侧插入 p4（热图）
  insert_left(p3,width = 0.3)                                 # 再在左侧插入 p3（聚类树）
# 最终图形从左到右：p3、p4、p1，顶部是 p2

ggsave("图.pdf",dpi = 300)                                    # 保存图片
# ⚠️ 建议加上 width 和 height，例如 width = 10, height = 8