library(tidyverse)
library(ggsci)
library(magrittr)
library(patchwork)


load("df.Rdata")

df$age <- factor(df$age,levels= df$age %>% as.data.frame() %>% distinct() %>% pull())

df$location <- factor(df$location,
                      levels=c("Global","High SDI","High-middle SDI","Middle SDI","Low-middle SDI","Low SDI"))

df$measure <- factor(df$measure,levels=c("Incidence","Deaths","DALYs"))

df %>% filter(sex=="Both") %>% 
  ggplot(aes(age,val,group=location))+
  geom_line(aes(color=location),size=0.8)+
  geom_point(aes(fill=location,color=location),size=2,alpha=0.8)+
  facet_grid(measure~.,scales = "free",labeller=label_wrap_gen())+
  scale_color_jco()+
  theme_bw()+
  theme(panel.spacing.x=unit(0,"cm"),
        panel.spacing.y=unit(0.15,"cm"),
        axis.title = element_blank(),
        strip.background = element_rect(fill="grey80"),
        strip.text.x = element_blank(),
        strip.text.y = element_text(size=10,color="black"),
        strip.background.y = element_blank(),
        strip.background.x = element_blank(),
        axis.text = element_text(color="black"),
        plot.margin=unit(c(0.2,0.5,0.2,0.5),units=,"cm"),
        legend.text = element_text(size=8),
        legend.key.width=unit(0.5,'cm'),
        legend.key.height=unit(0.5,'cm'),
        legend.spacing.x=unit(0.1,'cm'),
        legend.spacing.y=unit(0,'cm'),
        legend.background = element_blank(),
        legend.position = c(0.005,0.99),
        legend.justification = c(0.005,0.99))

#----------------------------------------------------------------------------
dd1 <- df %>% filter(measure=="Incidence",sex !="Both")

df1 <- dd1 %>% filter(sex=="Male") %>% 
  left_join(.,dd1 %>% filter(sex=="Female"),by=c("location","age")) %>%
  dplyr::rename(Male="val.x",Female="val.y") %>% 
  mutate(`Male/Female`=Male/Female) %>% select(1,2,3,`Male/Female`) %>%
  mutate(across("age",str_replace,"95 plus","95+")) %>%
  mutate(across("age",str_replace," to ","-"))

df1$age <- factor(df1$age,levels=df1$age %>% as.data.frame() %>% distinct() %>% pull())
df1$location <- factor(df1$location,
                       levels=c("Global","High SDI","High-middle SDI","Middle SDI","Low-middle SDI","Low SDI"))

ggplot(df1,aes(age,`Male/Female`,group=1,color="location"))+
  geom_point(size=1)+geom_line()+
  facet_wrap(.~location,nrow =3,scales = "free_y")+
  scale_color_nejm()+
  labs(x="age (years old)")+labs(y="Incidence Male/Female")+
  labs(x=NULL)+
  theme_bw()+
  theme(legend.position="non",strip.background.x = element_blank(),
        strip.text.x = element_text(size=10,color="black",face="bold"),
        axis.text.x = element_text(color="black",angle = 45,vjust=1,hjust=1),
        axis.text.y=element_text(color="black"),
        plot.margin=unit(c(0.2,0.2,0.2,0.2),units=,"cm"),
        axis.title.y=element_text(color="black",margin = margin(r=5)),
        axis.title.x=element_text(color="black",margin = margin(t=5))
  )
ggsave("图.pdf",width = 6,height = 6,dpi = 300)
