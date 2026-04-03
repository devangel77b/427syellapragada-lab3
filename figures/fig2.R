library(ggplot2)
library(dplyr)

raw <- read.csv('fig2.csv',header=TRUE)
data <- tibble(raw)
g = 9.81
data <- mutate(data,
     GPE = m_kg*g*h_m)
grouped <- data %>% group_by(design)

# make plot
fig2 <- ggplot(data,aes(x=design,y=h_m,fill=design))+
     geom_hline(yintercept=0,color="gray70")+
     stat_summary(fun=mean,geom="bar",width=0.6)+
     stat_summary(fun.data=mean_sdl,fun.args=list(mult=1),
	geom="errorbar",width=0.2)+
	theme_bw(base_size=8)+
	ylab('$h$, \\unit{\\meter}')+
	theme(legend.position="none",
	axis.title.x=element_blank(),
	axis.title.y=element_text(margin=margin(t=0,r=4,b=0,l=0)))
ggsave('fig2.svg',plot=fig2,width=3.4167,height=2,units="in")

# make plot
fig2b <- ggplot(data,aes(x=design,y=m_kg,fill=design))+
     geom_hline(yintercept=0,color="gray70")+
     stat_summary(fun=mean,geom="bar",width=0.6)+
     stat_summary(fun.data=mean_sdl,fun.args=list(mult=1),
	geom="errorbar",width=0.2)+
	theme_bw(base_size=8)+
	ylab('$m$, \\unit{\\kilo\\gram}')+
	theme(legend.position="none",
	axis.title.x=element_blank())
ggsave('fig2b.svg',plot=fig2b,width=3.4167,height=2,units="in")

# make plot
fig2c <- ggplot(data,aes(x=design,y=GPE,fill=design))+
     geom_hline(yintercept=0,color="gray70")+
     stat_summary(fun=mean,geom="bar",width=0.6)+
     stat_summary(fun.data=mean_sdl,fun.args=list(mult=1),
	geom="errorbar",width=0.2)+
	theme_bw(base_size=8)+
	ylab('GPE, \\unit{\\joule}')+
	theme(legend.position="none",
	axis.title.x=element_blank())
ggsave('fig2c.svg',plot=fig2c,width=3.4167,height=2,units="in")

# check stats
model1 <- aov(h_m~design,data)
model2 <- aov(m_kg~design,data)
model3 <- aov(GPE~design,data)
print(tidy(model1))
print(tidy(model2))
print(tidy(model3))

library(rstatix)
print(t_test(data,h_m~design))
print(t_test(data,m_kg~design))
print(t_test(data,GPE~design))
