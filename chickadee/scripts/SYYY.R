# 1. Loading required library
library(tidyverse)

# 2. Setwd
setwd("chickadee/output/")

# 3. Reading in data (tab delimited), dropping last blank row
temp <- read_tsv("../data/S2.txt")
temp <- temp[1:165,]

ggplot(temp,aes(y=Weight,x=BC_genetic_cluster_assignment,fill=BC_genetic_cluster_assignment,shape=Sampling_period),color="black") + 
  geom_smooth(method = "lm",color="black") +
  geom_point(size=5) +  
  scale_x_continuous(limits=c(0,1),expand = c(0, 0),name = str_wrap("Assignment to BC genetic cluster",width=40)) + 
  scale_y_continuous(expand = c(0, 0),name=str_wrap("Weight (g)", width=40))  + 
  scale_fill_gradientn(colors=c("#15326C","#9437FF","#CE1B26")) + 
  scale_shape_manual(values = c(21,22,23,24)) +
  theme_bw(base_size = 16) +
  theme(aspect.ratio = 1) +
  theme(legend.position = "none") +
  coord_cartesian(clip = 'off') 

temptemp <- temp %>% 
  filter(!is.na(Weight)) %>% filter(!is.na(BC_genetic_cluster_assignment)) %>% 
  select(Weight,BC_genetic_cluster_assignment) 

    cor(temptemp[,1],temptemp[,2])^2

tempwingtail <- temp %>% 
  mutate(Tail=as.numeric(Tail)) %>% 
  filter(!is.na(Wing)) %>% 
  filter(!is.na(Tail)) %>%  
  filter(!is.na(BC_genetic_cluster_assignment)) %>% 
  mutate(Wing_Tail_Ratio=(Wing/Tail))
    
ggplot(tempwingtail,aes(x=Wing_Tail_Ratio,y=BC_genetic_cluster_assignment,fill=BC_genetic_cluster_assignment),color="black") + 
      geom_smooth(method = "lm",color="black") +
      geom_point(tempwingtail,size=5,mapping=aes(shape=Sampling_period)) +  
      scale_x_continuous(expand = c(0, 0),name = str_wrap("Wing/Tail ratio",width=40)) + 
      scale_y_reverse(expand = c(0, 0),name=str_wrap("Assignment to BC genetic cluster", width=40))  + 
      scale_fill_gradientn(colors=c("#15326C","#9437FF","#CE1B26")) + 
      scale_shape_manual(values = c(21,22,23,24)) +
      theme_bw(base_size = 16) +
      theme(aspect.ratio = 1) +
      theme(legend.position = "none") +
      coord_cartesian(clip = 'off') 
    