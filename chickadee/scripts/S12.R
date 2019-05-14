# 1. Loading required library
library(tidyverse)

# 2. Setwd
setwd("chickadee/output/")

# 3. Reading in data (tab delimited), dropping last blank row
temp <- read_tsv("../data/S2.txt")
temp <- temp[1:165,]

# 4. Plot and rsq for Weight vs cluster assignment. Saved plot as 1000 pixels in width.
# FigS4_weight_cluster.png in output folder
ggplot(temp,aes(y=Weight,x=BC_genetic_cluster_assignment,fill=BC_genetic_cluster_assignment,shape=Sampling_period),color="black") + 
  geom_smooth(method = "lm",color="black") +
  geom_point(size=10) +  
  scale_x_continuous(limits=c(0,1),expand = c(0, 0),name = str_wrap("Assignment to BC genetic cluster",width=20)) + 
  scale_y_continuous(expand = c(0, 0),name=str_wrap("Weight (g)", width=20))  + 
  scale_fill_gradientn(colors=c("#15326C","#9437FF","#CE1B26")) + 
  scale_shape_manual(values = c(21,22,23,24)) +
  theme_bw(base_size = 40) +
  theme(aspect.ratio = 1) +
  theme(legend.position = "none") +
  coord_cartesian(clip = 'off') + 
  theme(axis.title=element_text(size=48,face="bold"))

temptemp <- temp %>% 
  filter(!is.na(Weight)) %>% filter(!is.na(BC_genetic_cluster_assignment)) %>% 
  select(Weight,BC_genetic_cluster_assignment) 

cor(temptemp[,1],temptemp[,2])^2

# 5. Plot and rsq for Wing/Tail ratio vs cluster assignment. Saved plot as 1000 pixels in width.
# FigS4_cluster_wingtailratio.png in output folder
tempwingtail <- temp %>% 
  mutate(Tail=as.numeric(Tail)) %>% 
  filter(!is.na(Wing)) %>% 
  filter(!is.na(Tail)) %>%  
  filter(!is.na(BC_genetic_cluster_assignment)) %>% 
  mutate(Wing_Tail_Ratio=(Wing/Tail))
    
ggplot(tempwingtail,aes(x=Wing_Tail_Ratio,y=BC_genetic_cluster_assignment,fill=BC_genetic_cluster_assignment),color="black") + 
      geom_smooth(method = "lm",color="black") +
      geom_point(tempwingtail,size=10,mapping=aes(shape=Sampling_period)) +  
      scale_x_reverse(expand = c(0, 0),name = str_wrap("Wing/Tail ratio",width=20)) + 
      scale_y_continuous(limits=c(0,1),expand = c(0, 0),name=str_wrap("Assignment to BC genetic cluster", width=20))  + 
      scale_fill_gradientn(colors=c("#15326C","#9437FF","#CE1B26")) + 
      scale_shape_manual(values = c(21,22,23,24)) +
      theme_bw(base_size = 40) +
      theme(aspect.ratio = 1) +
      theme(legend.position = "none") +
      coord_cartesian(clip = 'off') + 
      theme(axis.title=element_text(size=48,face="bold"))

cor(tempwingtail$Wing_Tail_Ratio,tempwingtail$BC_genetic_cluster_assignment)^2    

# 6. Plot and rsq for Wing/Tail ratio vs Weight. Saved plot as 1000 pixels in width.
# FigS4_weight_wingtailratio.png in output folder
ggplot(tempwingtail,aes(x=Wing_Tail_Ratio,y=Weight,fill=BC_genetic_cluster_assignment),color="black") + 
  geom_smooth(method = "lm",color="black") +
  geom_point(tempwingtail,size=10,mapping=aes(shape=Sampling_period)) +  
  scale_x_reverse(expand = c(0, 0),name = str_wrap("Wing/Tail ratio",width=20)) + 
  scale_y_continuous(expand = c(0, 0),name=str_wrap("Weight (g)", width=20))  + 
  scale_fill_gradientn(colors=c("#15326C","#9437FF","#CE1B26")) + 
  scale_shape_manual(values = c(21,22,23,24)) +
  theme_bw(base_size = 40) +
  theme(aspect.ratio = 1) +
  theme(legend.position = "none") +
  coord_cartesian(clip = 'off') + 
  theme(axis.title=element_text(size=48,face="bold"))

tempwingtail <- tempwingtail %>% 
  filter(!is.na(Weight))

cor(tempwingtail$Wing_Tail_Ratio,tempwingtail$Weight)^2  

# Manually arranged these plots in ppt/illustrator along with rsq

sessionInfo()
#R version 3.5.1 (2018-07-02)
#Platform: x86_64-apple-darwin15.6.0 (64-bit)
#Running under: macOS Sierra 10.12.6
#
#Matrix products: default
#BLAS: /System/Library/Frameworks/Accelerate.framework/Versions/A/Frameworks/vecLib.framework/Versions/A/libBLAS.dylib
#LAPACK: /Library/Frameworks/R.framework/Versions/3.5/Resources/lib/libRlapack.dylib
#
#locale:
#  [1] en_NZ.UTF-8/en_NZ.UTF-8/en_NZ.UTF-8/C/en_NZ.UTF-8/en_NZ.UTF-8
#
#attached base packages:
#  [1] stats     graphics  grDevices utils     datasets  methods   base     
#
#other attached packages:
#  [1] bindrcpp_0.2.2     forcats_0.3.0      stringr_1.3.1      dplyr_0.7.8        purrr_0.3.0        readr_1.3.1       
#[7] tidyr_0.8.2        tibble_2.0.1       ggplot2_3.1.0.9000 tidyverse_1.2.1   
#
#loaded via a namespace (and not attached):
#  [1] Rcpp_1.0.1       cellranger_1.1.0 pillar_1.3.1     compiler_3.5.1   bindr_0.1.1      tools_3.5.1      jsonlite_1#.6    
#[8] lubridate_1.7.4  nlme_3.1-137     gtable_0.2.0     lattice_0.20-38  pkgconfig_2.0.2  rlang_0.3.1      cli_1.0.1       
#[15] rstudioapi_0.9.0 yaml_2.2.0       haven_2.0.0      withr_2.1.2      xml2_1.2.0       httr_1.4.0       generics_0.0.2  
#[22] hms_0.4.2        grid_3.5.1       tidyselect_0.2.5 glue_1.3.0       R6_2.3.0         fansi_0.4.0      readxl_1.2.0    
#[29] modelr_0.1.2     magrittr_1.5     backports_1.1.3  scales_1.0.0     rvest_0.3.2      assertthat_0.2.0 colorspace_1.4-0
#[36] labeling_0.3     utf8_1.1.4       stringi_1.2.4    lazyeval_0.2.1   munsell_0.5.0    broom_0.5.1      crayon_1.3.4 
