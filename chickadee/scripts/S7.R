# 1. Loading required libraries and scripts
#library(devtools)
#devtools::install_github("bcm-uga/TESS3_encho_sen")
library(tidyverse)
library(tess3r)
library(ggplot2)
library(ggrepel)
source("http://membres-timc.imag.fr/Olivier.Francois/POPSutilities.R")

# 2. Setwd
setwd("chickadee/output/")

# 3. Reading in data (tab delimited), dropping last blank row
temp <- read_table2("../data/S2.txt")
temp <- temp[1:165,]

# 4. Creating variables with our variables of interest
mod_coords <- as.matrix(temp %>% filter(Sampling_period=="MODERN") %>% filter(Included_in_tess3r=="YES") %>% select(DecimalLongitude,DecimalLatitude))
  
hist_coords <- as.matrix(temp %>% filter(Sampling_period=="SMITHSONIAN") %>% filter(Included_in_tess3r=="YES") %>% select(DecimalLongitude,DecimalLatitude))

mod_q.matrix <- as.matrix(temp %>% filter(Sampling_period=="MODERN") %>% filter(Included_in_tess3r=="YES") %>% select(BC_genetic_cluster_assignment,CC_genetic_cluster_assignment))

hist_q.matrix <-  as.matrix(temp %>% filter(Sampling_period=="SMITHSONIAN") %>% filter(Included_in_tess3r=="YES") %>% select(BC_genetic_cluster_assignment,CC_genetic_cluster_assignment))

grid <- createGrid(min(mod_coords[,1],hist_coords[,1]),max(mod_coords[,1], hist_coords[,1]),min(mod_coords[,2], hist_coords[,2]),max(mod_coords[,2], hist_coords[,2]),2000,2000)

# 5. Creating the base maps of interpolated genome make up
maps(matrix = mod_q.matrix, mod_coords, grid, method = "max", colorGradientsList = list(c("gray95",brewer.pal(9,"Reds")),c("gray95",brewer.pal(9,"Blues"))))
# Manually exported as a *.png 2000*2000 pixels in size

maps(matrix = hist_q.matrix, hist_coords, grid, method = "max", colorGradientsList = list(c("gray95",brewer.pal(9,"Reds")),c("gray95",brewer.pal(9,"Blues"))))
# Manually exported as a *.png 2000*2000 pixels in size

# 6. Creating site-specific points to manually overlay on base maps
grid <- as.data.frame(grid)

modern_mymarkers <- temp %>% filter(Sampling_period=="MODERN") %>% filter(Included_in_tess3r=="YES") %>% group_by(Location_code, DecimalLongitude, DecimalLatitude) %>% 
  summarise(BCsum=sum(BC_genetic_cluster_assignment),CCsum=sum(CC_genetic_cluster_assignment),max_admixture=max(min(BC_genetic_cluster_assignment,CC_genetic_cluster_assignment))) %>%
  mutate(r=BCsum+CCsum,hybrid_status=ifelse(((BCsum/r)>=0.95 & max_admixture <= 0.05),"BC",ifelse(((CCsum/r)>=0.95 & max_admixture <= 0.05),"CC","Hybrid"))) 

modern <- ggplot(grid, aes(x = grid$V1, y = grid$V2)) + geom_point(modern_mymarkers, mapping=aes(x = DecimalLongitude, y = DecimalLatitude,fill = hybrid_status), shape=21,color = "#F2B01E",size=24, stroke = 2)+scale_fill_manual(values=c("#CE1B26", "#15326C","#9437FF")) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank(), axis.line = element_line(colour = "black")) + theme(legend.position="none") + geom_label_repel(data=modern_mymarkers, aes(x = DecimalLongitude, y = DecimalLatitude, label = modern_mymarkers$Location_code, color= hybrid_status),fill="#F2B01E",segment.size=0,size=15, force=1.5, max.iter = 40000)+scale_color_manual(values=c("#CE1B26", "#15326C","#9437FF"))+
  xlim(min(grid$V1),max(grid$V1)) +
  ylim(min(grid$V2),max(grid$V2))
# Manually exported as a *.png 2000*2000 pixels in size

historical_mymarkers <- temp %>% filter(Sampling_period=="SMITHSONIAN") %>% filter(Included_in_tess3r=="YES") %>% group_by(Location_code, DecimalLongitude, DecimalLatitude) %>% 
  summarise(BCsum=sum(BC_genetic_cluster_assignment),CCsum=sum(CC_genetic_cluster_assignment),max_admixture=max(min(BC_genetic_cluster_assignment,CC_genetic_cluster_assignment))) %>%
  mutate(r=BCsum+CCsum,hybrid_status=ifelse(((BCsum/r)>=0.95 & max_admixture <= 0.05),"BC",ifelse(((CCsum/r)>=0.95 & max_admixture <= 0.05),"CC","Hybrid"))) 

historical <- ggplot(grid, aes(x = grid$V1, y = grid$V2)) + geom_point(historical_mymarkers, mapping=aes(x = DecimalLongitude, y = DecimalLatitude,fill = hybrid_status), shape=21,color = "#F2B01E",size=24, stroke = 2)+scale_fill_manual(values=c("#CE1B26", "#15326C","#9437FF")) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank(), axis.line = element_line(colour = "black")) + theme(legend.position="none") + geom_label_repel(data=historical_mymarkers, aes(x = DecimalLongitude, y = DecimalLatitude, label = historical_mymarkers$Location_code, color= hybrid_status),fill="#F2B01E",segment.size=0,size=15, force=1.5, max.iter = 40000)+scale_color_manual(values=c("#CE1B26", "#15326C","#9437FF"))+
  xlim(min(grid$V1),max(grid$V1)) +
  ylim(min(grid$V2),max(grid$V2))
# Manually exported as a *.png 2000*2000 pixels in size

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
#  [1] grid      stats     graphics  grDevices utils     datasets  methods  
#[8] base     
#
#other attached packages:
#  [1] bindrcpp_0.2.2     forcats_0.3.0      stringr_1.3.1      dplyr_0.7.8       
#[5] purrr_0.3.0        readr_1.3.1        tidyr_0.8.2        tibble_2.0.1      
#[9] tidyverse_1.2.1    RColorBrewer_1.1-2 fields_9.6         maps_3.3.0        
#[13] spam_2.2-1         dotCall64_1.0-0    ggrepel_0.8.0.9000 ggplot2_3.1.0.9000
#[17] tess3r_1.1.0      
#
#loaded via a namespace (and not attached):
#  [1] RcppEigen_0.3.3.5.0 tidyselect_0.2.5    haven_2.0.0        
#[4] lattice_0.20-38     colorspace_1.4-0    generics_0.0.2     
#[7] yaml_2.2.0          rlang_0.3.1         pillar_1.3.1       
#[10] glue_1.3.0          withr_2.1.2         modelr_0.1.2       
#[13] readxl_1.2.0        bindr_0.1.1         munsell_0.5.0      
#[16] gtable_0.2.0        cellranger_1.1.0    rvest_0.3.2        
#[19] labeling_0.3        broom_0.5.1         Rcpp_1.0.1         
#[22] scales_1.0.0        backports_1.1.3     jsonlite_1.6       
#[25] hms_0.4.2           stringi_1.2.4       cli_1.0.1          
#[28] tools_3.5.1         magrittr_1.5        lazyeval_0.2.1     
#[31] crayon_1.3.4        pkgconfig_2.0.2     Matrix_1.2-15      
#[34] xml2_1.2.0          lubridate_1.7.4     assertthat_0.2.0   
#[37] httr_1.4.0          rstudioapi_0.9.0    R6_2.3.0           
#[40] nlme_3.1-137        compiler_3.5.1  