# This code corresponds to Fig S5 in Alexander et al.
# It creates Fig 2 (spatial interpolation of hybrid zone movement)  in the 
# main manuscript of Alexander et al.

# 1. Loading required libraries and scripts
# library(devtools)
# Before the next step need to have openmp installed if on mac. Did this by:
# brew install llvm
# mkdir -p ~/.R
# vi ~/.R/Makevars # Then inserting the following
# C=/usr/local/opt/llvm/bin/clang
# CXX=/usr/local/opt/llvm/bin/clang++
#devtools::install_github("bcm-uga/TESS3_encho_sen")
library(tidyverse)
library(tess3r)
library(ggrepel)
source("http://membres-timc.imag.fr/Olivier.Francois/POPSutilities.R")
library(fields)

# 2. Setwd
setwd("chickadee/output/")

# 3. Reading in data (tab delimited), dropping row
# corresponding to sample 99788 with low coverage
temp <- read_tsv("../data/Table_S1.txt")
temp <- temp %>% filter(Catalog_number!="99788")

# 4. Creating variables with our variables of interest
mod_coords <- as.matrix(temp %>% filter(Sampling_period=="MODERN") %>% filter(Included_in_tess3r=="YES") %>% select(DecimalLongitude,DecimalLatitude))
  
hist_coords <- as.matrix(temp %>% filter(Sampling_period=="SMITHSONIAN") %>% filter(Included_in_tess3r=="YES") %>% select(DecimalLongitude,DecimalLatitude))

mod_q.matrix <- as.matrix(temp %>% filter(Sampling_period=="MODERN") %>% filter(Included_in_tess3r=="YES") %>% select(BC_genetic_cluster_assignment,CC_genetic_cluster_assignment))

hist_q.matrix <-  as.matrix(temp %>% filter(Sampling_period=="SMITHSONIAN") %>% filter(Included_in_tess3r=="YES") %>% select(BC_genetic_cluster_assignment,CC_genetic_cluster_assignment))

grid <- createGrid(min(mod_coords[,1],hist_coords[,1]),max(mod_coords[,1], hist_coords[,1]),min(mod_coords[,2], hist_coords[,2]),max(mod_coords[,2], hist_coords[,2]),2000,2000)

# 5. Creating the base maps of interpolated genome make up
maps(matrix = mod_q.matrix, mod_coords, grid, method = "max", colorGradientsList = list(c("gray95",brewer.pal(9,"Reds")),c("gray95",brewer.pal(9,"Blues"))))
# Manually exported as a *.png 2000*2000 pixels in size, Fig_2_modern_baselayer.png in output folder

maps(matrix = hist_q.matrix, hist_coords, grid, method = "max", colorGradientsList = list(c("gray95",brewer.pal(9,"Reds")),c("gray95",brewer.pal(9,"Blues"))))
# Manually exported as a *.png 2000*2000 pixels in size, Fig_2_historical_baselayer.png in output folder

# 6. Creating site-specific points to manually overlay on base maps
grid <- as.data.frame(grid)

modern_mymarkers <- temp %>% filter(Sampling_period=="MODERN") %>% filter(Included_in_tess3r=="YES") %>% group_by(Location_code, DecimalLongitude, DecimalLatitude) %>% 
  summarise(BCsum=sum(BC_genetic_cluster_assignment),CCsum=sum(CC_genetic_cluster_assignment),max_admixture=max(min(BC_genetic_cluster_assignment,CC_genetic_cluster_assignment))) %>%
  mutate(r=BCsum+CCsum,hybrid_status=ifelse(((BCsum/r)>=0.95 & max_admixture <= 0.05),"BC",ifelse(((CCsum/r)>=0.95 & max_admixture <= 0.05),"CC","Hybrid"))) 

modern <- ggplot(grid, aes(x = grid$V1, y = grid$V2)) + geom_point(modern_mymarkers, mapping=aes(x = DecimalLongitude, y = DecimalLatitude,fill = hybrid_status), shape=21,color = "#F2B01E",size=24, stroke = 2)+scale_fill_manual(values=c("#CE1B26", "#15326C","#9437FF")) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank(), axis.line = element_line(colour = "black")) + theme(legend.position="none") + geom_label_repel(data=modern_mymarkers, aes(x = DecimalLongitude, y = DecimalLatitude, label = modern_mymarkers$Location_code, color= hybrid_status),fill="#F2B01E",segment.size=0,size=15, force=1.5, max.iter = 40000)+scale_color_manual(values=c("#CE1B26", "#15326C","#9437FF"))+
  xlim(min(grid$V1),max(grid$V1)) +
  ylim(min(grid$V2),max(grid$V2))
# Manually exported as a *.png 2000*2000 pixels in size, Fig_2_modern_overlay.png in output folder

historical_mymarkers <- temp %>% filter(Sampling_period=="SMITHSONIAN") %>% filter(Included_in_tess3r=="YES") %>% group_by(Location_code, DecimalLongitude, DecimalLatitude) %>% 
  summarise(BCsum=sum(BC_genetic_cluster_assignment),CCsum=sum(CC_genetic_cluster_assignment),max_admixture=max(min(BC_genetic_cluster_assignment,CC_genetic_cluster_assignment))) %>%
  mutate(r=BCsum+CCsum,hybrid_status=ifelse(((BCsum/r)>=0.95 & max_admixture <= 0.05),"BC",ifelse(((CCsum/r)>=0.95 & max_admixture <= 0.05),"CC","Hybrid"))) 

historical <- ggplot(grid, aes(x = grid$V1, y = grid$V2)) + geom_point(historical_mymarkers, mapping=aes(x = DecimalLongitude, y = DecimalLatitude,fill = hybrid_status), shape=21,color = "#F2B01E",size=24, stroke = 2)+scale_fill_manual(values=c("#CE1B26", "#15326C","#9437FF")) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank(), axis.line = element_line(colour = "black")) + theme(legend.position="none") + geom_label_repel(data=historical_mymarkers, aes(x = DecimalLongitude, y = DecimalLatitude, label = historical_mymarkers$Location_code, color= hybrid_status),fill="#F2B01E",segment.size=0,size=15, force=1.5, max.iter = 40000)+scale_color_manual(values=c("#CE1B26", "#15326C","#9437FF"))+
  xlim(min(grid$V1),max(grid$V1)) +
  ylim(min(grid$V2),max(grid$V2))
# Manually exported as a *.png 2000*2000 pixels in size, Fig_2_historical_overlay.png in output folder

sessionInfo()
#R version 3.6.2 (2019-12-12)
#Platform: x86_64-apple-darwin15.6.0 (64-bit)
#Running under: macOS Sierra 10.12.6

#Matrix products: default
#BLAS:   /System/Library/Frameworks/Accelerate.framework/Versions/A/Frameworks/vecLib.framework/Versions/A/libBLAS.dylib
#LAPACK: /Library/Frameworks/R.framework/Versions/3.6/Resources/lib/libRlapack.dylib

#locale:
#  [1] en_US.UTF-8/en_US.UTF-8/en_US.UTF-8/C/en_US.UTF-8/en_US.UTF-8

#attached base packages:
#  [1] grid      stats     graphics  grDevices utils     datasets 
#[7] methods   base     

#other attached packages:
#  [1] fields_10.3        maps_3.3.0         spam_2.5-1        
#[4] dotCall64_1.0-0    RColorBrewer_1.1-2 tess3r_1.1.0      
#[7] forcats_0.4.0      stringr_1.4.0      purrr_0.3.3       
#[10] tidyr_1.0.2        tibble_2.1.3       tidyverse_1.3.0   
#[13] ggrepel_0.8.1      ggmap_3.0.0        scatterpie_0.1.4  
#[16] ggplot2_3.2.1      dplyr_0.8.4        readr_1.3.1       

#loaded via a namespace (and not attached):
#  [1] Rcpp_1.0.3          lubridate_1.7.4     lattice_0.20-38    
#[4] png_0.1-7           assertthat_0.2.1    digest_0.6.24      
#[7] utf8_1.1.4          ggforce_0.3.1       R6_2.4.1           
#[10] cellranger_1.1.0    plyr_1.8.5          backports_1.1.5    
#[13] reprex_0.3.0        httr_1.4.1          pillar_1.4.3       
#[16] RgoogleMaps_1.4.5.3 rlang_0.4.4         lazyeval_0.2.2     
#[19] curl_4.3            readxl_1.3.1        rstudioapi_0.11    
#[22] Matrix_1.2-18       labeling_0.3        RcppEigen_0.3.3.7.0
#[25] polyclip_1.10-0     munsell_0.5.0       broom_0.5.4        
#[28] compiler_3.6.2      modelr_0.1.5        pkgconfig_2.0.3    
#[31] tidyselect_1.0.0    fansi_0.4.1         crayon_1.3.4       
#[34] dbplyr_1.4.2        withr_2.1.2         MASS_7.3-51.5      
#[37] bitops_1.0-6        nlme_3.1-144        jsonlite_1.6.1     
#[40] gtable_0.3.0        lifecycle_0.1.0     DBI_1.1.0          
#[43] magrittr_1.5        scales_1.1.0        cli_2.0.1          
#[46] stringi_1.4.5       farver_2.0.3        fs_1.3.1           
#[49] sp_1.3-2            xml2_1.2.2          ellipsis_0.3.0     
#[52] rvcheck_0.1.7       generics_0.0.2      vctrs_0.2.2        
#[55] rjson_0.2.20        tools_3.6.2         glue_1.3.1         
#[58] tweenr_1.0.1        hms_0.5.3           jpeg_0.1-8.1       
#[61] colorspace_1.4-1    BiocManager_1.30.10 rvest_0.3.5        
#[64] haven_2.2.0        
