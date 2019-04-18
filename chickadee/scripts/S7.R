# 1. Loading required libraries and scripts
#library(devtools)
#devtools::install_github("bcm-uga/TESS3_encho_sen")
library(tess3r)
library(ggplot2)
library(ggrepel)
source("http://membres-timc.imag.fr/Olivier.Francois/POPSutilities.R")

# 2. Setwd
setwd("chickadees/output/")

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

modern <- ggplot(grid, aes(x = grid$V1, y = grid$V2)) + geom_point(modern_mymarkers, mapping=aes(x = DecimalLongitude, y = DecimalLatitude,fill = hybrid_status), shape=21,color = "gold",size=24, stroke = 2)+scale_fill_manual(values=c("#CE1B26", "#15326C","#9437FF")) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank(), axis.line = element_line(colour = "black")) + theme(legend.position="none") + geom_label_repel(data=modern_mymarkers, aes(x = DecimalLongitude, y = DecimalLatitude, label = modern_mymarkers$Location_code, color= hybrid_status),fill="gold",segment.size=0,size=15, force=1.5, max.iter = 40000)+scale_color_manual(values=c("#CE1B26", "#15326C","#9437FF"))+
  xlim(min(grid$V1),max(grid$V1)) +
  ylim(min(grid$V2),max(grid$V2))
# Manually exported as a *.png 2000*2000 pixels in size

historical_mymarkers <- temp %>% filter(Sampling_period=="SMITHSONIAN") %>% filter(Included_in_tess3r=="YES") %>% group_by(Location_code, DecimalLongitude, DecimalLatitude) %>% 
  summarise(BCsum=sum(BC_genetic_cluster_assignment),CCsum=sum(CC_genetic_cluster_assignment),max_admixture=max(min(BC_genetic_cluster_assignment,CC_genetic_cluster_assignment))) %>%
  mutate(r=BCsum+CCsum,hybrid_status=ifelse(((BCsum/r)>=0.95 & max_admixture <= 0.05),"BC",ifelse(((CCsum/r)>=0.95 & max_admixture <= 0.05),"CC","Hybrid"))) 

historical <- ggplot(grid, aes(x = grid$V1, y = grid$V2)) + geom_point(historical_mymarkers, mapping=aes(x = DecimalLongitude, y = DecimalLatitude,fill = hybrid_status), shape=21,color = "gold",size=24, stroke = 2)+scale_fill_manual(values=c("#CE1B26", "#15326C","#9437FF")) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank(), axis.line = element_line(colour = "black")) + theme(legend.position="none") + geom_label_repel(data=historical_mymarkers, aes(x = DecimalLongitude, y = DecimalLatitude, label = historical_mymarkers$Location_code, color= hybrid_status),fill="gold",segment.size=0,size=15, force=1.5, max.iter = 40000)+scale_color_manual(values=c("#CE1B26", "#15326C","#9437FF"))+
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
#  [1] grid      stats     graphics  grDevices utils     datasets  methods   base     
#
#other attached packages:
#  [1] RColorBrewer_1.1-2 fields_9.6         maps_3.3.0         spam_2.2-1         dotCall64_1.0-0   
#[6] tess3r_1.1.0       usethis_1.4.0      devtools_2.0.1     bindrcpp_0.2.2     ggrepel_0.8.0.9000
#[11] ggmap_2.7.904      scatterpie_0.1.2   ggplot2_3.1.0.9000 dplyr_0.7.8        readr_1.3.1       
#
#loaded via a namespace (and not attached):
#  [1] Rcpp_1.0.1          lattice_0.20-38     tidyr_0.8.2         prettyunits_1.0.2   png_0.1-7          
#[6] ps_1.3.0            assertthat_0.2.0    rprojroot_1.3-2     digest_0.6.18       utf8_1.1.4         
#[11] ggforce_0.1.3       R6_2.3.0            plyr_1.8.4          backports_1.1.3     httr_1.4.0         
#[16] pillar_1.3.1        RgoogleMaps_1.4.3   rlang_0.3.1         lazyeval_0.2.1      curl_3.3           
#[21] rstudioapi_0.9.0    callr_3.1.1         Matrix_1.2-15       desc_1.2.0          labeling_0.3       
#[26] stringr_1.3.1       RcppEigen_0.3.3.5.0 munsell_0.5.0       compiler_3.5.1      pkgconfig_2.0.2    
#[31] pkgbuild_1.0.2      tidyselect_0.2.5    tibble_2.0.1        fansi_0.4.0         crayon_1.3.4       
#[36] withr_2.1.2         MASS_7.3-51.1       bitops_1.0-6        gtable_0.2.0        magrittr_1.5       
#[41] units_0.6-2         scales_1.0.0        cli_1.0.1           stringi_1.2.4       farver_1.1.0       
#[46] fs_1.2.6            remotes_2.0.2       rvcheck_0.1.3       rjson_0.2.20        tools_3.5.1        
#[51] glue_1.3.0          tweenr_1.0.1        purrr_0.3.0         hms_0.4.2           jpeg_0.1-8         
#[56] processx_3.2.1      pkgload_1.0.2       yaml_2.2.0          colorspace_1.4-0    sessioninfo_1.1.1  
#[61] memoise_1.1.0       bindr_0.1.1 
