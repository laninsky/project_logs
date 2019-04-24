# 1. Loading necessary libraries
library(readr)
library(dplyr)
library(ggplot2)
# install.packages("scatterpie")
library(scatterpie)
# if(!requireNamespace("devtools")) install.packages("devtools")
# devtools::install_github("dkahle/ggmap", ref = "tidyup")
library(ggmap)
# devtools::install_github("slowkow/ggrepel")
library(ggrepel)

# 2. Setwd
setwd("chickadee/output/")

# 3. Reading in data (tab delimited), dropping last blank row
temp <- read_table2("../data/S2.txt")
temp <- temp[1:165,]

# 4. Creating a new table with the variables we are interested in
reduced_table <- temp %>% filter(Sampling_period=="MODERN" | Sampling_period=="SMITHSONIAN") %>% select(Catalog_number, BC_genetic_cluster_assignment, CC_genetic_cluster_assignment, DecimalLongitude, DecimalLatitude, Sampling_period, Location_code,Included_in_tess3r)

reduced_table # should print out something similar to following
## A tibble: 160 x 7
#Catalog_number BC_genetic_clus… CC_genetic_clus… DecimalLongitude DecimalLatitude Sampling_period
#<dbl>            <dbl>            <dbl>            <dbl>           <dbl> <chr>          
#  1         132041          0.002              0.998            -94.1            38.0 MODERN         
#2         132042          0.00267            0.997            -94.1            38.0 MODERN         
#3         132043          0.00267            0.997            -94.1            38.0 MODERN         
#4         132044          0.002              0.998            -94.1            38.0 MODERN         
#5         132045          0.01               0.99             -94.1            38.0 MODERN         
#6         132046          0.001              0.999            -94.1            38.0 MODERN         
#7         132047          0.00367            0.996            -94.1            38.0 MODERN         
#8         132048          0.003              0.997            -93.7            37.9 MODERN         
#9         132049          0.00367            0.996            -93.7            37.9 MODERN         
#10         132050          0.00367            0.996            -93.7            37.9 MODERN         
## … with 150 more rows, and 1 more variable: Location_code <chr>

# 5. Filtering data for each sampling period
modern <- reduced_table %>% filter(Sampling_period=="MODERN") %>% group_by(DecimalLongitude,DecimalLatitude,Location_code,Included_in_tess3r) %>% 
  summarise(BCsum=sum(BC_genetic_cluster_assignment),CCsum=sum(CC_genetic_cluster_assignment)) %>%
  mutate(r=BCsum+CCsum,status=ifelse((BCsum/r)>=0.95,"BC",ifelse((CCsum/r)>=0.95,"CC","Hybrid"))) 

historical <- reduced_table %>% filter(Sampling_period=="SMITHSONIAN") %>% group_by(DecimalLongitude,DecimalLatitude,Location_code,Included_in_tess3r) %>% 
  summarise(BCsum=sum(BC_genetic_cluster_assignment),CCsum=sum(CC_genetic_cluster_assignment)) %>%
  mutate(r=BCsum+CCsum,status=ifelse((BCsum/r)>=0.95,"BC",ifelse((CCsum/r)>=0.95,"CC","Hybrid"))) 

# 6. Making a box containing all sites in both modern and historical samples
# along with some buffer
latbound <- max(reduced_table$DecimalLatitude)-min(reduced_table$DecimalLatitude)+0.2
longbound <- max(reduced_table$DecimalLongitude)-min(reduced_table$DecimalLongitude)+0.2
if (latbound > longbound) {
  midlong <- (max(reduced_table$DecimalLongitude)+min(reduced_table$DecimalLongitude))/2
  minlong <- midlong-longbound/2
  maxlong <- midlong+longbound/2
  minlat <- min(reduced_table$DecimalLatitude)-0.1
  maxlat <- max(treduced_table$DecimalLatitude)+0.1
} else {
  midlat <- (max(reduced_table$DecimalLatitude)+min(reduced_table$DecimalLatitude))/2
  minlat <- midlat-longbound/2
  maxlat <- midlat+longbound/2
  minlong <- min(reduced_table$DecimalLongitude)-0.1
  maxlong <- max(reduced_table$DecimalLongitude)+0.1
}

sbbox <- make_bbox(lon=c(minlong,maxlong), lat=c(minlat,maxlat),f=0)

# 7. Using the sbbox object to retrieve a map covering the sample sites
sq_map <- get_map(location = sbbox, maptype = "terrain-background", source = "stamen", crop=TRUE)

# 8. Mapping pies on to underlying maps using scatterpie
modernpies <- ggmap(sq_map) + 
  geom_scatterpie(aes(x=DecimalLongitude,y=DecimalLatitude,r=r/100),
                  data=modern,cols=c("BCsum","CCsum"),sorted_by_radius = TRUE) +
  scale_fill_manual(values=c("#CE1B26", "#15326C")) + theme(legend.position="none") +
  coord_fixed(ratio=1)+ xlab("Longitude") + ylab("Latitude") + 
  theme(axis.text=element_text(size=16), axis.title=element_text(size=18,face="bold"),
        panel.border=element_rect(fill = NA)) + 
  geom_scatterpie_legend(modern$r/100,x=-94.4,y=37.6,labeller=function(x) 100*x)

modernpies + geom_label_repel(modern,mapping=aes(fontface="bold",x=DecimalLongitude,y=DecimalLatitude,label=Location_code,fill=Included_in_tess3r,color=status),size=8,force=20, max.iter = 40000, segment.color="black") +
  scale_fill_manual(values=c("#CE1B26", "#15326C","#FFFFFF", "#F2B01F")) + 
  scale_color_manual(values=c("#CE1B26", "#15326C","#9437FF")) 

ggsave(filename="Fig_1_modern_aggregated_by_site.pdf",plot = last_plot(),width=12.804,height=10.351,units="in")

historicalpies <- ggmap(sq_map) +  
  geom_scatterpie(aes(x=DecimalLongitude,y=DecimalLatitude,r=r/100),
                  data=historical,cols=c("BCsum","CCsum"),sorted_by_radius=T) +
  scale_fill_manual(values=c("#CE1B26", "#15326C")) + theme(legend.position="none") +
  coord_fixed(ratio=1) + xlab("Longitude") + ylab("Latitude") + 
  theme(axis.text=element_text(size=16), axis.title=element_text(size=18,face="bold"),
        panel.border=element_rect(fill = NA)) +
  geom_scatterpie_legend(historical$r/100,x=-93.76,y=38.25,labeller=function(x) 100*x)

historicalpies + geom_label_repel(historical,mapping=aes(fontface="bold",x=DecimalLongitude,y=DecimalLatitude,label=Location_code,fill=Included_in_tess3r,color=status),size=8, force=10, max.iter = 40000, segment.color="black") +
  scale_fill_manual(values=c("#CE1B26", "#15326C","#FFFFFF", "#F2B01F")) + 
  scale_color_manual(values=c("#CE1B26", "#15326C","#9437FF")) 

ggsave(filename="Fig_1_historical_aggregated_by_site.pdf",plot = last_plot(),width=12.804,height=10.351,units="in")

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
#  [1] bindrcpp_0.2.2     ggrepel_0.8.0.9000 ggmap_2.7.904      scatterpie_0.1.2   ggplot2_3.1.0.9000
#[6] dplyr_0.7.8        readr_1.3.1       
#
#loaded via a namespace (and not attached):
#  [1] Rcpp_1.0.0        plyr_1.8.4        pillar_1.3.1      compiler_3.5.1    bindr_0.1.1      
#[6] bitops_1.0-6      tools_3.5.1       digest_0.6.18     tibble_2.0.1      gtable_0.2.0     
#[11] pkgconfig_2.0.2   png_0.1-7         rlang_0.3.1       cli_1.0.1         rstudioapi_0.9.0 
#[16] rvcheck_0.1.3     yaml_2.2.0        stringr_1.3.1     httr_1.4.0        withr_2.1.2      
#[21] RgoogleMaps_1.4.3 hms_0.4.2         grid_3.5.1        tidyselect_0.2.5  glue_1.3.0       
#[26] R6_2.3.0          jpeg_0.1-8        fansi_0.4.0       purrr_0.3.0       tweenr_1.0.1     
#[31] farver_1.1.0      tidyr_0.8.2       magrittr_1.5      scales_1.0.0      MASS_7.3-51.1    
#[36] units_0.6-2       assertthat_0.2.0  ggforce_0.1.3     colorspace_1.4-0  labeling_0.3     
#[41] utf8_1.1.4        stringi_1.2.4     lazyeval_0.2.1    munsell_0.5.0     rjson_0.2.20     
#[46] crayon_1.3.4