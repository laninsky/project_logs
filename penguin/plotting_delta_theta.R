# 1. Load required libraries
library(tidyverse)

#2. Read in and check data
data <- read_tsv("tab_delimited_snapp.txt")
data
## A tibble: 29 x 9
#Replicate Delta_theta Location Location_code Taxa_common_name Taxa_scientific…
#<dbl>       <dbl> <chr>    <chr>         <chr>            <chr>           
#  1        NA    NA       NA       NA            Snares Crested   NA              
#2        NA    NA       NA       NA            Fiordland crest… NA              
#3         1     0.0334  Elephant ELE           Macaroni penguin Eudyptes chryso…
#4         2     0.179   Elephant ELE           Macaroni penguin Eudyptes chryso…
#5         3     0.0395  Elephant ELE           Macaroni penguin Eudyptes chryso…
#6         1     0.0176  SouthGe… SGE           Macaroni penguin Eudyptes chryso…
#7         2     0.00824 SouthGe… SGE           Macaroni penguin Eudyptes chryso…
#8         3    -0.0112  SouthGe… SGE           Macaroni penguin Eudyptes chryso…
#9         1     0.00594 SouthSa… SSI           Macaroni penguin Eudyptes chryso…
#10         2     0.0410  SouthSa… SSI           Macaroni penguin Eudyptes chryso…
## … with 19 more rows, and 3 more variables: Taxa_order_for_graphing <dbl>,
##   Location_order_for_graphing <dbl>, Overall_order <dbl>

# 3. Plot data
ggplot(data,aes(x=factor(Location_code,levels=unique(as.character(data$Location_code))),y=Delta_theta)) + 
  geom_jitter(aes(fill=factor(Taxa_common_name,levels=unique(as.character(data$Taxa_common_name))),shape=as.character(Replicate)),size=5,color="black",height=0,width=0.2) + 
  theme_bw(base_size = 20) + 
  scale_shape_manual(name="Replicate",values=c(21,22,23)) + 
  scale_fill_manual(name="Species",values=c("#E5E523","#4FA253","#B73331","#EBBDBD","#FFB400","#03BAFF", "#3E42C1","#BE00FF")) +
  guides(fill=guide_legend(override.aes=list(shape=21))) +
  xlab("Location") +
  ylab("Δ theta") + 
  theme(axis.text.x = element_text(angle = 90)) +
  theme(axis.title.x=element_text(face="bold")) +
  theme(axis.title.y=element_text(face="bold")) + 
  theme(legend.title = element_text(face="bold"))

sessionInfo()
#R version 3.5.2 (2018-12-20)
#Platform: x86_64-apple-darwin15.6.0 (64-bit)
#Running under: macOS High Sierra 10.13.6
#
#Matrix products: default
#BLAS: /System/Library/Frameworks/Accelerate.framework/Versions/A/Frameworks/vecLib.framework/Versions/A/libBLAS.dylib
#LAPACK: /Library/Frameworks/R.framework/Versions/3.5/Resources/lib/libRlapack.dylib
#
#locale:
#  [1] en_US.UTF-8/en_US.UTF-8/en_US.UTF-8/C/en_US.UTF-8/en_US.UTF-8
#
#attached base packages:
#  [1] stats     graphics  grDevices utils     datasets  methods   base     
#
#other attached packages:
#  [1] forcats_0.3.0      stringr_1.4.0      dplyr_0.8.0.1      purrr_0.3.2       
#[5] readr_1.3.1        tidyr_0.8.3        tibble_2.1.1       ggplot2_3.1.1.9000
#[9] tidyverse_1.2.1   
#
#loaded via a namespace (and not attached):
#  [1] Rcpp_1.0.1       cellranger_1.1.0 pillar_1.3.1     compiler_3.5.2   tools_3.5.2     
#[6] jsonlite_1.6     lubridate_1.7.4  gtable_0.3.0     nlme_3.1-137     lattice_0.20-38 
#[11] pkgconfig_2.0.2  rlang_0.3.4      cli_1.1.0        rstudioapi_0.9.0 yaml_2.2.0      
#[16] haven_2.0.0      withr_2.1.2      xml2_1.2.0       httr_1.4.0       generics_0.0.2  
#[21] hms_0.4.2        grid_3.5.2       tidyselect_0.2.5 glue_1.3.1       R6_2.4.0        
#[26] readxl_1.2.0     modelr_0.1.2     magrittr_1.5     backports_1.1.3  scales_1.0.0    
#[31] rvest_0.3.2      assertthat_0.2.1 colorspace_1.4-1 stringi_1.4.3    lazyeval_0.2.2  
#[36] munsell_0.5.0    broom_0.5.1      crayon_1.3.4
