# 1. Load required libraries
library(tidyverse)
library(plotrix)

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

# Creating a vector of colours associated with each species
penguin_colours <- c("#4FA253",
                     "#BE00FF",
                     "#a67101",
                     "#EBBDBD",
                     "#3E42C1",
                     "#f97509",
                     "#8d8d8d",
                     "#f656a6",
                     "#03BAFF",
                     "#e8e8e8",
                     "#E5E523",
                     "#B73331",
                     "#FFB400")

names(penguin_colours) <- c("Fiordland crested",
                            "Western rockhopper",
                            "Chinstrap",
                            "Macaroni Royal hybrid",
                            "Eastern rockhopper",
                            "Gentoo",
                            "Adélie",
                            "Emperor",
                            "Northern rockhopper",
                            "King",
                            "Snares crested",
                            "Macaroni",
                            "Royal"
                            
)

penguin_colours <- penguin_colours[order(factor(names(penguin_colours), levels=as.character(unique(data$Taxa_common_name))))]

# 4. Plot data by location
ggplot(data,aes(x=factor(Location_code,levels=unique(as.character(data$Location_code))),y=Delta_theta)) + 
  geom_jitter(aes(fill=factor(Taxa_common_name,levels=names(penguin_colours)),shape=as.character(Replicate)),size=2,color="black",height=0,width=0.2) + 
  theme_bw(base_size = 8) + 
  scale_shape_manual(name="Replicate",values=c(21,22,23)) + 
  scale_fill_manual(name="Species",values=penguin_colours) +
  guides(fill=guide_legend(override.aes=list(shape=21))) +
  xlab("Location") +
  ylab("Delta theta") + 
  theme(axis.text.x = element_text(angle = 90)) +
  theme(axis.title.x=element_text(face="bold")) +
  theme(axis.title.y=element_text(face="bold")) + 
  theme(legend.title = element_text(face="bold")) +
  theme(legend.key.size = unit(0.3, "cm"))

ggsave("SNAPP_by_sampling_location.pdf",height=8.7,width=17.8,units="cm")

# 5. Plot data by species, considering each replicate*location as a separate data point
ggplot(data,aes(x=factor(Taxa_common_name,levels=names(penguin_colours)),y=Delta_theta)) + 
  geom_boxplot(aes(fill=factor(Taxa_common_name,levels=names(penguin_colours))),show.legend=FALSE,size=0.3) + 
  scale_fill_manual(name="Species",values=penguin_colours) +
  xlab("Species") +
  ylab("Delta theta") + 
  theme(axis.text.x = element_text(angle = 90)) +
  theme(axis.title.x=element_text(face="bold")) +
  theme(axis.title.y=element_text(face="bold")) + 
  theme_bw(base_size = 8) +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 10))

ggsave("SNAPP_by_species.pdf",height=8.7,width=17.8,units="cm")

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
#  [1] plotrix_3.7-5      bindrcpp_0.2.2     forcats_0.3.0      stringr_1.3.1      dplyr_0.7.8        purrr_0.3.0        readr_1.3.1       
#[8] tidyr_0.8.2        tibble_2.0.1       ggplot2_3.1.0.9000 tidyverse_1.2.1   
#
#loaded via a namespace (and not attached):
#  [1] Rcpp_1.0.1       cellranger_1.1.0 pillar_1.3.1     compiler_3.5.1   bindr_0.1.1      tools_3.5.1      digest_0.6.18    jsonlite_1.6    
#[9] lubridate_1.7.4  nlme_3.1-137     gtable_0.2.0     lattice_0.20-38  pkgconfig_2.0.2  rlang_0.3.1      cli_1.0.1        rstudioapi_0.9.0
#[17] yaml_2.2.0       haven_2.0.0      withr_2.1.2      xml2_1.2.0       httr_1.4.0       generics_0.0.2   hms_0.4.2        grid_3.5.1      
#[25] tidyselect_0.2.5 glue_1.3.0       R6_2.3.0         fansi_0.4.0      readxl_1.2.0     modelr_0.1.2     magrittr_1.5     backports_1.1.3 
#[33] scales_1.0.0     rvest_0.3.2      assertthat_0.2.0 colorspace_1.4-0 labeling_0.3     utf8_1.1.4       stringi_1.2.4    lazyeval_0.2.1  
#[41] munsell_0.5.0    broom_0.5.1      crayon_1.3.4  