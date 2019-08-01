# 1. Load required libraries
library(tidyverse)
library(gridExtra)
library(scales)

#2. Read in and check data
data <- read_tsv("tab_delimited_snapp.txt")
data
## A tibble: 139 x 11
#Replicate Delta_theta Location Location_code Taxa_common_name Taxa_scientific… Taxa_order_for_… Location_order_… Overall_order Node_height
#<dbl>       <dbl> <chr>    <chr>         <chr>            <chr>                       <dbl>            <dbl>         <dbl>       <dbl>
#  1         1    -0.00343 SNA      SNA           Snares crested   Eudyptes robust…                1                1             1     0.0137 
#2         1    -0.120   WES      WES           Snares crested   Eudyptes robust…                1                2             2     0.0137 
#3         2    -0.0121  SNA      SNA           Snares crested   Eudyptes robust…                1                1             3     0.0128 
#4         2    -0.155   WES      WES           Snares crested   Eudyptes robust…                1                2             4     0.0128 
#5         3    -0.399   SNA      SNA           Snares crested   Eudyptes robust…                1                1             5     0.0002 
#6         3    -0.401   WES      WES           Snares crested   Eudyptes robust…                1                2             6     0.0002 
#7         1     0.0253  COD      COD           Fiordland crest… Eudyptes pachyr…                2                1             7     0.00427
#8         1    -0.353   JAC      JAC           Fiordland crest… Eudyptes pachyr…                2                2             8     0.00427
#9         1    -0.0344  MIL      MIL           Fiordland crest… Eudyptes pachyr…                2                3             9     0.00514
#10         2    -0.491   COD      COD           Fiordland crest… Eudyptes pachyr…                2                1            10     0.00311
## … with 129 more rows, and 1 more variable: Gen_time <dbl>

# Because we coded the Macaroni, Royal, and hybrids separately in data, need to combine
# them for this analysis so it matches up with the rest of the paper
data <- data %>% mutate(Taxa_common_name=ifelse(Taxa_common_name=="Macaroni","Macaroni and Royal",
                                        ifelse(Taxa_common_name=="Royal","Macaroni and Royal",
                                               ifelse(Taxa_common_name=="Macaroni Royal hybrid","Macaroni and Royal",Taxa_common_name))))

# Creating a vector of colours associated with each species
penguin_colours <- c("#4FA253",
                     "#BE00FF",
                     "#a67101",
                     "#3E42C1",
                     "#f97509",
                     "#8d8d8d",
                     "#f656a6",
                     "#03BAFF",
                     "#e8e8e8",
                     "#E5E523",
                     "#B73331")

names(penguin_colours) <- c("Fiordland crested",
                            "Western rockhopper",
                            "Chinstrap",
                            "Eastern rockhopper",
                            "Gentoo",
                            "Adélie",
                            "Emperor",
                            "Northern rockhopper",
                            "King",
                            "Snares crested",
                            "Macaroni and Royal")

# Reading in the order from the cubSFS analysis so we can order these plots the same way
data_summary <- read_csv("CubSFS_pop_expansion_median_summary.csv")


# Getting the order that Tess wants for the manuscript 
data <- data %>% arrange(factor(Taxa_common_name, levels = c("Macaroni and Royal","Eastern rockhopper","Adélie","Gentoo","Chinstrap","King","Emperor","Northern rockhopper","Western rockhopper","Fiordland crested")))
penguin_colours <- penguin_colours[order(factor(names(penguin_colours), levels=c("Macaroni and Royal","Eastern rockhopper","Adélie","Gentoo","Chinstrap","King","Emperor","Northern rockhopper","Western rockhopper","Fiordland crested")))]

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

ggsave("SNAPP_by_species_FigS9.pdf",height=8.7,width=17.8,units="cm")

#6. Plot delta theta values by species and by age of node they represent the delta theta over
# (young nodes may not be showing the overall trends that have occured since the LGM)
data <- data %>% mutate(Age_years=Node_height/(2.6*10^(-7))*Gen_time)

ggplot(data) + geom_rect(mapping=aes(xmin=18000, 
                                     xmax=25000,
                                     ymin=min(Delta_theta)-0.01,
                                     ymax=max(Delta_theta)+0.01), 
                         color="black", fill="grey",size=0.1) + 
  geom_point(aes(x=Age_years,y=Delta_theta,
                 fill=factor(Taxa_common_name,levels=names(penguin_colours)),shape=as.character(Replicate)),size=2,color="black") + 
  theme_bw(base_size = 8) + 
  scale_shape_manual(name="Replicate",values=c(21,22,23)) + 
  scale_fill_manual(name="Species",values=penguin_colours) +
  guides(fill=guide_legend(override.aes=list(shape=21))) +
  xlab("Age (years)") +
  ylab("Delta theta") + 
  theme(axis.title.x=element_text(face="bold")) +
  theme(axis.title.y=element_text(face="bold")) + 
  theme(legend.title = element_text(face="bold")) +
  theme(legend.key.size = unit(0.3, "cm")) + scale_x_reverse(labels=comma_format()) +
  scale_y_continuous(expand=c(0,0))

ggsave("SNAPP_delta_theta_by_node_age.pdf",height=8.7,width=17.8,units="cm")

#7. Similar plot as above, but facet wrapped (same x/y for all species)
ggplot(data) + geom_rect(mapping=aes(xmin=18000, 
                                     xmax=25000,
                                     ymin=min(Delta_theta)-0.01,
                                     ymax=max(Delta_theta)+0.01), 
                         color="black", fill="grey",size=0.1) + 
  geom_point(aes(x=Age_years,y=Delta_theta,
                 fill=factor(Taxa_common_name,levels=names(penguin_colours)),shape=as.character(Replicate)),size=1.5,color="black",show.legend = FALSE) + 
  theme_bw(base_size = 8) + 
  scale_shape_manual(name="Replicate",values=c(21,22,23)) + 
  scale_fill_manual(name="Species",values=penguin_colours) +
  guides(fill=guide_legend(override.aes=list(shape=21))) +
  xlab("Age (years)") +
  ylab("Delta theta") + 
  theme(axis.title.x=element_text(face="bold")) +
  theme(axis.title.y=element_text(face="bold")) + 
  theme(legend.title = element_text(face="bold")) +
  theme(legend.key.size = unit(0.3, "cm")) + scale_x_reverse(labels=comma_format()) +
  scale_y_continuous(expand=c(0,0)) + facet_wrap(~ factor(Taxa_common_name,levels=names(penguin_colours)),ncol=2)

ggsave("SNAPP_node_age_by_species_same_axes.pdf",width=8.7,height=17.8,units="cm")

#8. Similar plot as above, but giving each of the species its own axes
# First creating a column in thousands of years, because of constraints in plotting
data <- data %>% mutate(Age_in_thous=Age_years/1000)
groblist <- list()

species_order <- c("Macaroni and Royal","Eastern rockhopper","Adélie","Gentoo","Chinstrap","King","Emperor","Northern rockhopper","Western rockhopper","Fiordland crested", "Snares crested")

for (i in 1:length(species_order)) {
  temp <- data %>% filter(Taxa_common_name==species_order[i])
  groblist[[i]] <- ggplot(temp)  + geom_rect(mapping=aes(xmin=18, 
                                                          xmax=25,
                                                          ymin=min(Delta_theta)-(max(Delta_theta)-min(Delta_theta))/10,
                                                          ymax=max(Delta_theta)+(max(Delta_theta)-min(Delta_theta))/10), 
                                              color="black", fill="grey",size=0.1) +
    geom_point(aes(x=Age_in_thous,y=Delta_theta,
                   fill=factor(Taxa_common_name,levels=names(penguin_colours)),shape=as.character(Replicate)),size=1.5,color="black",show.legend = FALSE)+ 
    theme_bw(base_size = 8) + 
    scale_shape_manual(name="Replicate",values=c(21,22,23)) + 
    scale_fill_manual(name="Species",values=penguin_colours) +
    guides(fill=guide_legend(override.aes=list(shape=21))) +
    xlab("Age (in 1,000 years)") +
    ylab("Delta theta") + 
    theme(axis.title.x=element_text(face="bold")) +
    theme(axis.title.y=element_text(face="bold")) + 
    theme(legend.title = element_text(face="bold")) +
    theme(legend.key.size = unit(0.3, "cm")) + scale_x_reverse(labels=comma_format()) +
    scale_y_continuous(expand=c(0,0)) + facet_wrap(~ factor(Taxa_common_name,levels=names(penguin_colours)))
}

plot_to_write <- arrangeGrob(grobs=groblist, ncol=2)

ggsave("SNAPP_node_age_by_species_diff_axes_Figs10.pdf",plot_to_write,width=8.7,height=17.8,units="cm")

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
#  [1] bindrcpp_0.2.2     scales_1.0.0       gridExtra_2.3      forcats_0.3.0      stringr_1.3.1      dplyr_0.7.8        purrr_0.3.0       
#[8] readr_1.3.1        tidyr_0.8.2        tibble_2.0.1       ggplot2_3.1.0.9000 tidyverse_1.2.1   
#
#loaded via a namespace (and not attached):
#  [1] Rcpp_1.0.1       cellranger_1.1.0 pillar_1.3.1     compiler_3.5.1   bindr_0.1.1      tools_3.5.1      digest_0.6.18    jsonlite_1.6    
#[9] lubridate_1.7.4  nlme_3.1-137     gtable_0.2.0     lattice_0.20-38  pkgconfig_2.0.2  rlang_0.3.1      cli_1.0.1        rstudioapi_0.9.0
#[17] yaml_2.2.0       haven_2.0.0      withr_2.1.2      xml2_1.2.0       httr_1.4.0       generics_0.0.2   hms_0.4.2        grid_3.5.1      
#[25] tidyselect_0.2.5 glue_1.3.0       R6_2.3.0         fansi_0.4.0      readxl_1.2.0     modelr_0.1.2     magrittr_1.5     backports_1.1.3 
#[33] rvest_0.3.2      assertthat_0.2.0 colorspace_1.4-0 labeling_0.3     utf8_1.1.4       stringi_1.2.4    lazyeval_0.2.1   munsell_0.5.0   
#[41] broom_0.5.1      crayon_1.3.4   