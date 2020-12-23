# This code corresponds to Fig S14 in Alexander et al.
# It creates Fig. S9 (average number of loci recovered across 
# the 13,366 pairwise comparisons) in the supplementary materials of 
# Alexander et al.

# 1. Loading required library
library(tidyverse)

# 2. Setwd
setwd("chickadee/output/")

# 3A. Reading in structure data (tab delimited) and standardizing names
temp <- read.table("../data/chickadee_singleton_filtered.stru")
temp <- t(temp)
temp[1,seq(1,length(temp[1,]),2)] <- gsub("_.*","_A",temp[1,seq(1,length(temp[1,]),2)])
temp[1,seq(2,length(temp[1,]),2)] <- gsub("_.*","_B",temp[1,seq(2,length(temp[1,]),2)])

# Some minor tweaking of reference sample names based on tissue number used in structure file vs catalog number in Table S1
temp[1,which(grepl("3474",temp[1,]))] <- c("90612_A","90612_B")
temp[1,which(grepl("6281",temp[1,]))] <- c("95776_A","95776_B")
temp[1,which(grepl("9898",temp[1,]))] <- c("131638_A","131638_B")
temp[1,which(grepl("7420",temp[1,]))] <- c("92269_A","92269_B")
temp[1,which(grepl("7421",temp[1,]))] <- c("92270_A","92270_B")

# 3B. Reading in Table S1 data (tab delimited), dropping last blank row
tempS1 <- read_table2("../data/Table_S1.txt")
tempS1 <- tempS1[1:165,]
# Also will drop the sample excluded from Structure analyses due to low depth
tempS1 <- tempS1[-(which(tempS1$Catalog_number=="99788")),]

# 3C. Reading in ipyrad summary, including read depth data (tab delimited)
# This is equivalent to Table S5
tempread <- read_tsv("../data/Table_S5.txt")
tempread[,3] <- as_tibble(gsub("_.*","",as.matrix(tempread[,3])))
# Also will drop the sample excluded from Structure analyses due to low depth
tempread <- tempread[-(which(tempread$Sample=="649257")),]

# Some minor tweaking of reference sample names based on tissue number used in readfile vs catalog number in Table_S1
tempread[which(grepl("3474",as.matrix(tempread[,3]))),3] <- "90612"
tempread[which(grepl("6281",as.matrix(tempread[,3]))),3] <- "95776"
tempread[which(grepl("9898",as.matrix(tempread[,3]))),3] <- "131638"
tempread[which(grepl("7420",as.matrix(tempread[,3]))),3] <- "92269"
tempread[which(grepl("7421",as.matrix(tempread[,3]))),3] <- "92270"

# 4. Creating a new data object to store our comparisons and populating it with sample codes
norows <- round(factorial(dim(tempS1)[1])/(factorial(dim(tempS1)[1]-2)*factorial(2)))

pairwise_comparisons <- matrix(NA,ncol=6,nrow=norows)
x <- 1
for (i in 1:(dim(tempS1)[1]-1)) {
  for (j in (i+1):((dim(tempS1)[1]))) {
    pairwise_comparisons[x,1] <- as.character(tempS1[i,1])
    pairwise_comparisons[x,2] <- as.character(tempS1[j,1])
    x <- x + 1
  }
}

# 5. Populating our data with things of interest
for (i in 1:dim(pairwise_comparisons)[1]) {
  # Only need to look at one "allele" per sample in structure file, finding those columns
  structure_sample_1 <- which(grepl(pairwise_comparisons[i,1], temp[1,]))[1]
  # The smithsonian samples have the tissue number instead of the catalog - looking in this column if need be
  if (is.na(structure_sample_1)) {
    structure_sample_1 <- which(grepl(as.matrix(tempS1[which(as.matrix(tempS1[,1])==pairwise_comparisons[i,1]),2]), temp[1,]))[1]
  }
  structure_sample_2 <- which(grepl(pairwise_comparisons[i,2], temp[1,]))[1]
  # The smithsonian samples have the tissue number instead of the catalog - looking in this column if need be
  if (is.na(structure_sample_2)) {  
    structure_sample_2 <- which(grepl(as.matrix(tempS1[which(as.matrix(tempS1[,1])==pairwise_comparisons[i,2]),2]), temp[1,]))[1]
  }
  
  # Finding the rows in Table S1 so we can extract cluster assignments
  S1_sample_1 <- which(grepl(pairwise_comparisons[i,1], as.matrix(tempS1[,1])))
  S1_sample_2 <- which(grepl(pairwise_comparisons[i,2], as.matrix(tempS1[,1])))
  
  # Finding the rows in tempread so we can pull our read depth
  read_sample_1 <- which(grepl(pairwise_comparisons[i,1], as.matrix(tempread[,3])))
  # The smithsonian samples have the tissue number instead of the catalog - looking in this column if need be
  if(length(read_sample_1)==0) {
    as.matrix(tempS1[which(as.matrix(tempS1[,1])==pairwise_comparisons[i,1]),2])
    read_sample_1 <- which(grepl(as.matrix(tempS1[which(as.matrix(tempS1[,1])==pairwise_comparisons[i,1]),2]), as.matrix(tempread[,3])))
  }
  read_sample_2 <- which(grepl(pairwise_comparisons[i,2], as.matrix(tempread[,3])))
  # The smithsonian samples have the tissue number instead of the catalog - looking in this column if need be
  if(length(read_sample_2)==0) {
    read_sample_2 <- which(grepl(as.matrix(tempS1[which(as.matrix(tempS1[,1])==pairwise_comparisons[i,2]),2]), as.matrix(tempread[,3])))
  }  
  
  # Number of loci that overlap between the samples
  pairwise_comparisons[i,3] <- length(which(temp[,structure_sample_1]!="-9" & temp[,structure_sample_2]!="-9"))-1
  
  # Mean cluster assignment across the pair of samples
  pairwise_comparisons[i,4] <- (as.matrix(tempS1[S1_sample_1,3])+as.matrix(tempS1[S1_sample_2,3]))/2
  
  # Absolute difference in cluster assignment between pairs
  pairwise_comparisons[i,5] <- abs(as.matrix(tempS1[S1_sample_1,3])-as.matrix(tempS1[S1_sample_2,3]))
  
  # Mean read depth across the pair of samples
  pairwise_comparisons[i,6] <- (as.numeric(as.matrix(tempread[read_sample_1,5]))+as.numeric(as.matrix(tempread[read_sample_2,5])))/2

}

# 6. Transforming pairwise_comparisons into a tibble so we can aggregate it and get plotting
pairwise_comparisons <- as_tibble(pairwise_comparisons)
names(pairwise_comparisons) <- c("sample_1","sample_2","overlapping_loci","average_cluster","abs_cluster_diff","average_read_cov")
pairwise_comparisons[,3:6] <- pairwise_comparisons[,3:6] %>% mutate_if(is.character, as.numeric)

breakpoints <- seq(0,1,0.025)
breakpoints[1] <- -0.00001
breakpoints[length(breakpoints)] <- 1.00001

# Creating a column aggregating on average cluster
pairwise_comparisons <- pairwise_comparisons %>% mutate(grouped_av_cluster=cut(average_cluster,breaks = breakpoints)) %>% mutate(grouped_av_cluster=(as.numeric(gsub("-1e-05","0",gsub(",.*","",gsub("\\(","",as.character(grouped_av_cluster)))))))

# Creating a column aggregating on abs_cluster_diff
pairwise_comparisons <- pairwise_comparisons %>% mutate(grouped_cluster_diff=cut(abs_cluster_diff,breaks = breakpoints)) %>% mutate(grouped_cluster_diff=(as.numeric(gsub("-1e-05","0",gsub(",.*","",gsub("\\(","",as.character(grouped_cluster_diff)))))))

# Creating a column aggregating on read depth - first getting appropriate break points
readbreakpointinterval <- (max(pairwise_comparisons$average_read_cov/1000000)-min(pairwise_comparisons$average_read_cov/1000000))/(length(breakpoints)-1)

readbreakpoints <- seq(min(pairwise_comparisons$average_read_cov/1000000),max(pairwise_comparisons$average_read_cov/1000000),readbreakpointinterval)

readbreakpoints[1] <- readbreakpoints[1]-0.00001
readbreakpoints[length(breakpoints)] <- readbreakpoints[length(breakpoints)] +0.00001

pairwise_comparisons <- pairwise_comparisons %>% mutate(grouped_read_depth=cut(average_read_cov/1000000,breaks = readbreakpoints)) %>% mutate(grouped_read_depth=(as.numeric(gsub("-1e-05","0",gsub(",.*","",gsub("\\(","",as.character(grouped_read_depth)))))))

# 7. Now grouping for the first plot
firstplot <- pairwise_comparisons %>% group_by(grouped_av_cluster,grouped_cluster_diff) %>% summarise(mean_loci=mean(overlapping_loci),num_pairs=n(),av_cluster=mean(average_cluster),av_diff=mean(abs_cluster_diff))

# export as 1000 pixels width, Fig_S9_firstplot_main.png in output folder
firstplot_to_save <- ggplot(firstplot,aes(x=grouped_cluster_diff,y=grouped_av_cluster,fill=mean_loci)) + 
  geom_tile(color="black") + 
  scale_fill_gradientn(colors=c("royalblue3","lightseagreen","chartreuse3","greenyellow","yellow"), guide = guide_colorbar(frame.colour = "black")) + 
  theme(panel.background  = element_rect(color="black")) + 
  theme_bw(base_size = 16) + 
  scale_x_continuous(limits = c(NA,1),expand = c(0, 0), name = str_wrap("Average absolute difference in cluster assignment between samples in pair",width=40)) + 
  scale_y_continuous(limits = c(0,1),expand = c(0, 0),name = str_wrap("Average assignment to BC cluster across samples in pair",width=40))  + 
  theme(axis.title=element_text(size=18,face="bold")) + 
  theme(legend.title = element_blank())   +
  theme(plot.margin=unit(c(1,1,1,1),"cm"))  + 
  theme(aspect.ratio = 1)

# export as 840 pixels width, 210 high, Fig_S9_numpairs_cluster_diff.png in output folder
numpairsdiff <- ggplot(firstplot,aes(x=grouped_cluster_diff,y=num_pairs)) + 
  geom_bar(stat='identity', width=0.025,fill="black",color="black") + 
  theme_bw(base_size = 16) + 
  scale_x_continuous(limits=c(NA,1),expand = c(0, 0),name = str_wrap("Average absolute difference in cluster assignment between samples in pair",width=40)) + 
  scale_y_continuous(expand = c(0, 0),name=str_wrap("Number of pairs", width=10))  + 
  theme(axis.title=element_text(size=18,face="bold")) +
  theme(plot.margin=unit(c(1,1,1,1),"cm"))

# export as 225 pixels width, 840 high, Fig_S9_numpairs_cluster_av.png in output folder
numpairsav <- ggplot(firstplot,aes(x=grouped_av_cluster,y=num_pairs)) + 
  geom_bar(stat='identity', width=0.025,fill="black",color="black") + 
  theme_bw(base_size = 16) + 
  scale_x_continuous(limits=c(NA,1),expand = c(0, 0), name = str_wrap("Average assignment to BC cluster across samples in pair",width=40)) + 
  scale_y_reverse(expand = c(0, 0),name=str_wrap("Number of pairs", width=10),breaks=c(0,1000,2000,3000)) +
  coord_flip()  + 
  theme(axis.title=element_text(size=18,face="bold")) + 
  theme(axis.text.x = element_text(angle = 270)) +
  theme(plot.margin=unit(c(1,1,1,1),"cm"))

# 8. Now grouping for the second plot
secondplot <- pairwise_comparisons %>% group_by(grouped_read_depth,grouped_cluster_diff) %>% summarise(mean_loci=mean(overlapping_loci),num_pairs=n(),av_readdepth=mean(average_read_cov),av_diff=mean(abs_cluster_diff))

# export as 1000 pixels wide, Fig_S9_secondplot_main.png in output folder
secondplot_to_save <- ggplot(secondplot,aes(x=grouped_cluster_diff,y=grouped_read_depth,fill=mean_loci)) + 
  geom_tile(color="black") +
  scale_fill_gradientn(colors=c("royalblue3","lightseagreen","chartreuse3","greenyellow","yellow"), guide = guide_colorbar(frame.colour = "black")) + 
  theme(panel.background  = element_rect(color="black")) + 
  theme_bw(base_size = 16) + 
  scale_x_continuous(limits = c(0,1),expand = c(0, 0), name = str_wrap("Average absolute difference in cluster assignment between samples in pair",width=40)) + 
  scale_y_continuous(expand = c(0, 0), name = str_wrap("Average read depth across members of pair (× 1,000,000)",width=40))  + 
  theme(axis.title=element_text(size=18,face="bold"))   + 
  theme(legend.title = element_blank()) +
  theme(plot.margin=unit(c(1,1,1,1),"cm"))  + 
  theme(aspect.ratio = 1)

# export as 210 pixels width, 840 high, Fig_S9_numpairs_read.png in output folder
numpairsreads <- ggplot(secondplot,aes(x=grouped_read_depth,y=num_pairs)) + 
  geom_bar(stat='identity', width=0.07,fill="black",color="black") + 
  theme_bw(base_size = 16) + 
  scale_x_continuous(expand = c(0, 0), name=str_wrap("Average read depth across members of pair (× 1,000,000)", width=40)) + 
  scale_y_reverse(expand = c(0, 0), name=str_wrap("Number of pairs", width=10)) + 
  coord_flip() + 
  theme(axis.title=element_text(size=18,face="bold")) +
  theme(axis.text.x = element_text(angle = 270)) +
  theme(plot.margin=unit(c(1,1,1,1),"cm"))

# assembled the plots manually in ppt/illustrator

sessionInfo()
#R version 3.6.3 (2020-02-29)
#Platform: x86_64-apple-darwin15.6.0 (64-bit)
#Running under: macOS Sierra 10.12.6
#
#Matrix products: default
#BLAS:   /System/Library/Frameworks/Accelerate.framework/Versions/A/Frameworks/vecLib.framework/Versions/A/libBLAS.dylib
#LAPACK: /Library/Frameworks/R.framework/Versions/3.6/Resources/lib/libRlapack.dylib
#
#locale:
#  [1] en_US.UTF-8/en_US.UTF-8/en_US.UTF-8/C/en_US.UTF-8/en_US.UTF-8
#
#attached base packages:
#  [1] stats     graphics  grDevices utils     datasets  methods   base     
#
#other attached packages:
#  [1] forcats_0.4.0   stringr_1.4.0   dplyr_0.8.4     purrr_0.3.3     readr_1.3.1    
#[6] tidyr_1.0.2     tibble_2.1.3    ggplot2_3.2.1   tidyverse_1.3.0
#
#loaded via a namespace (and not attached):
#  [1] Rcpp_1.0.4.6     cellranger_1.1.0 pillar_1.4.3     compiler_3.6.3   dbplyr_1.4.2 #   
#[6] tools_3.6.3      digest_0.6.24    lubridate_1.7.4  jsonlite_1.6.1   lifecycle_0.1.0 
#[11] nlme_3.1-144     gtable_0.3.0     lattice_0.20-38  pkgconfig_2.0.3  rlang_0.4.4     
#[16] reprex_0.3.0     cli_2.0.1        DBI_1.1.0        rstudioapi_0.11  haven_2.2.0     
#[21] withr_2.1.2      xml2_1.2.2       httr_1.4.1       fs_1.3.1         generics_0.0.2  
#[26] vctrs_0.2.2      hms_0.5.3        grid_3.6.3       tidyselect_1.0.0 glue_1.3.1      
#[31] R6_2.4.1         fansi_0.4.1      readxl_1.3.1     farver_2.0.3     modelr_0.1.5    
#[36] magrittr_1.5     backports_1.1.5  scales_1.1.0     rvest_0.3.5      assertthat_0.2.1
#[41] colorspace_1.4-1 labeling_0.3     utf8_1.1.4       stringi_1.4.5    lazyeval_0.2.2  
#[46] munsell_0.5.0    broom_0.5.4      crayon_1.3.4