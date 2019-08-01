# Loading required libraries
library(tidyverse)
library(gridExtra)
library(scales)

# Loading in the files
filelist <- list.files(pattern="*popsize.txt")
data <- read_table2(filelist[1])
data <- data %>% mutate(Species=gsub("popsize.txt","",filelist[1]))

for (i in 2:length(filelist)) {
  temp <- read_table2(filelist[i])
  temp <- temp %>% mutate(Species=gsub("popsize.txt","",filelist[i]))
  data <- rbind(data,temp)
}

# Cleaning up the species names to match the rest of the paper
data$Species <- gsub("Adelie","Adélie",data$Species)
data$Species <- gsub("Tawaki", "Fiordland crested",data$Species)
data$Species <- gsub("Macroy","Macaroni and Royal",data$Species)
data$Species <- gsub("Filholi","Eastern rockhopper",data$Species)
data$Species <- gsub("Chrysocome","Western rockhopper",data$Species)
data$Species <- gsub("Moseleyi","Northern rockhopper",data$Species)

# Creating a column so we can facet wrap on whether species are expected to be expanding or not
data <- data %>% mutate(expanding=ifelse((Species %in% c("Fiordland crested","Western rockhopper","Northern rockhopper")),"Temperate","Ice associated"))

#Creating a vector tying species name to colour so we can manipulate the order
#Using the results from data_summary, rather than manually
penguin_colours <- c("#4FA253",
                     "#BE00FF",
                     "#a67101",
                     "#B73331",
                     "#3E42C1",
                     "#f97509",
                     "#8d8d8d",
                     "#f656a6",
                     "#03BAFF",
                     "#e8e8e8")

names(penguin_colours) <- c("Fiordland crested",
                            "Western rockhopper",
                            "Chinstrap",
                            "Macaroni and Royal",
                            "Eastern rockhopper",
                            "Gentoo",
                            "Adélie",
                            "Emperor",
                            "Northern rockhopper",
                            "King")

# Reordering our variables based on data_summary
penguin_colours <- penguin_colours[order(factor(names(penguin_colours), levels=c("Macaroni and Royal","Eastern rockhopper","Adélie","Gentoo","Chinstrap","King","Emperor","Northern rockhopper","Western rockhopper","Fiordland crested")))]
data$Species <- factor(data$Species, levels = c("Macaroni and Royal","Eastern rockhopper","Adélie","Gentoo","Chinstrap","King","Emperor","Northern rockhopper","Western rockhopper","Fiordland crested"))


# Plotting all CIs on one graph with a logged y axis
ggplot(data) + geom_rect(mapping=aes(xmin=18000, 
                                     xmax=25000,
                                     ymin=min(Nt,LCL,median,UCL),
                                     ymax=max(Nt,LCL,median,UCL)), 
                         color="black", fill="grey",size=0.1) +
  geom_ribbon(aes(x=time,ymin=LCL,ymax=UCL,
                  fill=Species),alpha=0.6,color="black",size=0.1) +
  scale_fill_manual(name="Species",values=penguin_colours) +
  theme_bw(base_size = 4) + scale_y_log10(expand=c(0,0)) + ylab("Effective population size") + xlab ("Time (years)") +
  theme(axis.title=element_text(face="bold"), legend.title = element_text(face="bold")) + theme(aspect.ratio = 1) + 
  theme(legend.key.size = unit(0.5,"line")) + scale_x_reverse(expand=c(0,0)) + facet_wrap(~expanding)

# Saving this graph
ggsave("combined_cubSFS_figS6.pdf",height=8.7,width=8.7,units="cm")

# Getting some summary statistics from our data based on median values
data_summary <- data %>% group_by(Species) %>% summarise(`N at time 0`=median[time==0],`N at time 1000`=median[time==1000],`Is population expanding?`=ifelse(`N at time 0`>`N at time 1000`,"Yes","No"),`N at start of expansion`=ifelse(`Is population expanding?`=="Yes",min(median),NA),`Fold expansion`=ifelse(`Is population expanding?`=="Yes",`N at time 0`/`N at start of expansion`,NA),`Time at start of expansion`=ifelse(`Is population expanding?`=="Yes",time[median==`N at start of expansion`],NA)) %>% arrange(desc(`Time at start of expansion`),`N at time 0`)

# Saving this data
write.csv(data_summary,"CubSFS_pop_expansion_median_summary.csv",quote=FALSE,row.names = FALSE)

# Getting some summary statistics from our data based on observed values
data_summary_observed <- data %>% group_by(Species) %>% summarise(`N at time 0`=Nt[time==0],`N at time 1000`=Nt[time==1000],`Is population expanding?`=ifelse(`N at time 0`>`N at time 1000`,"Yes","No"),`N at start of expansion`=ifelse(`Is population expanding?`=="Yes",min(Nt),NA),`Fold expansion`=ifelse(`Is population expanding?`=="Yes",`N at time 0`/`N at start of expansion`,NA),`Time at start of expansion`=ifelse(`Is population expanding?`=="Yes",time[Nt==`N at start of expansion`],NA)) %>% arrange(desc(`Time at start of expansion`),`N at time 0`)

# Saving this data
write.csv(data_summary_observed,"CubSFS_pop_expansion_observed_summary.csv",quote=FALSE,row.names = FALSE)

# Determining what the cut-off on the y-axis should be based on the maximum minimum in the non expanding species
ymximum <- as.numeric(data %>% filter(Species %in% (data_summary$Species[data_summary$`Is population expanding?`=="No"])) %>% 
  group_by(Species) %>% summarise(minimum_count=max(LCL)) %>% arrange(desc(minimum_count)) %>% select(minimum_count) %>% slice(1L)) 
 
# Creating a modified matrix with all values over this trimmed to this
data2 <- data %>% mutate(UCL=ifelse(UCL>ymximum,ymximum,UCL)) %>% 
  mutate(LCL=ifelse(LCL>ymximum,ymximum,LCL)) %>% 
  mutate(Nt=ifelse(Nt>ymximum,ymximum,Nt)) %>% 
  mutate(median=ifelse(median>ymximum,ymximum,median))

# Reordering our variables based on data_summary
penguin_colours <- penguin_colours[order(factor(names(penguin_colours), levels=as.character(data_summary$Species)))]
data2$Species <- factor(data2$Species, levels = as.character(data_summary$Species))

# Plotting all species with the same y-axis (logged)
ggplot(data2)  + geom_rect(mapping=aes(xmin=18000, 
                                       xmax=25000,
                                       ymin=min(Nt,LCL,median,UCL),
                                       ymax=max(Nt,LCL,median,UCL)), 
                           color="black", fill="grey",size=0.1) + 
  geom_ribbon(aes(x=time,ymin=LCL,ymax=UCL,
                  fill=factor(Species,levels=names(penguin_colours)
                  )),color="black",show.legend = FALSE) +
  scale_fill_manual(name="Species",values=penguin_colours) +
  theme_bw(base_size = 6) + ylab("Effective population size") + xlab ("Time") +
  facet_wrap(~Species,ncol=2) + ylim(c(NA,ymximum)) + scale_y_log10(expand=c(0,0)) +
  geom_line(aes(x=time,y=Nt),size=0.5) +
  geom_line(aes(x=time,y=median),size=0.5,linetype="dashed") + xlab ("Time (years)") +
  theme(axis.title=element_text(face="bold")) + scale_x_reverse(expand=c(0,0))

ggsave("facetwrap_same_y_cubSFS.pdf",width=8.7,height=17.8,units="cm")

# Plotting all species with different (logged) axes
groblist <- list()
data <- data %>% arrange(factor(Species, levels = data_summary$Species))

for (i in 1:length(unique(data$Species))) {
  temp <- data %>% filter(Species==(unique(data$Species))[i])
  groblist[[i]] <- ggplot(temp)  + geom_rect(mapping=aes(xmin=18000, 
                                                         xmax=25000,
                                                         ymin=min(Nt,LCL,median,UCL),
                                                         ymax=max(Nt,LCL,median,UCL)), 
                                             color="black", fill="grey",size=0.1) + 
    geom_ribbon(aes(x=time,ymin=LCL,ymax=UCL),
                    fill=penguin_colours[i],
                    color="black",show.legend = FALSE) +
    ylab("Effective population size") + xlab ("Time (years)") +
    geom_line(aes(x=time,y=Nt),size=0.5) +
    geom_line(aes(x=time,y=median),size=0.5,linetype="dashed") +
    theme(axis.title=element_text(face="bold")) + theme_bw(base_size = 6) +
    facet_grid(~ Species) + scale_x_reverse(expand=c(0,0)) + scale_y_continuous(expand=c(0,0))
}

plot_to_write <- arrangeGrob(grobs=groblist, ncol=2)
ggsave("facetwrap_diff_non_log_y_cubSFS.pdf",plot_to_write,width=8.7,height=17.8,units="cm")

# Same thing as above but logging the y, except for Northern rockhopper, because there was
# not enough variaiton through time for it to be displayed on a log scale
rm(groblist)
groblist <- list()

for (i in 1:length(unique(data$Species))) {
  temp <- data %>% filter(Species==(unique(data$Species))[i])
  groblist[[i]] <- ggplot(temp)  + geom_rect(mapping=aes(xmin=18000, 
                                                         xmax=25000,
                                                         ymin=min(Nt,LCL,median,UCL),
                                                         ymax=max(Nt,LCL,median,UCL)), 
                                             color="black", fill="grey",size=0.1) + 
    geom_ribbon(aes(x=time,ymin=LCL,ymax=UCL),
                fill=penguin_colours[i],
                color="black",show.legend = FALSE) +
    ylab("Effective population size") + xlab ("Time (years)") +
    geom_line(aes(x=time,y=Nt),size=0.5) +
    geom_line(aes(x=time,y=median),size=0.5,linetype="dashed") +
    theme(axis.title=element_text(face="bold")) + theme_bw(base_size = 6) +
    facet_grid(~ Species) + scale_y_log10(expand=c(0,0)) + scale_x_reverse(expand=c(0,0))
  }

temp <- data %>% filter(Species==(unique(data$Species))[7])
groblist[[7]] <- ggplot(temp)  + geom_rect(mapping=aes(xmin=18000, 
                                                       xmax=25000,
                                                       ymin=min(Nt,LCL,median,UCL),
                                                       ymax=max(Nt,LCL,median,UCL)), 
                                           color="black", fill="grey",size=0.1) + 
    geom_ribbon(aes(x=time,ymin=LCL,ymax=UCL),
                fill=penguin_colours[7],
                color="black",show.legend = FALSE) +
    ylab("Effective population size") + xlab ("Time (years)") +
    geom_line(aes(x=time,y=Nt),size=0.5) +
    geom_line(aes(x=time,y=median),size=0.5,linetype="dashed") +
    theme(axis.title=element_text(face="bold")) + theme_bw(base_size = 6) +
    facet_grid(~ Species) + scale_y_continuous(labels=scientific,expand=c(0,0))+ 
    theme(axis.text.y = element_text(size=4))  + scale_x_reverse(expand=c(0,0))

plot_to_write <- arrangeGrob(grobs=groblist, ncol=2)
ggsave("facetwrap_diff_log_y_cubSFS.pdf",plot_to_write,width=8.7,height=17.8,units="cm")

# Original name, scientific name, and colour
# Tawaki "Fiordland crested" (Eudyptes pachyrhynchus, green) #4FA253
# Adelie "Adélie" #(Pygoscelis adeliae, grey) #8d8d8d
# macroy "Macaroni and Royal" #(Eudyptes chrysolophus chrysolophus/E. c. schlegeli, red/light orange) #EBBDBD
# Filholi "Eastern rockhopper" #(E. filholi, dark blue) #3E42C1
# Chrysocome "Western rockhopper" #(E. chrysocome, purple) #BE00FF
# emperor (Aptenodytes forsteri, pink) #f656a6
# gentoo (P. papua, orange) #f97509
# king (Aptenodytes patagonicus, white) #e8e8e8
# chinstrap (Pygoscelis antarctica, brown) #a67101
# Not present in SFSs
# Snares Crested: "#E5E523"
# Pure Macaroni: "#B73331"
# Pure Royal: #FFB400
# Northern rockhopper: #03BAFF
# Hybrid Macroy: "#EBBDBD"

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
#  [1] scales_1.0.0       gridExtra_2.3      forcats_0.3.0      stringr_1.4.0     
#[5] dplyr_0.8.0.1      purrr_0.3.2        readr_1.3.1        tidyr_0.8.3       
#[9] tibble_2.1.1       ggplot2_3.1.1.9000 tidyverse_1.2.1   
#
#loaded via a namespace (and not attached):
#  [1] Rcpp_1.0.1       plyr_1.8.4       pillar_1.3.1     compiler_3.5.2   cellranger_1.1.0
#[6] tools_3.5.2      digest_0.6.18    jsonlite_1.6     lubridate_1.7.4  nlme_3.1-137    
#[11] gtable_0.3.0     lattice_0.20-38  pkgconfig_2.0.2  rlang_0.3.4      cli_1.1.0       
#[16] rstudioapi_0.9.0 yaml_2.2.0       haven_2.0.0      withr_2.1.2      xml2_1.2.0      
#[21] httr_1.4.0       generics_0.0.2   hms_0.4.2        grid_3.5.2       tidyselect_0.2.5
#[26] glue_1.3.1       R6_2.4.0         readxl_1.2.0     reshape2_1.4.3   modelr_0.1.2    
#[31] magrittr_1.5     backports_1.1.3  rvest_0.3.2      assertthat_0.2.1 colorspace_1.4-1
#[36] labeling_0.3     stringi_1.4.3    lazyeval_0.2.2   munsell_0.5.0    broom_0.5.1     
#[41] crayon_1.3.4 
#
