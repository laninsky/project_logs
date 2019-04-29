# 1. Loading required library
library(tidyverse)

# 2. Setwd
setwd("chickadee/output/")

# 3. Reading in data (tab delimited), dropping last blank row and filtering for samples where song data available
temp <- read_tsv("../data/S2.txt")
temp <- temp[1:165,]
temp <- temp %>% filter(!is.na(Song_summary)) %>% arrange(Song_summary)

# 4. Expanding dataset into "long" format 
longtemp <- gather(temp,cluster_assignment,assignment_value,c(BC_genetic_cluster_assignment,CC_genetic_cluster_assignment)) %>% arrange(assignment_value) %>% arrange(desc(cluster_assignment))

# 5. Creating structure plot based on "long" format and annotating Appleton City samples
# Saving as 1000 pixels wide, preserving aspect ratio as FigS5.png (in output folder)
ggplot(longtemp, aes(fill=cluster_assignment,y=assignment_value,x=as.factor(Catalog_number))) +
  geom_bar(stat="identity",color="black",width=1) + theme (legend.position="none", axis.text = element_blank(),axis.title=element_blank(), axis.ticks = element_blank()) + scale_y_continuous(limits=c(0,1),expand = c(0, 0)) +
  theme(aspect.ratio = 1/3) +
  scale_x_discrete(limits=temp$Catalog_number) +
  scale_fill_manual(values = (c("#CE1B26","#15326C"))) +
  annotate("text",x=as.character(as.matrix(temp %>% filter(str_detect(Specific_locality,"Appleton City")) %>% select(Catalog_number))),y=0.025,label="*",color="orange",size=12)

# 6. Extracting information for doing t-tests etc

