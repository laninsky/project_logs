# 1. Loading necessary libraries
library(tidyverse)
library(ggmap)
library(ggrepel)

# 2. Setwd
setwd("chickadee/output/")

# 3. Reading in data (tab delimited), dropping last blank row
temp <- read_tsv("../data/S2.txt")
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
  maxlat <- max(reduced_table$DecimalLatitude)+0.1
} else {
  midlat <- (max(reduced_table$DecimalLatitude)+min(reduced_table$DecimalLatitude))/2
  minlat <- midlat-longbound/2
  maxlat <- midlat+longbound/2
  minlong <- min(reduced_table$DecimalLongitude)-0.1
  maxlong <- max(reduced_table$DecimalLongitude)+0.1
}

# Taking the extent of the plot out to the kansas border if it doesn't
# already overlap with it.
if (minlong > -94.6333333) {
  original_extent <- maxlong - minlong
  minlong <- -94.6333333
  new_extent <- maxlong - minlong
  minlat <- minlat - (new_extent-original_extent)/2
  maxlat <- maxlat + (new_extent-original_extent)/2
  sbbox <- make_bbox(lon=c(minlong,maxlong), lat=c(minlat,maxlat),f=0)
} else {
  sbbox <- make_bbox(lon=c(minlong,maxlong), lat=c(minlat,maxlat),f=0)
}

# 7. Using the sbbox object to retrieve a map covering the sample sites
sq_map <- get_map(location = sbbox, maptype = "terrain-background", source = "stamen", crop=TRUE)

# 8. Creating maps of sample locations by "hybrid status"
modernsamplelocations <- ggmap(sq_map) + geom_vline(xintercept=-94.63333) +
  geom_point(data = modern, mapping = aes(x = DecimalLongitude, y = DecimalLatitude,fill = status), shape=21,color = "black",size=12)+
  scale_fill_manual(values=c("#CE1B26", "#15326C","#9437FF")) +
  coord_fixed(ratio=1) + xlab("Longitude") + ylab("Latitude") + 
  theme_bw(base_size = 20) +
  theme(legend.position="none",panel.border=element_rect(fill = NA)) + 
  theme(axis.title=element_text(size=28,face="bold"))

modernsamplelocations + geom_label_repel(modern,mapping=aes(fontface="bold",x=DecimalLongitude,y=DecimalLatitude,label=Location_code,fill=Included_in_tess3r,color=status),size=8, force=20, max.iter = 40000, segment.color="black") + scale_fill_manual(values=c("#CE1B26", "#15326C","#9437FF","#FFFFFF", "#F2B01F")) +
  scale_color_manual(values=c("#CE1B26", "#15326C","#9437FF")) +
  theme(plot.margin=unit(c(1,1,1,1),"cm"))

ggsave(filename="Fig_S2_modern_aggregated_by_site.pdf",plot = last_plot(),width=12.804,height=10.351,units="in")

historicalsamplelocations <- ggmap(sq_map)  + geom_vline(xintercept=-94.63333) + 
  geom_point(data = historical, mapping = aes(x = DecimalLongitude, y = DecimalLatitude,fill = status), shape=21,color = "black",size=12)+
  scale_fill_manual(values=c("#CE1B26", "#15326C","#9437FF")) +
  coord_fixed(ratio=1) + xlab("Longitude") + ylab("Latitude") + 
  theme_bw(base_size = 20) +
          theme(legend.position="none",panel.border=element_rect(fill = NA)) + 
  theme(axis.title=element_text(size=28,face="bold"))

historicalsamplelocations + geom_label_repel(historical,mapping=aes(fontface="bold",x=DecimalLongitude,y=DecimalLatitude,label=Location_code,fill=Included_in_tess3r,color=status),size=8, force=10, max.iter = 40000, segment.color="black") + scale_fill_manual(values=c("#CE1B26", "#15326C","#9437FF","#FFFFFF", "#F2B01F")) +
scale_color_manual(values=c("#CE1B26", "#15326C","#9437FF")) +
  theme(plot.margin=unit(c(1,1,1,1),"cm"))

ggsave(filename="Fig_S2_historical_aggregated_by_site.pdf",plot = last_plot(),width=12.804,height=10.351,units="in")

