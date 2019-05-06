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
  geom_point(data = historical, mapping = aes(x = DecimalLongitude, y = DecimalLatitude,fill = status), shape=21,color = "black",size=12) +
  scale_x_continuous(expand=c(0,0),position = "top") +
  scale_fill_manual(values=c("#CE1B26", "#15326C","#9437FF")) +
  coord_fixed(ratio=1) + xlab("Longitude") + ylab("Latitude") + 
  theme_bw(base_size = 20) +
          theme(legend.position="none",panel.border=element_rect(fill = NA)) + 
  theme(axis.title=element_text(size=28,face="bold")) 

historicalsamplelocations + geom_label_repel(historical,mapping=aes(fontface="bold",x=DecimalLongitude,y=DecimalLatitude,label=Location_code,fill=Included_in_tess3r,color=status),size=8, force=10, max.iter = 40000, segment.color="black") + scale_fill_manual(values=c("#CE1B26", "#15326C","#9437FF","#FFFFFF", "#F2B01F")) +
scale_color_manual(values=c("#CE1B26", "#15326C","#9437FF")) +
  theme(plot.margin=unit(c(1,1,1,1),"cm"))

ggsave(filename="Fig_S2_historical_aggregated_by_site.pdf",plot = last_plot(),width=12.804,height=10.351,units="in")

# 9. Creating the structure plots ordered by location
# Modern by longitude, export as 1000 pixels wide: FigS2_modern_long.png
modernindividualnames <- temp %>% arrange(desc(BC_genetic_cluster_assignment)) %>% arrange(Location_code) %>% arrange(DecimalLongitude) %>% filter(Sampling_period=="MODERN")

modernindividual <- temp %>% filter(Sampling_period=="MODERN") %>% gather(cluster_assignment,assignment_value,c(BC_genetic_cluster_assignment,CC_genetic_cluster_assignment)) %>% arrange(desc(cluster_assignment)) %>% arrange(Location_code) %>% arrange(DecimalLongitude)

ggplot(modernindividual, aes(fill=cluster_assignment,y=assignment_value,x=as.factor(Catalog_number))) +
  geom_bar(stat="identity",color="black",width=1) + theme (legend.position="none", axis.text = element_blank(),axis.title=element_blank(), axis.ticks = element_blank()) + scale_y_continuous(limits=c(0,1),expand = c(0, 0)) +
  theme(aspect.ratio = 1/4) +
  scale_x_discrete(limits=modernindividualnames$Catalog_number) +
  scale_fill_manual(values = (c("#CE1B26","#15326C")))

# Historical by longitude, export as 1000 pixels wide: FigS2_historical_long.png
historicalindividualnames <- temp %>% arrange(desc(BC_genetic_cluster_assignment)) %>% arrange(Location_code) %>% arrange(DecimalLongitude) %>% filter(Sampling_period=="SMITHSONIAN")

historicalindividual <- temp %>% filter(Sampling_period=="SMITHSONIAN") %>% gather(cluster_assignment,assignment_value,c(BC_genetic_cluster_assignment,CC_genetic_cluster_assignment)) %>% arrange(desc(cluster_assignment)) %>% arrange(Location_code) %>% arrange(DecimalLongitude)

ggplot(historicalindividual, aes(fill=cluster_assignment,y=assignment_value,x=as.factor(Catalog_number))) +
  geom_bar(stat="identity",color="black",width=1) + theme (legend.position="none", axis.text = element_blank(),axis.title=element_blank(), axis.ticks = element_blank()) + scale_y_continuous(limits=c(0,1),expand = c(0, 0)) +
  theme(aspect.ratio = 1/4) +
  scale_x_discrete(limits=historicalindividualnames$Catalog_number) +
  scale_fill_manual(values = (c("#CE1B26","#15326C")))

# Modern by latitude, export as 1000 pixels height: FigS2_modern_lat.png
modernindividualnames <- temp %>% arrange(desc(BC_genetic_cluster_assignment)) %>% arrange(Location_code) %>% arrange(DecimalLatitude) %>% filter(Sampling_period=="MODERN")

modernindividual <- temp %>% filter(Sampling_period=="MODERN") %>% gather(cluster_assignment,assignment_value,c(BC_genetic_cluster_assignment,CC_genetic_cluster_assignment)) %>% arrange(desc(cluster_assignment)) %>% arrange(Location_code) %>% arrange(DecimalLatitude)

ggplot(modernindividual, aes(fill=cluster_assignment,y=assignment_value,x=as.factor(Catalog_number))) +
  geom_bar(stat="identity",color="black",width=1) + theme (legend.position="none", axis.text = element_blank(),axis.title=element_blank(), axis.ticks = element_blank()) + scale_y_continuous(limits=c(0,1),expand = c(0, 0)) +
  theme(aspect.ratio = 4/1) +
  scale_x_discrete(limits=modernindividualnames$Catalog_number) +
  scale_fill_manual(values = (c("#CE1B26","#15326C"))) +
  coord_flip()

# Modern by latitude, export as 1000 pixels height: FigS2_historical_lat.png
historicalindividualnames <- temp %>% arrange(desc(BC_genetic_cluster_assignment)) %>% arrange(Location_code) %>% arrange(DecimalLatitude) %>% filter(Sampling_period=="SMITHSONIAN")

historicalindividual <- temp %>% filter(Sampling_period=="SMITHSONIAN") %>% gather(cluster_assignment,assignment_value,c(BC_genetic_cluster_assignment,CC_genetic_cluster_assignment)) %>% arrange(desc(cluster_assignment)) %>% arrange(Location_code) %>% arrange(DecimalLatitude)

ggplot(historicalindividual, aes(fill=cluster_assignment,y=assignment_value,x=as.factor(Catalog_number))) +
  geom_bar(stat="identity",color="black",width=1) + theme (legend.position="none", axis.text = element_blank(),axis.title=element_blank(), axis.ticks = element_blank()) + scale_y_continuous(limits=c(0,1),expand = c(0, 0)) +
  theme(aspect.ratio = 4/1) +
  scale_x_discrete(limits=historicalindividualnames$Catalog_number) +
  scale_fill_manual(values = (c("#CE1B26","#15326C"))) +
  coord_flip()

# 10. Generating site labels, first for modern by longitude (the x-axis for the plot)
modern <- modern %>% arrange(Location_code) %>% arrange(DecimalLongitude)

# replacing "status" with the appropriate color
fill_codes <- as.matrix(modern$status)[,1]
fill_codes <- gsub("Hybrid","#9437FF",gsub("CC","#15326C",gsub("BC","#CE1B26",fill_codes)))

# creating a vector for the position of our sampling site text labels
labelx <- modern$r[length(modern$r)]/2
for (i in (dim(modern)[1]-1):1) {
  labelx <- c(labelx, (labelx[length(labelx)]+(modern$r[i+1]/2)+(modern$r[i]/2)))
}

# creating a vector for the y-position of the labels to stagger them
# and avoid overlap
labely <- rep(4,length(modern$r))
labely[seq(2,length(modern$r),4)] <- 1
labely[seq(3,length(modern$r),4)] <- 3
labely[seq(4,length(modern$r),4)] <- 0

# building the base colored plot
modernlong <- ggplot(modern,aes(y=r,x=2)) + geom_bar(stat="identity",color="black",aes(fill=factor(modern$Location_code,levels=as.numeric(modern$Location_code)))) +
  theme_classic() +
  theme(panel.border = element_blank(),axis.line=element_blank()) +
  scale_x_continuous(limits=c(-1,5)) +
  scale_fill_manual(values = c(fill_codes)) +
  theme (legend.position="none", axis.text = element_blank(),axis.title=element_blank(), axis.ticks = element_blank())  +
  theme(aspect.ratio = 1/5) +
  coord_flip() 

# Figuring out what the colors should be for fill and outline of the 
# first site label
if (modern$Included_in_tess3r[length(modern$Included_in_tess3r)]=="NO") {
  fillcolor <- "#FFFFFF"
} else {
  fillcolor <- "#F2B01F"
}
if (modern$status[length(modern$Included_in_tess3r)]=="Hybrid") {
  colorcolor <- "#9437FF"
} else {
  if (modern$status[length(modern$Included_in_tess3r)]=="CC") {
    colorcolor <- "#15326C"
  }  else {
    colorcolor <- "#CE1B26"
  }
}

# creating a vector to put in some vlines separating the labels
vlinepos <- c(0,modern$r[length(modern$r)])
for (i in (dim(modern)[1]-1):1) {
  vlinepos <- c(vlinepos, (vlinepos[length(vlinepos)]+(modern$r[i])))
}

# and adding this annotation on to the baseplot
modernlong <- modernlong + geom_hline(yintercept = vlinepos, linetype="dashed")

# and adding text label annotation on to the baseplot for left-most site
modernlong <- modernlong + annotate("label",y=labelx[1],x=labely[1],fontface="bold",label=modern$Location_code[length(modern$Included_in_tess3r)],fill=fillcolor,color=colorcolor,size=12)

# and now doing this for all the remaining labels
for (i in 2:length(labelx)) {
  if (modern$Included_in_tess3r[length(modern$Included_in_tess3r)-i+1]=="NO") {
    fillcolor <- "#FFFFFF"
  } else {
    fillcolor <- "#F2B01F"
  }
  if (modern$status[length(modern$Included_in_tess3r)-i+1]=="Hybrid") {
    colorcolor <- "#9437FF"
  } else {
    if (modern$status[length(modern$Included_in_tess3r)-i+1]=="CC") {
      colorcolor <- "#15326C"
    }  else {
    colorcolor <- "#CE1B26"
    }
  }
  modernlong <- modernlong + annotate("label",y=labelx[i],x=labely[i],fontface="bold",label=modern$Location_code[length(modern$Included_in_tess3r)-i+1],fill=fillcolor,color=colorcolor,size=12)
}  

# Exporting as 1500 pixels width, FigS2_modern_long_sites.png
modernlong + scale_y_reverse() 

# 11. Generating site labels, next historical by longitude (the x-axis for the plot)
historical <- historical %>% arrange(Location_code) %>% arrange(DecimalLongitude)

# replacing "status" with the appropriate color
fill_codes <- as.matrix(historical$status)[,1]
fill_codes <- gsub("Hybrid","#9437FF",gsub("CC","#15326C",gsub("BC","#CE1B26",fill_codes)))

# creating a vector for the position of our sampling site text labels
labelx <- historical$r[length(historical$r)]/2
for (i in (dim(historical)[1]-1):1) {
  labelx <- c(labelx, (labelx[length(labelx)]+(historical$r[i+1]/2)+(historical$r[i]/2)))
}

# creating a vector for the y-position of the labels to stagger them
# and avoid overlap
labely <- rep(4,length(historical$r))
labely[seq(2,length(historical$r),4)] <- 1
labely[seq(3,length(historical$r),4)] <- 3
labely[seq(4,length(historical$r),4)] <- 0

# building the base colored plot
historicallong <- ggplot(historical,aes(y=r,x=2)) + geom_bar(stat="identity",color="black",aes(fill=factor(historical$Location_code,levels=as.numeric(historical$Location_code)))) +
  theme_classic() +
  theme(panel.border = element_blank(),axis.line=element_blank()) +
  scale_x_continuous(limits=c(-1,5)) +
  scale_fill_manual(values = c(fill_codes)) +
  theme (legend.position="none", axis.text = element_blank(),axis.title=element_blank(), axis.ticks = element_blank())  +
  theme(aspect.ratio = 1/5) +
  coord_flip() 

# Figuring out what the colors should be for fill and outline of the 
# first site label
if (historical$Included_in_tess3r[length(historical$Included_in_tess3r)]=="NO") {
  fillcolor <- "#FFFFFF"
} else {
  fillcolor <- "#F2B01F"
}
if (historical$status[length(historical$Included_in_tess3r)]=="Hybrid") {
  colorcolor <- "#9437FF"
} else {
  if (historical$status[length(historical$Included_in_tess3r)]=="CC") {
    colorcolor <- "#15326C"
  }  else {
    colorcolor <- "#CE1B26"
  }
}

# creating a vector to put in some vlines separating the labels
vlinepos <- c(0,historical$r[length(historical$r)])
for (i in (dim(historical)[1]-1):1) {
  vlinepos <- c(vlinepos, (vlinepos[length(vlinepos)]+(historical$r[i])))
}

# and adding this annotation on to the baseplot
historicallong <- historicallong + geom_hline(yintercept = vlinepos, linetype="dashed")

# and adding text label annotation on to the baseplot for left-most site
historicallong <- historicallong + annotate("label",y=labelx[1],x=labely[1],fontface="bold",label=historical$Location_code[length(historical$Included_in_tess3r)],fill=fillcolor,color=colorcolor,size=12)

# and now doing this for all the remaining labels
for (i in 2:length(labelx)) {
  if (historical$Included_in_tess3r[length(historical$Included_in_tess3r)-i+1]=="NO") {
    fillcolor <- "#FFFFFF"
  } else {
    fillcolor <- "#F2B01F"
  }
  if (historical$status[length(historical$Included_in_tess3r)-i+1]=="Hybrid") {
    colorcolor <- "#9437FF"
  } else {
    if (historical$status[length(historical$Included_in_tess3r)-i+1]=="CC") {
      colorcolor <- "#15326C"
    }  else {
      colorcolor <- "#CE1B26"
    }
  }
  historicallong <- historicallong + annotate("label",y=labelx[i],x=labely[i],fontface="bold",label=historical$Location_code[length(historical$Included_in_tess3r)-i+1],fill=fillcolor,color=colorcolor,size=12)
}  

# Exporting as 1500 pixels width, FigS2_historical_long_sites.png
historicallong + scale_y_reverse() 

# 12. Generating site labels, next historical by latitude (the y-axis for the plot)
historical <- historical %>% arrange(Location_code) %>% arrange(desc(DecimalLatitude))

# replacing "status" with the appropriate color
fill_codes <- as.matrix(historical$status)[,1]
fill_codes <- gsub("Hybrid","#9437FF",gsub("CC","#15326C",gsub("BC","#CE1B26",fill_codes)))

# creating a vector for the position of our sampling site text labels
labelx <- historical$r[length(historical$r)]/2
for (i in (dim(historical)[1]-1):1) {
  labelx <- c(labelx, (labelx[length(labelx)]+(historical$r[i+1]/2)+(historical$r[i]/2)))
}

# creating a vector for the y-position of the labels to stagger them
# and avoid overlap
labely <- rep(4,length(historical$r))
labely[seq(2,length(historical$r),4)] <- 1
labely[seq(3,length(historical$r),4)] <- 3
labely[seq(4,length(historical$r),4)] <- 0

# building the base colored plot
historicallat <- ggplot(historical,aes(y=r,x=2)) + geom_bar(stat="identity",color="black",aes(fill=factor(historical$Location_code,levels=as.numeric(historical$Location_code)))) +
  theme_classic() +
  theme(panel.border = element_blank(),axis.line=element_blank()) +
  scale_x_continuous(limits=c(-1,5)) +
  scale_fill_manual(values = c(fill_codes)) +
  theme (legend.position="none", axis.text = element_blank(),axis.title=element_blank(), axis.ticks = element_blank())  +
  theme(aspect.ratio = 5/1)

# Figuring out what the colors should be for fill and outline of the 
# first site label
if (historical$Included_in_tess3r[length(historical$Included_in_tess3r)]=="NO") {
  fillcolor <- "#FFFFFF"
} else {
  fillcolor <- "#F2B01F"
}
if (historical$status[length(historical$Included_in_tess3r)]=="Hybrid") {
  colorcolor <- "#9437FF"
} else {
  if (historical$status[length(historical$Included_in_tess3r)]=="CC") {
    colorcolor <- "#15326C"
  }  else {
    colorcolor <- "#CE1B26"
  }
}

# creating a vector to put in some vlines separating the labels
vlinepos <- c(0,historical$r[length(historical$r)])
for (i in (dim(historical)[1]-1):1) {
  vlinepos <- c(vlinepos, (vlinepos[length(vlinepos)]+(historical$r[i])))
}

# and adding this annotation on to the baseplot
historicallat <- historicallat + geom_hline(yintercept = vlinepos, linetype="dashed")

# and adding text label annotation on to the baseplot for left-most site
historicallat <- historicallat + annotate("label",y=labelx[1],x=labely[1],fontface="bold",label=historical$Location_code[length(historical$Included_in_tess3r)],fill=fillcolor,color=colorcolor,size=12)

# and now doing this for all the remaining labels
for (i in 2:length(labelx)) {
  if (historical$Included_in_tess3r[length(historical$Included_in_tess3r)-i+1]=="NO") {
    fillcolor <- "#FFFFFF"
  } else {
    fillcolor <- "#F2B01F"
  }
  if (historical$status[length(historical$Included_in_tess3r)-i+1]=="Hybrid") {
    colorcolor <- "#9437FF"
  } else {
    if (historical$status[length(historical$Included_in_tess3r)-i+1]=="CC") {
      colorcolor <- "#15326C"
    }  else {
      colorcolor <- "#CE1B26"
    }
  }
  historicallat <- historicallat + annotate("label",y=labelx[i],x=labely[i],fontface="bold",label=historical$Location_code[length(historical$Included_in_tess3r)-i+1],fill=fillcolor,color=colorcolor,size=12)
}  

# Exporting as 1500 pixels height, FigS2_historical_lat_sites.png
historicallat

# 13. Generating site labels, finally modern by latitude (the y-axis for the plot)
modern <- modern %>% arrange(Location_code) %>% arrange(desc(DecimalLatitude))

# replacing "status" with the appropriate color
fill_codes <- as.matrix(modern$status)[,1]
fill_codes <- gsub("Hybrid","#9437FF",gsub("CC","#15326C",gsub("BC","#CE1B26",fill_codes)))

# creating a vector for the position of our sampling site text labels
labelx <- modern$r[length(modern$r)]/2
for (i in (dim(modern)[1]-1):1) {
  labelx <- c(labelx, (labelx[length(labelx)]+(modern$r[i+1]/2)+(modern$r[i]/2)))
}

# creating a vector for the y-position of the labels to stagger them
# and avoid overlap
labely <- rep(4,length(modern$r))
labely[seq(2,length(modern$r),4)] <- 1
labely[seq(3,length(modern$r),4)] <- 3
labely[seq(4,length(modern$r),4)] <- 0

# building the base colored plot
modernlat <- ggplot(modern,aes(y=r,x=2)) + geom_bar(stat="identity",color="black",aes(fill=factor(modern$Location_code,levels=as.numeric(modern$Location_code)))) +
  theme_classic() +
  theme(panel.border = element_blank(),axis.line=element_blank()) +
  scale_x_continuous(limits=c(-1,5)) +
  scale_fill_manual(values = c(fill_codes)) +
  theme (legend.position="none", axis.text = element_blank(),axis.title=element_blank(), axis.ticks = element_blank())  +
  theme(aspect.ratio = 5/1)

# Figuring out what the colors should be for fill and outline of the 
# first site label
if (modern$Included_in_tess3r[length(modern$Included_in_tess3r)]=="NO") {
  fillcolor <- "#FFFFFF"
} else {
  fillcolor <- "#F2B01F"
}
if (modern$status[length(modern$Included_in_tess3r)]=="Hybrid") {
  colorcolor <- "#9437FF"
} else {
  if (modern$status[length(modern$Included_in_tess3r)]=="CC") {
    colorcolor <- "#15326C"
  }  else {
    colorcolor <- "#CE1B26"
  }
}

# creating a vector to put in some vlines separating the labels
vlinepos <- c(0,modern$r[length(modern$r)])
for (i in (dim(modern)[1]-1):1) {
  vlinepos <- c(vlinepos, (vlinepos[length(vlinepos)]+(modern$r[i])))
}

# and adding this annotation on to the baseplot
modernlat <- modernlat + geom_hline(yintercept = vlinepos, linetype="dashed")

# and adding text label annotation on to the baseplot for left-most site
modernlat <- modernlat + annotate("label",y=labelx[1],x=labely[1],fontface="bold",label=modern$Location_code[length(modern$Included_in_tess3r)],fill=fillcolor,color=colorcolor,size=12)

# and now doing this for all the remaining labels
for (i in 2:length(labelx)) {
  if (modern$Included_in_tess3r[length(modern$Included_in_tess3r)-i+1]=="NO") {
    fillcolor <- "#FFFFFF"
  } else {
    fillcolor <- "#F2B01F"
  }
  if (modern$status[length(modern$Included_in_tess3r)-i+1]=="Hybrid") {
    colorcolor <- "#9437FF"
  } else {
    if (modern$status[length(modern$Included_in_tess3r)-i+1]=="CC") {
      colorcolor <- "#15326C"
    }  else {
      colorcolor <- "#CE1B26"
    }
  }
  modernlat <- modernlat + annotate("label",y=labelx[i],x=labely[i],fontface="bold",label=modern$Location_code[length(modern$Included_in_tess3r)-i+1],fill=fillcolor,color=colorcolor,size=12)
}  

# Exporting as 1500 pixels height, FigS2_modern_lat_sites.png
modernlat
