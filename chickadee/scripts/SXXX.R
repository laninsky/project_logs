# 1. Loading required libraries and scripts
library(tidyverse)

# 2. Setwd
setwd("chickadee/output/")

# 3A. Reading in structure data (tab delimited) and standardizing names
temp <- read.table("../data/chickadee_structure_file.txt")
temp <- t(temp)
temp[1,seq(1,length(temp[1,]),2)] <- gsub("_.*","_A",temp[1,seq(1,length(temp[1,]),2)])
temp[1,seq(2,length(temp[1,]),2)] <- gsub("_.*","_B",temp[1,seq(2,length(temp[1,]),2)])

# Some minor tweaking of reference sample names based on tissue number used in structure file vs catalog number in S2
temp[1,which(grepl("3474",temp[1,]))] <- c("90612_A","90612_B")
temp[1,which(grepl("6281",temp[1,]))] <- c("95776_A","95776_B")
temp[1,which(grepl("9898",temp[1,]))] <- c("131638_A","131638_B")
temp[1,which(grepl("7420",temp[1,]))] <- c("92269_A","92269_B")
temp[1,which(grepl("7421",temp[1,]))] <- c("92270_A","92270_B")

# 3B. Reading in S2 data (tab delimited), dropping last blank row
tempS2 <- read_table2("../data/S2.txt")
tempS2 <- tempS2[1:165,]

# 3C. Reading in ipyrad summary, including read depth data (tab delimited)
tempread <- read_table2("../data/ipyrad_summary.txt")
tempread[,1] <- as_tibble(gsub("_.*","",as.matrix(tempread[,1])))

# Some minor tweaking of reference sample names based on tissue number used in readfile vs catalog number in S2
tempread[which(grepl("3474",as.matrix(tempread[,1]))),1] <- "90612"
tempread[which(grepl("6281",as.matrix(tempread[,1]))),1] <- "95776"
tempread[which(grepl("9898",as.matrix(tempread[,1]))),1] <- "131638"
tempread[which(grepl("7420",as.matrix(tempread[,1]))),1] <- "92269"
tempread[which(grepl("7421",as.matrix(tempread[,1]))),1] <- "92270"

# 4. Creating a new data object to store our comparisons and populating it with sample codes
norows <- factorial(dim(tempS2)[1])/(factorial(dim(tempS2)[1]-2)*factorial(2))

pairwise_comparisons <- matrix(NA,ncol=6,nrow=norows)
x <- 1
for (i in 1:(dim(tempS2)[1]-1)) {
  for (j in (i+1):((dim(tempS2)[1]))) {
    pairwise_comparisons[x,1] <- as.character(tempS2[i,1])
    pairwise_comparisons[x,2] <- as.character(tempS2[j,1])
    x <- x + 1
  }
}

# 5. Populating our data with things of interest
for (i in 1:dim(pairwise_comparisons)[1]) {
  # Only need to look at one "allele" per sample in structure file, finding those columns
  structure_sample_1 <- which(grepl(pairwise_comparisons[i,1], temp[1,]))[1]
  # The smithsonian samples have the tissue number instead of the catalog - looking in this column if need be
  if (is.na(structure_sample_1)) {
    structure_sample_1 <- which(grepl(as.matrix(tempS2[which(as.matrix(tempS2[,1])==pairwise_comparisons[i,1]),2]), temp[1,]))[1]
  }
  structure_sample_2 <- which(grepl(pairwise_comparisons[i,2], temp[1,]))[1]
  # The smithsonian samples have the tissue number instead of the catalog - looking in this column if need be
  if (is.na(structure_sample_2)) {  
    structure_sample_2 <- which(grepl(as.matrix(tempS2[which(as.matrix(tempS2[,1])==pairwise_comparisons[i,2]),2]), temp[1,]))[1]
  }
  
  # Finding the rows in S2 so we can extract cluster assignments
  S2_sample_1 <- which(grepl(pairwise_comparisons[i,1], as.matrix(tempS2[,1])))
  S2_sample_2 <- which(grepl(pairwise_comparisons[i,2], as.matrix(tempS2[,1])))
  
  # Finding the rows in tempread so we can pull our read depth
  read_sample_1 <- which(grepl(pairwise_comparisons[i,1], as.matrix(tempread[,1])))
  # The smithsonian samples have the tissue number instead of the catalog - looking in this column if need be
  if(length(read_sample_1)==0) {
    as.matrix(tempS2[which(as.matrix(tempS2[,1])==pairwise_comparisons[i,1]),2])
    read_sample_1 <- which(grepl(as.matrix(tempS2[which(as.matrix(tempS2[,1])==pairwise_comparisons[i,1]),2]), as.matrix(tempread[,1])))
  }
  read_sample_2 <- which(grepl(pairwise_comparisons[i,2], as.matrix(tempread[,1])))
  # The smithsonian samples have the tissue number instead of the catalog - looking in this column if need be
  if(length(read_sample_2)==0) {
    read_sample_2 <- which(grepl(as.matrix(tempS2[which(as.matrix(tempS2[,1])==pairwise_comparisons[i,2]),2]), as.matrix(tempread[,1])))
  }  
  
  # Number of loci that overlap between the samples
  pairwise_comparisons[i,3] <- length(which(temp[,structure_sample_1]!="-9" & temp[,structure_sample_2]!="-9"))-1
  
  # Mean cluster assignment across the pair of samples
  pairwise_comparisons[i,4] <- (as.matrix(tempS2[S2_sample_1,3])+as.matrix(tempS2[S2_sample_2,3]))/2
  
  # Absolute difference in cluster assignment between pairs
  pairwise_comparisons[i,5] <- abs(as.matrix(tempS2[S2_sample_1,3])-as.matrix(tempS2[S2_sample_2,3]))
  
  # Mean read depth across the pair of samples
  pairwise_comparisons[i,6] <- (as.numeric(as.matrix(tempread[read_sample_1,3]))+as.numeric(as.matrix(tempread[read_sample_2,3])))/2

}

# 6. Transforming pairwise_comparisons into a tibble so we can aggregate it and get plotting
pairwise_comparisons <- as_tibble(pairwise_comparisons)
names(pairwise_comparisons) <- c("sample_1","sample_2","overlapping_loci","average_cluster","abs_cluster_diff","average_read_cov")
pairwise_comparisons[,3:6] <- pairwise_comparisons[,3:6] %>% mutate_if(is.character, as.numeric)

breakpoints <- seq(0,1,0.02)
breakpoints[1] <- -0.00001
breakpoints[length(breakpoints)] <- 1.00001

# Creating a column aggregating on average cluster
pairwise_comparisons <- pairwise_comparisons %>% mutate(grouped_av_cluster=cut(average_cluster,breaks = breakpoints))

# Creating a column aggregating on abs_cluster_diff
pairwise_comparisons <- pairwise_comparisons %>% mutate(grouped_cluster_diff=cut(abs_cluster_diff,breaks = breakpoints))

# Creating a column aggregating on read depth - first getting appropriate break points
(max(pairwise_comparisons$average_read_cov/1000000)-min(pairwise_comparisons$average_read_cov/1000000))/50

readbreakpoints <- seq(min(pairwise_comparisons$average_read_cov/1000000),max(pairwise_comparisons$average_read_cov/1000000),0.06226106)

pairwise_comparisons <- pairwise_comparisons %>% mutate(grouped_read_depth=cut(average_read_cov/1000000,breaks = readbreakpoints))

# 7. Now grouping for the first plot
firstplot <- pairwise_comparisons %>% group_by(grouped_av_cluster,grouped_cluster_diff) %>% summarise(mean_loci=mean(overlapping_loci),num_pairs=n(),av_cluster=mean(average_cluster),av_diff=mean(abs_cluster_diff))

ggplot(firstplot,aes(x=grouped_cluster_diff,y=grouped_av_cluster,fill=mean_loci)) + geom_tile() + coord_fixed() 

                     