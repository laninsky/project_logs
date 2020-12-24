# This code corresponds to Fig S18 in Alexander et al.
# It creates Fig S8 (distribution of sample depth across the 8,056 SNPs in the STRUCTURE dataset)
# in the supplementary materials of of Alexander et al.

# 1. Loading required library
library(tidyverse)

# 2. Setwd
setwd("chickadee/output/")

# 3. Loading in STRUCTURE  file
temp <- read_delim("../data/chickadee_singleton_filtered.stru",delim=" ",col_names = FALSE)

# 4. Creating a count of how many samples are present for each SNP
sample_count <- as.data.frame(matrix(colSums(temp!=-9)/2,ncol=1))
names(sample_count) <- "No_of_samples"

quantile_values <- quantile(sample_count[,1])
fill_values <- cbind(seq(min(quantile_values),max(quantile_values),1),"gray70")
fill_values[which(fill_values[,1]==round(mean(sample_count[,1]))),2] <- "forestgreen"
fill_values[which(fill_values[,1]==quantile_values[1]),2] <- "darkviolet"
fill_values[which(fill_values[,1]==quantile_values[2]),2] <- "blue"
fill_values[which(fill_values[,1]==quantile_values[3]),2] <- "yellow"
fill_values[which(fill_values[,1]==quantile_values[4]),2] <- "darkorange"
fill_values[which(fill_values[,1]==quantile_values[5]),2] <- "red"

# 5. plotting the data
ggplot() + geom_bar(data=sample_count, aes(x=No_of_samples),fill=fill_values[,2],color="black") +
  annotate("text", x = quantile_values[1]+7, y = 50, label = paste("Minimum (",quantile_values[1],")",sep=""),color="darkviolet",size=15) +
  annotate("text", x = quantile_values[2], y = 45, label = paste("L.Q. (",quantile_values[2],")",sep=""),color="blue",size=15) +
  annotate("text", x = round(mean(sample_count[,1])), y = 60, label = paste("Mean (",round(mean(sample_count[,1])),")",sep=""),color="forestgreen",size=15) +
  annotate("text", x = quantile_values[3], y = 125, label = paste("Median (",quantile_values[3],")",sep=""),color="yellow3",size=15) +
  annotate("text", x = quantile_values[4]-1, y = 250, label = paste("U.Q. (",quantile_values[4],")",sep=""),color="darkorange",size=15) +
  annotate("text", x = quantile_values[5]-10, y = 590, label = paste("Maximum (",quantile_values[5],")",sep=""),color="red",size=15) +
  scale_x_continuous(name="Completeness (number of samples)",expand=c(0,0),breaks=seq(0,160,10)) +
  scale_y_log10(name="Number of SNPs at each level\n of completeness",expand=c(0,0),breaks=c(1,2,4,6,8,10,20,40,60,80,100,200,400,600,800)) +
  theme_bw(base_size=66) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  theme(axis.title=element_text(size=80,face="bold"))

# Saved manually as a plot 4000 pixels wide * 2000 pixels wall
# Fig_S8_missingness.png
