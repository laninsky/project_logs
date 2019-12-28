# This code corresponds to Fig SY in Alexander et al.
# It creates Fig. S7 (distribution of missingness of data) in the 
# supplementary materials of Alexander et al.

# 1. Loading required library
library(tidyverse)

# 2. Setwd
setwd("chickadee/output/")

# 3. Reading in structure data (tab delimited) and standardizing names
temp <- read.table("../data/chickadee_structure_file.txt")
temp <- as_tibble(t(temp))
temp <- temp[-1,]

# 4. Counting the number of samples that have data for each locus
no_of_cols <- dim(temp)[2]
sample_count <- rep(NA,dim(temp)[1])
for (i in 1:dim(temp)[1]) {
  sample_count[i] <- (no_of_cols-sum(grepl("-9",temp[i,])))/2
}

# Getting frequency of each sample no
freq_count <- tibble(sample_count) %>% group_by(sample_count) %>% summarise(n=n())

# 5. Summarizing and plotting the data
maximum_count <- max(sample_count)
mean_count <- mean(sample_count)
quart75_count <- quantile(sample_count,0.75)
median_count <- median(sample_count)
quart25_count <- quantile(sample_count,0.25)
minimum_count <- min(sample_count)

ggplot() +
  geom_col(data=freq_count,aes(x=sample_count,y=n),color="black",fill="grey60") +
  geom_vline(xintercept = maximum_count, color = "red", size=1.5, linetype="dashed") +
  geom_vline(xintercept = mean_count, color = "orange", size=1.5, linetype="dashed") +
  geom_vline(xintercept = quart75_count, color = "yellow", size=1.5, linetype="dashed") +
  geom_vline(xintercept = median_count, color = "green", size=1.5, linetype="dashed") +
  geom_vline(xintercept = quart25_count, color = "dodgerblue1", size=1.5, linetype="dashed") +
  geom_vline(xintercept = minimum_count, color = "purple", size=1.5, linetype="dashed") +
  theme_bw(base_size=20) +
  scale_y_log10(name="Number of SNPs",expand=c(0,0)) +
  scale_x_continuous(name="Number of samples with data for SNP",expand=c(0,0)) +
  theme(axis.title=element_text(size=24,face="bold")) +
  theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank()) +
  annotate("text", x = 87, y = 2000, label = "bold(Key)",size=6,color="black", parse=TRUE) +
  annotate("text", x = 87, y = 1300, label = "Maximum (160)",size=6,color="red") +
  annotate("text", x = 87, y = 800, label = "Mean (17)",size=6,color="darkorange3") +
  annotate("text", x = 87, y = 500, label = "75th quartile (16)",size=6,color="orange") +
  annotate("text", x = 87, y = 300, label = "Median (7)",size=6,color="green3") +
  annotate("text", x = 87, y = 190, label = "25th quartile (5)",size=6,color="dodgerblue3") +
  annotate("text", x = 87, y = 120, label = "Minimum (1)",size=6,color="purple")
  
# Saved manually as a plot 2000 high * 1000 pixels wide, Fig_S7_missing_data.png
