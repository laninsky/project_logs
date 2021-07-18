# This code corresponds to Fig S10 in Alexander et al.
# It calcualtes statistics presented in the main results text

# 1. Loading necessary libraries
library(tidyverse)

# 2. Setwd
setwd("chickadee/output/")

# 3. Reading in data (tab delimited), dropping last blank row
temp <- read_tsv("../data/Table_S1.txt")
temp <- temp[1:165,]

#4. Appleton City genetic cluster temporal comparisons
# Smithsonian percentages
temp %>% filter(Sampling_period=="SMITHSONIAN") %>% filter(grepl("Appleton",Specific_locality)) %>% mutate(status=ifelse(BC_genetic_cluster_assignment>=0.95,"BC",ifelse(CC_genetic_cluster_assignment>=0.95,"CC","hybrid"))) %>% filter(!(is.na(status))) %>% group_by(status) %>% tally() %>% mutate(perc=n/sum(n)*100)

# Modern percentages
temp %>% filter(Sampling_period=="MODERN") %>% filter(grepl("Appleton",Specific_locality)) %>% mutate(status=ifelse(BC_genetic_cluster_assignment>=0.95,"BC",ifelse(CC_genetic_cluster_assignment>=0.95,"CC","hybrid"))) %>% group_by(status) %>% tally() %>% mutate(perc=n/sum(n)*100)

# Mann-Whitney U-test
smithsonian <- as.matrix(temp %>% filter(Sampling_period=="SMITHSONIAN") %>% filter(grepl("Appleton",Specific_locality)) %>% select(BC_genetic_cluster_assignment))[,1]
smithsonian <- cbind(smithsonian,0)

modern <- as.matrix(temp %>% filter(Sampling_period=="MODERN") %>% filter(grepl("Appleton",Specific_locality)) %>% select(BC_genetic_cluster_assignment))[,1]
modern <- cbind(modern,1)

data <- rbind(smithsonian,modern)
data <- as.data.frame(data)
names(data) <- c("structure","population")
wilcox.test(data$structure~data$population)

#4. Rockville genetic cluster temporal comparisons
# Smithsonian percentages
temp %>% filter(Sampling_period=="SMITHSONIAN") %>% filter(grepl("Rockville",Specific_locality)) %>% mutate(status=ifelse(BC_genetic_cluster_assignment>=0.95,"BC",ifelse(CC_genetic_cluster_assignment>=0.95,"CC","hybrid"))) %>% group_by(status) %>% tally() %>% mutate(perc=n/sum(n)*100)

# Modern percentages
temp %>% filter(Sampling_period=="MODERN") %>% filter(grepl("Rockville",Specific_locality)) %>% mutate(status=ifelse(BC_genetic_cluster_assignment>=0.95,"BC",ifelse(CC_genetic_cluster_assignment>=0.95,"CC","hybrid"))) %>% group_by(status) %>% tally() %>% mutate(perc=n/sum(n)*100)

# Mann-Whitney U-test
smithsonian <- as.matrix(temp %>% filter(Sampling_period=="SMITHSONIAN") %>% filter(grepl("Rockville",Specific_locality)) %>% select(BC_genetic_cluster_assignment))[,1]
smithsonian <- cbind(smithsonian,0)

modern <- as.matrix(temp %>% filter(Sampling_period=="MODERN") %>% filter(grepl("Rockville",Specific_locality)) %>% select(BC_genetic_cluster_assignment))[,1]
modern <- cbind(modern,1)

data <- rbind(smithsonian,modern)
data <- as.data.frame(data)
names(data) <- c("structure","population")
wilcox.test(data$structure~data$population)

#5. Song genetic cluster comparisons
BCsong <- as.matrix(temp %>% filter(Song_summary=="PUREBC") %>% select(BC_genetic_cluster_assignment))[,1]

sd(BCsong)

CCsong <- as.matrix(temp %>% filter(Song_summary=="PURECC") %>% select(CC_genetic_cluster_assignment))[,1]

sd(CCsong)

t.test(BCsong,CCsong)
