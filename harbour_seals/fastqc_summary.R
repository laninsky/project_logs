# Loading required libraries
if (!require('tidyverse')) install.packages('tidyverse'); library('tidyverse')

# Reading in the fastqc summary
fastqc <- read_tsv("fastqc_summary.txt",col_names=FALSE)

# Creating columns so that we can carry out comparisons
fastqc <- fastqc %>% mutate(Sample=gsub("_L00.*","",X3)) %>% 
  mutate(Read_file=gsub(".trimmed","",gsub(".fastq.gz","",X3))) %>% 
    mutate(Trimmed=ifelse(grepl("trimmed",X3),"Y","N"))

# Selecting columns of interest
fastqc <- fastqc %>% select(Sample, Read_file, Trimmed, X2, X1)

# Pivot_wider on Trimmed, filling in with pass/fail results
fastqc <- fastqc %>% pivot_wider(names_from=Trimmed,values_from=X1)

# Filtering just for the values where post-trimming warn/fails are still present
