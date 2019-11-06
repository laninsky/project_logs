# Loading required libraries
if (!require('tidyverse')) install.packages('tidyverse'); library('tidyverse')

# Reading in the fastqc summary
fastqc <- read_tsv(fastqc_summary.txt,col_names=FALSE)

# Creating columns so that we can carry out comparisons
fastqc <- fastqc %>% mutate(Sample=gsub("_L00.*","",X3)) %>% 
  mutate(Read_file=gsub(".trimmed","",gsub(".fastq.gz","",X3))) %>% 
    mutate(Trimmed=ifelse(grepl("trimmed",X3),"Y","N"))
