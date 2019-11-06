# Loading required libraries
if (!require('tidyverse')) install.packages('tidyverse'); library('tidyverse')

# Reading in the fastqc summary
fastqc <- read_tsv(fastqc_summary.txt,col_names=FALSE)

# Creating a sample column
fastqc %>% mutate(Sample=gsub("_L00.*","",X3)) %>% mutate(Read_file=gsub(".trimmed","",gsub(".fastq.gz","",X3)))
