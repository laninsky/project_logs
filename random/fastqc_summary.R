# Loading required libraries
if (!require('tidyverse')) install.packages('tidyverse'); library('tidyverse')

# Reading in the fastqc summary
fastqc <- read_tsv("fastqc_summary.txt",col_names=FALSE)

# Reading in the key.txt file
key <- read_table2("key.txt",col_names=FALSE)

# Creating variables to capture sample name and whether file is R1 or R2
Sample <- rep(NA,dim(fastqc)[1])
R1_R2 <-  rep(NA,dim(fastqc)[1])
undertermined_rows <- NULL

# Working out these values for each of the rows in fastqc
for (i in 1:length(Sample)) {
  # If the file name in fastqc has "trimmed" in it
  if (grepl("trimmed",fastqc$X3[i])) {
    # If this file name has ".trimmed.R1.fastq.gz" in it
    if (grepl(".trimmed.R1.fastq.gz",fastqc$X3[i])) {
      # The R1_R2 should equal "R1"
      R1_R2[i] <- "R1"
      # And Sample should equal file name with ".trimmed.R1.fastq.gz" gsubbed off
      Sample[i] <- gsub(".trimmed.R1.fastq.gz","",fastqc$X3[i])
    } else {
      # Otherwise if the file name has "trimmed" but not ".trimmed.R1.fastq.gz" in it
      # R1_R2 should be "R2"
      R1_R2[i] <- "R2"
      # And Sample should be the file name with ".trimmed.R2.fastq.gz" gsubbed off
      Sample[i] <- gsub(".trimmed.R2.fastq.gz","",fastqc$X3[i])
    }     
  } else {
    if (length((which(key$X1 %in% fastqc$X3[i])))==0) {
      undertermined_rows <- c(undertermined_rows,i)
    } else {  
      Sample[i] <- as.matrix(key[(which(key$X1 %in% fastqc$X3[i])),2])[1,1]
      if (grepl("_R1_001.fastq.gz",fastqc$X3[i])) {
        R1_R2[i] <- "R1"
      } else {
        R1_R2[i] <- "R2"
      }
    }
  }
}  

# Removing rows not present in the key.txt file
fastqc <- fastqc[-undertermined_rows,]

fastqc <- fastqc %>% mutate(Sample=gsub("_L00.*","",X3)) %>% 
  mutate(Read_file=gsub(".trimmed","",gsub(".fastq.gz","",X3))) %>% 
    mutate(Trimmed=ifelse(grepl("trimmed",X3),"Y","N"))

# Selecting columns of interest
fastqc <- fastqc %>% select(Sample, Read_file, Trimmed, X2, X1)

# Pivot_wider on Trimmed, filling in with pass/fail results
fastqc <- fastqc %>% pivot_wider(names_from=Trimmed,values_from=X1)

# Filtering just for the values where post-trimming warn/fails are still present
# Ignoring 'Per tile sequence quality' where quality hasn't changed between before and after trimming
# Ignoring 'Per sequence GC quality' where quality hasn't changed between before and after trimming
# Ignoring sequence length distribution warning (due to trimming)
fastqc %>% filter(N!=Y) %>%
  filter(((X2=="Per tile sequence quality"| X2=="Per sequence GC quality") & N!=Y) | (X2!="Per tile sequence quality"| X2!="Per sequence GC quality")) %>%
    filter(X2!="Sequence Length Distribution" | (X2=="Sequence Length Distribution" & !(N=="PASS" & Y=="WARN"))) %>%
      filter(Y!="PASS")
