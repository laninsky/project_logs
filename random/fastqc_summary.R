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
    # If the file name in fastqc doesn't have "trimmed" in it
    # If the file name is not in key.txt
    if (length((which(key$X1 %in% fastqc$X3[i])))==0) {
      # Adding the row number to 'undetermined rows' (probably unassigned barcodes)
      undertermined_rows <- c(undertermined_rows,i)
    } else { 
      # If the file name in fastqc doesn't have "trimmed" in it
      # And the file name IS in key.txt
      # Then Sample is whatever sample name the file name corresponds to in key.txt
      Sample[i] <- as.matrix(key[(which(key$X1 %in% fastqc$X3[i])),2])[1,1]
      # And then we determine whether this file is R1 or R2 based on the file ending
      if (grepl("_R1_001.fastq.gz",fastqc$X3[i])) {
        R1_R2[i] <- "R1"
      } else {
        R1_R2[i] <- "R2"
      }
    }
  }
}  

# Sticking our new variables to the tibble
fastqc <- as_tibble(cbind(fastqc,Sample,R1_R2))

# Removing rows not present in the key.txt file (probably undetermined files)
# If there are undetermined_rows
if (!(is.null(undertermined_rows))) {
  fastqc <- fastqc[-undertermined_rows,]
}
    
# Adding a row to say whether trimmed or not
fastqc <- fastqc %>% mutate(Trimmed=ifelse(grepl("trimmed",X3),"Y","N"))

# Merging sample name and whether R1 or R2
fastqc <- fastqc %>% mutate(full_name=paste(Sample,R1_R2,sep="."))

# Selecting columns of interest
fastqc <- fastqc %>% select(full_name, Trimmed, X2, X1)

# Pivot_wider on Trimmed, filling in with pass/fail results
fastqc <- fastqc %>% pivot_wider(names_from=Trimmed,values_from=X1)

# Filtering just for the values where post-trimming warn/fails are still present
# Ignoring 'Per tile sequence quality' where quality hasn't changed between before and after trimming
# Ignoring 'Per sequence GC quality' where quality hasn't changed between before and after trimming
# Ignoring sequence length distribution warning (due to trimming)
comparisons_to_check <- fastqc %>% filter(N!=Y) %>%
  filter(((X2=="Per tile sequence quality"| X2=="Per sequence GC quality") & N!=Y) | (X2!="Per tile sequence quality"| X2!="Per sequence GC quality")) %>%
    filter(X2!="Sequence Length Distribution" | (X2=="Sequence Length Distribution" & !(N=="PASS" & Y=="WARN"))) %>%
      filter(Y!="PASS")

# Grabbing the original file names
sample_names <- gsub(".R1|.R2","",comparisons_to_check$full_name)
trimmed_name <- rep(NA,length(sample_names))
original_name <- rep(NA,length(sample_names))

for (i in 1:length(sample_names)) {
  # Getting whether the sample is R1 or R2
  R1_R2_placeholder <- gsub(paste(sample_names[i],".",sep=""),"",comparisons_to_check[i,1])
  # Recapitulating the trimmed name 
  trimmed_name[i] <-  paste(sample_names[i],".trimmed.",R1_R2_placeholder,".fastq.gz",sep="")
  # Getting the original file name
  if (R1_R2_placeholder=="R1") {
    original_name[i] <- as.matrix(key[which(key$X2 %in% sample_names[i])[1],1])
  } else {
    original_name[i] <- as.matrix(key[which(key$X2 %in% sample_names[i])[2],1])
  }  
}

# Binding this to the comparisons_to_check, giving it informative column names, and writing it out
comparisons_to_check <- as_tibble(cbind(comparisons_to_check,original_name,trimmed_name))
names(comparisons_to_check) <- c("sample","QC parameter","QC_after_trimming","QC_before_trimming","before_trimming_filename","after_trimming_filename")
write_tsv(comparisons_to_check,"comparisons_to_check.txt")



