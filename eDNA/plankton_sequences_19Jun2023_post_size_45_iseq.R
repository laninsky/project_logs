#########################
## 1. LOADING PACKAGES ##
#########################

library(tidyverse)

##############################
## 2. SETTING THE DIRECTORY ##
##############################

setwd("/Users/aleal62p/Dropbox (Otago University)/eDNA/19Jun2023_post_size_45/")

#########################
## 3. READING IN FILES ##
#########################

# Reading in ASV count per sample
seq_names <- names(read_table("single-end-tabulate-feature/metadata.tsv"))
# Getting rid of first column
data <- as_tibble(t(read_table("single-end-tabulate-feature/metadata.tsv")))[,-1]
# Extracting sample names
col_names <- as.character(data[1,])
# Removing the sample names from the data
data <- data[-1,]
# Adding the names back in as column names
names(data) <- col_names
# Adding the sequence names in
data <- as_tibble(cbind(seq_names[-1],data))
# Adding a more informative name
names(data)[1] <- "sequence_names"

# Convering to numeric
data[,-1] <- data[,-1] %>% mutate_if(is.character,as.numeric)

# Reading in sequences to get species ID
references <- readLines("../planktonic_contam.fasta")[grep(">",readLines("../planktonic_contam.fasta"))]
reference_names <- str_sub(references,1,12)

# Reading in BLAST results
blast_results <- readLines("single-end-plankton_blast_results.txt")
unlisted_blast_results <- unlist(strsplit(blast_results," "))
padded_blast_results <- unlisted_blast_results[1]
for (i in 2:length(unlisted_blast_results)) {
  if(all(grepl(">",unlisted_blast_results[i]),grepl(">",unlisted_blast_results[i-1]))) {
    padded_blast_results <- c(padded_blast_results,rep("",24),unlisted_blast_results[i])
  } else {
    padded_blast_results <- c(padded_blast_results,unlisted_blast_results[i])
  }
} 

if(grepl(">",unlisted_blast_results[i])) {
  padded_blast_results <- c(padded_blast_results,rep("",24))
}

blast_results <- as_tibble(matrix(padded_blast_results,byrow = TRUE,ncol=25))
names(blast_results) <- c("seqname","1_qseqid", "1_sseqid", "1_pident", "1_length", "1_mismatch", "1_gapopen", 
                          "1_qstart", "1_qend", "1_sstart", "1_send", "1_evalue", "1_bitscore",
                          "2_qseqid", "2_sseqid", "2_pident", "2_length", "2_mismatch", "2_gapopen", 
                          "2_qstart", "2_qend", "2_sstart", "2_send", "2_evalue", "2_bitscore"
                          )

blast_results <- blast_results %>% mutate(`1_pident`=as.numeric(`1_pident`),
                         `1_length`=as.numeric(`1_length`),
                         `1_mismatch`=as.numeric(`1_mismatch`),
                         `1_gapopen`=as.numeric(`1_gapopen`),
                         `1_qstart`=as.numeric(`1_qstart`),
                         `1_qend`=as.numeric(`1_qend`),
                         `1_sstart`=as.numeric(`1_sstart`),
                         `1_send`=as.numeric(`1_send`),
                         `1_evalue`=as.numeric(`1_evalue`),
                         `1_bitscore`=as.numeric(`1_bitscore`),
                         `2_pident`=as.numeric(`2_pident`),
                         `2_length`=as.numeric(`2_length`),
                         `2_mismatch`=as.numeric(`2_mismatch`),
                         `2_gapopen`=as.numeric(`2_gapopen`),
                         `2_qstart`=as.numeric(`2_qstart`),
                         `2_qend`=as.numeric(`2_qend`),
                         `2_sstart`=as.numeric(`2_sstart`),
                         `2_send`=as.numeric(`2_send`),
                         `2_evalue`=as.numeric(`2_evalue`),
                         `2_bitscore`=as.numeric(`2_bitscore`))

# Differences between cetacean and "plankton" based on length. 
blast_results %>% arrange(desc(`1_length`)) %>% filter(`1_pident`==100,`1_length`>120)
