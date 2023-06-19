#########################
## 1. LOADING PACKAGES ##
#########################

library(tidyverse)

##############################
## 2. SETTING THE DIRECTORY ##
##############################

setwd("/Users/aleal62p/Dropbox (Otago University)/eDNA/4Apr2023_post_size/")

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

blast_results %>% arrange(desc(`1_length`)) %>% filter(`1_pident`==100,`1_length`>120)
## A tibble: 11 × 25
#   seqname     1_qse…¹ 1_sse…² 1_pid…³ 1_len…⁴ 1_mis…⁵ 1_gap…⁶ 1_qst…⁷ 1_qen…⁸ 1_sst…⁹ 1_sen…˟ 1_eval…˟ 1_bit…˟ 2_qse…˟ 2_sse…˟ 2_pid…˟ 2_len…˟ 2_mis…˟ 2_gap…˟ 2_qst…˟ 2_qen…˟
#   <chr>       <chr>   <chr>     <dbl>   <dbl>   <dbl>   <dbl>   <dbl>   <dbl>   <dbl>   <dbl>    <dbl>   <dbl> <chr>   <chr>     <dbl>   <dbl>   <dbl>   <dbl>   <dbl>   <dbl>
# 1 >230b2d308… Query_1 298e5f…     100     122       0       0       1     122       1     122 1.2 e-61     221 Query_1 05b78f…    96.7     122       4       0       1     122
# 2 >72a1ceacb… Query_1 05b78f…     100     122       0       0       1     122       1     122 1.2 e-61     221 Query_1 298e5f…    96.7     122       4       0       1     122
# 3 >5154b4878… Query_1 33d447…     100     122       0       0       1     122      63     184 1.2 e-61     221 Query_1 ab6d22…   100       122       0       0       1     122
# 4 >55fb51e68… Query_1 b8a8d7…     100     122       0       0       1     122       1     122 1.2 e-61     221 Query_1 b8a8d7…   100       122       0       0       1     122
# 5 >91baea479… Query_1 ba3baa…     100     122       0       0       1     122       1     122 1.2 e-61     221 Query_1 4c5b04…    99.2     122       1       0       1     122
# 6 >761a7261b… Query_1 0e48e6…     100     122       0       0       1     122       1     122 1.2 e-61     221 Query_1 786f35…   100       122       0       0       1     122
# 7 >c4b3f0287… Query_1 28caf9…     100     122       0       0       1     122      84     205 1.2 e-61     221 Query_1 42ac26…   100       122       0       0       1     122
# 8 >aeeba4739… Query_1 7853c9…     100     122       0       0       1     122       1     122 1.2 e-61     221 Query_1 7853c9…   100       122       0       0       1     122
# 9 >0b4b4f723… Query_1 1378ef…     100     122       0       0       1     122       1     122 1.2 e-61     221 Query_1 1378ef…   100       122       0       0       1     122
#10 >2d6dc77b1… Query_1 e31942…     100     122       0       0       1     122       1     122 1.2 e-61     221 Query_1 e31942…   100       122       0       0       1     122
#11 >9ea415194… Query_1 05b78f…     100     121       0       0       1     121       1     121 4.17e-61     219 Query_1 298e5f…    96.7     121       4       0       1     121
## … with 4 more variables: `2_sstart` <dbl>, `2_send` <dbl>, `2_evalue` <dbl>, `2_bitscore` <dbl>, and abbreviated variable names ¹​`1_qseqid`, ²​`1_sseqid`, ³​`1_pident`,
##   ⁴​`1_length`, ⁵​`1_mismatch`, ⁶​`1_gapopen`, ⁷​`1_qstart`, ⁸​`1_qend`, ⁹​`1_sstart`, ˟​`1_send`, ˟​`1_evalue`, ˟​`1_bitscore`, ˟​`2_qseqid`, ˟​`2_sseqid`, ˟​`2_pident`, ˟​`2_length`,
##   ˟​`2_mismatch`, ˟​`2_gapopen`, ˟​`2_qstart`, ˟​`2_qend`
## ℹ Use `colnames()` to see all variable names

# 11 sequences had long, 100% matches to previous problematic off-target sequences
