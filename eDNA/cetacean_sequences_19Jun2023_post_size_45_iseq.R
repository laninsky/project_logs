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
references <- readLines("../qiime_output/cetacean_refseq_mitogenome.fasta")[grep(">",readLines("../qiime_output/cetacean_refseq_mitogenome.fasta"))]
reference_names <- str_sub(references,1,12)
description <- sapply((strsplit(str_sub(references,14,1000),split = " ")), function(x) paste(x[1],x[2],sep=" "))
references <- as_tibble(cbind(reference_names,description))

# Reading in BLAST results
blast_results <- readLines("single-end-blast_results.txt")
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

# Differences between cetacean and "junk" based on length. 
blast_results %>% arrange(desc(`1_length`))

#Checking top 7 results, but suspect only top 5 are cetacean
blast_results %>% arrange(desc(`1_length`)) %>% select(seqname,`1_sseqid`,`2_sseqid`)

# A tibble: 2,132 × 3
#   seqname                           `1_sseqid`  `2_sseqid` 
#   <chr>                             <chr>       <chr>      
# 1 >b9b4cc338ab6d82ecec7f071c6c86a99 NC_060610.1 NC_019577.1
# 2 >00d3eb5ff7626e23a0ec71b1d3047897 NC_060610.1 NC_019577.1
# 3 >dd91af9ec9aa7d6e9ee6c5ddce2527b5 NC_060610.1 NC_020696.1
# 4 >b384b5ab4f1e192b560b931ab1f7b131 NC_034236.1 NC_000845.1
# 5 >919ea08296903bdd02ec62c8947496b6 NC_060610.1 NC_020696.1
# 6 >e771ef3766302ac749199f5003f93d04 NC_005274.1 NC_000845.1
# 7 >ccafaff50690f085397dca8e8feda43b NC_005276.1 NC_006929.1

# These cetacean matches are...
references %>% filter(reference_names %in% c(">NC_060610.1",">NC_019577.1")) # Cephalorhynchus commersonii, Pseudorca crassidens 
references %>% filter(reference_names %in% c(">NC_060610.1",">NC_019577.1")) # Cephalorhynchus commersonii, Pseudorca crassidens 
references %>% filter(reference_names %in% c(">NC_060610.1",">NC_020696.1")) # Cephalorhynchus commersonii, Cephalorhynchus heavisidii  
references %>% filter(reference_names %in% c(">NC_034236.1",">NC_000845.1")) # Sus scrofa, Delphinapterus leucas
references %>% filter(reference_names %in% c(">NC_060610.1",">NC_020696.1")) # Cephalorhynchus commersonii, Cephalorhynchus heavisidii 
references %>% filter(reference_names %in% c(">NC_005274.1",">NC_000845.1")) # Sus scrofa, Berardius bairdii 
references %>% filter(reference_names %in% c(">NC_005276.1",">NC_006929.1")) # Balaenoptera borealis, Inia geoffrensis 

# >b9b4cc338ab6d82ecec7f071c6c86a99
# CATAAACTATTCCTTGAAAAAAGCTTATTGTATAATTACCACAACCCCACAGTGCCACGTCAGTATTAAAAGTAATTTATTTTAAAAACATTTTACTGTACACATTACATATACACATACAC
# Web-blast search: Cephalorhynchus commersonii isolate SRR12437578 mitochondrion, complete genome	Cephalorhynchus commersonii	200	200	90%	4e-47	99.10%	16374	NC_060610.1

# >00d3eb5ff7626e23a0ec71b1d3047897
# CATAAACTATTCCTTGAAAAAAGCTTATTGTACAATTACCACAACCCCACAGTGCCACGTCAGTATTAAAAGTAATTTATTTTAAAAACATTTTACTGTACACATTACATATACACATACAC
# Web-blast search: Cephalorhynchus commersonii isolate SRR12437578 mitochondrion, complete genome	Cephalorhynchus commersonii	206	206	90%	9e-49	100.00%	16374	NC_060610.1

# >dd91af9ec9aa7d6e9ee6c5ddce2527b5
# CATAAACTATTCCTTGAAAAAAGCTTATTGTATAATTACCACAACCCCACAGTGCCACGTCAGTATTAAAAGTAATTTATTTTAAAAACATTTTACTGTACACATTACATATACACATACAT
# Web-blast search: Select seq NC_060610.1	Cephalorhynchus commersonii isolate SRR12437578 mitochondrion, complete genome	Cephalorhynchus commersonii	200	200	90%	4e-47	99.10%	16374	NC_060610.1

# >b384b5ab4f1e192b560b931ab1f7b131
# TTCAAAATTATATATCAAAACTCGTTTTTCCCCCAACATTGTGTGTGCGCTCCATACGAACACTGTTCCAGAGATGTCCCGTGAAACCAGCAACCCGTTCACCTCAGATCGGAAGAGCGGTT
# Web-blast search: NO SIGNIFICANT SIMILARITY FOUND [AKA NOT CETACEAN]

# >919ea08296903bdd02ec62c8947496b6
# CATAAACTATTCCTTGAAAAAAGCTTATTGTATAATTACCACAACCCCACAGTGCCACGTCCGTATTAATTGTCATTAATTTTACATATTACATAATATGTATGATCTTACATATTATACAT
# Web-blast search: Select seq NC_060610.1	Cephalorhynchus commersonii isolate SRR12437578 mitochondrion, complete genome	Cephalorhynchus commersonii	122	122	68%	1e-23	92.86%	16374	NC_060610.1
                                            
# >e771ef3766302ac749199f5003f93d04
# ACATTTGAATGACAAGTAATAACAAATGGCGTGAAACCAGCAACCCGTTGGAGCAAGATCGGAAGAGCGGTTCAGCAGGAATGCCGAGACCGATCTCGTATGCCGTCTTCTGCTTGAAAAAA
# Web-blast search: [NOT CETACEAN] Select seq LN590717.1	Cyprinus carpio genome assembly common carp genome, scaffold: LG12, chromosome: 12	Cyprinus carpio	126	126	55%	8e-25	100.00%	12725232	LN590717.1

# >ccafaff50690f085397dca8e8feda43b
# ACTCCATCACGAGTCTTGGCCTCCACGGTTGTTGCGTGAAACCAGCAACCCGTTGGAGCAAGATCGGAAGAGCGGTTCAGCAGGAATGCCGAGACCGATCTCGTATGCCGTCTTCTGCTTGA
# Web-blast search: [NOT CETACEAN] Cyprinus carpio genome assembly common carp genome, scaffold: LG12, chromosome: 12	Cyprinus carpio	117	117	51%	5e-22	100.00%	12725232	LN590717.1

                      
########################################
## 4. ASSIGNING SEQUENCES AS CETACEAN ##
########################################

# Pulling out site names and feature names                      
sample_names <- names(data)[-1]
feature_names <- c(data[,1])$sequence_names

# Tranposing the data and adding site/feature names back in                      
data_transposed <- as_tibble(cbind(sample_names,t(data[,-1])))
names(data_transposed) <- c("sites",feature_names)

# Changing to as_numeric                      
data_transposed[,-1] <- data_transposed[,-1] %>% mutate_if(is.character,as.numeric)

# Adding column for total read count, cetacean read count, non-cetacean read count
data_transposed <- data_transposed %>% mutate(total_read_count = rowSums(across(where(is.numeric))))
data_transposed <- data_transposed %>% mutate(cetacean_read_count = `b9b4cc338ab6d82ecec7f071c6c86a99`+`00d3eb5ff7626e23a0ec71b1d3047897`+`dd91af9ec9aa7d6e9ee6c5ddce2527b5`+`919ea08296903bdd02ec62c8947496b6`)
data_transposed <- data_transposed %>% mutate(notcetacean_read_count = total_read_count-cetacean_read_count)
data_transposed <- data_transposed %>% mutate(cetacean_data = ifelse(cetacean_read_count>0, "YES", "NO"))

# How many samples have cetacean DNA?
data_transposed %>% group_by(cetacean_data) %>% summarise(n())
## A tibble: 2 × 2
#  cetacean_data `n()`
#  <chr>         <int>
#1 NO               88
#2 YES               8

# Which samples?
data_transposed %>% filter(cetacean_data=="YES") %>% select(sites)
# 1 BP_AKH_HD_030B
# 2 BP_AKH_HD014A 
# 3 BP_AKH_HD015A 
# 4 BP_N_HD_028B  
# 5 BP_N_HD_029A  
# 6 BP_N_HD11A    
# 7 BP_S_HD_024B  
# 8 BP_S_HD01D 
# Interestingly no A and B - suggests perhaps our coverage isn't high enough                      

# Verifying negatives/blanks have low coverage
data_transposed <- data_transposed %>% mutate(negative_blank = ifelse(sites %in% c("BLANK_03.17A","BLANK_03.17B","NegativeA","NegativeB","NegativeC","NegativeD","BP_E_CNTRL_02A","BP_E_CNTRL_02B",
                                                                                   "BP_S_CTRL_01A","BP_S_CTRL_01B","TIM_CNTRL_01A","TIM_CNTRL_01B"),"YES","NO"))

# Some have pretty high coverage, however none have dolphin DNA
ggplot() + geom_point(data=data_transposed, mapping=aes(x=cetacean_data,y=total_read_count,color=negative_blank))

# The controls have high read counts, but given these are likely plankton without dolphin, that makes sense!
data_transposed %>% filter(negative_blank=="YES") %>% select(sites,total_read_count)
## A tibble: 12 × 2
#   sites          total_read_count
#   <chr>                     <dbl>
# 1 BLANK_03.17A                  1
# 2 BLANK_03.17B                  6
# 3 BP_E_CNTRL_02A             1480
# 4 BP_E_CNTRL_02B             3694
# 5 BP_S_CTRL_01A              1532
# 6 BP_S_CTRL_01B              2777
# 7 NegativeA                   154
# 8 NegativeB                   290
# 9 NegativeC                   751
#10 NegativeD                    53
#11 TIM_CNTRL_01A              2423
#12 TIM_CNTRL_01B              1861

# Plotting just the samples with dolphin
dolphin_only <- data_transposed %>% filter(cetacean_data=="YES") %>% 
                      mutate(cetacean_read_count=cetacean_read_count/total_read_count,notcetacean_read_count=notcetacean_read_count/total_read_count) %>% 
                      select(sites,cetacean_read_count,notcetacean_read_count) %>% 
                      pivot_longer(!sites,names_to="species",values_to="read_count")

ggplot(dolphin_only, aes(x=sites,y=read_count,fill=species)) +
  geom_bar(stat="identity")

# Looks like the overrepresentation of the non-target sequence has still occured in this run. Will double cbeck by searching for the "trouble child" from last time
