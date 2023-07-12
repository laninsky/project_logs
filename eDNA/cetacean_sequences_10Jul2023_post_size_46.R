#########################
## 1. LOADING PACKAGES ##
#########################

library(tidyverse)

##############################
## 2. SETTING THE DIRECTORY ##
##############################

setwd("/Users/aleal62p/Dropbox (Otago University)/eDNA/10Jul2023_miseq_46")

#########################
## 3. READING IN FILES ##
#########################

# Reading in ASV count per sample
seq_names <- names(read_table("tabulate-feature/metadata.tsv"))
# Getting rid of first column
data <- as_tibble(t(read_table("tabulate-feature/metadata.tsv")))[,-1]
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
data$BP_N_HD_027 <- as.numeric(data$BP_N_HD_027)
data$BP_S_BD01 <- as.numeric(data$BP_S_BD01)
data$BP_S_HD_021 <- as.numeric(data$BP_S_HD_021)
data$TIM_HD_043 <- as.numeric(data$TIM_HD_043)

# Reading in sequences to get species ID
references <- readLines("../cetacean_refseq_mitogenome.fasta")[grep(">",readLines("../cetacean_refseq_mitogenome.fasta"))]
reference_names <- str_sub(references,1,12)
description <- sapply((strsplit(str_sub(references,14,1000),split = " ")), function(x) paste(x[1],x[2],sep=" "))
references <- as_tibble(cbind(reference_names,description))

# Reading in BLAST results
blast_results <- readLines("blast_results.txt")
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

# First three matches look to be cetacean based on match length and identity
blast_results %>% arrange(`1_evalue`) 

blast_results %>% arrange(`1_evalue`) %>% select(seqname,`1_sseqid`,`2_sseqid`)

#seqname                           `1_sseqid`  `2_sseqid` 
#<chr>                             <chr>       <chr>      
#1 >4e9a6f0bd3e6a1ca284be16a317cc540 NC_012059.1 NC_037848.1
#2 >71c396bb977a65796c7e8f8ba0c40300 NC_012059.1 NC_032301.1
#3 >85999a631af7e63c2137ac037a731ccc NC_060610.1 NC_019591.1

# These cetacean matches are...
references %>% filter(reference_names %in% c(">NC_012059.1",">NC_037848.1")) # Tursiops truncatus, Lagenodelphis hosei
references %>% filter(reference_names %in% c(">NC_012059.1",">NC_032301.1")) # Tursiops truncatus, Stenella longirostris
references %>% filter(reference_names %in% c(">NC_060610.1",">NC_019591.1")) # Cephalorhynchus commersonii, Orcaella heinsohni

#>4e9a6f0bd3e6a1ca284be16a317cc540
#CATAAACTATTCCTTGAAAAAAGCTTATTGTACAGTTACCACAACATCACAGTACTACGTCAGTATTAAAAGTAATTTGTTTTAAAAACATTTTACTGTACACATTACATATACATACACATGTGCATGCTAATATTTAGTCTCTCCTTGTAAATATTCATACATACATGCTATGTATTATTGTGCATTCATTTATTTTCCATACGATAAGTTAAAGCCCGTATTAATTATCATTAATTTTACATATTACATAATATGCATGCTCTTACATATTATATCTCCCCTATCAATTTTATCTCCATTATACTCTATGGTCGCTCCATTAGATCACGAGCTTAATCACCATGCCG
# Web-blast search: DQ105706.1	Tursiops truncatus haplotype TTGC05 tRNA-Pro gene and D-loop, partial sequence; mitochondrial	Tursiops truncatus	634	634	100%	3e-177	99.43%	480	DQ105706.1

#>71c396bb977a65796c7e8f8ba0c40300
#CATAAACTATTCCTTGAAAAAGCTTATTGTACAGTTACCACAACACCACAGTACTACGTCAGTATTAAAAGTAATTTGTTTTAAAAACATTTTACTGTACACATTACATATACATACACATGCGCATGCTAATATTTAGTCTCTCCTTGTAAATATTCATATATACATGCTATGTATTATTGTGCATTCATTTATTTTCCATACGATAAGTTAAAGCCCGTATTAATTGTCATTAATTTTACATATTACATAATATGTATGCTCTTACATATTATATCTCCCCTATCAATCCTACCTCCATTATATCCTATGGTCACTCCATTAGATCACGAGCTTAATCACCATGCCG
# Web-blast search: Tursiops truncatus mitochondrial DNA, D-loop region, partial sequence, haplotype: 13	Tursiops truncatus	640	640	100%	6e-179	99.71%	427	AB303166.1

#>85999a631af7e63c2137ac037a731ccc
#CATAAACTATTCCTTGAAAAAAGCTTATTGTATAATTACCACAACCCCACAGTGCCACGTCAGTATTAAAAGTAATTTATTTTAAAAACATTTTACTGTACACATTACATATACACATACACACACCAATACTTATAGTCTCTCCTTGTAAATATTTATATGTACATGCTATGTATTATTGTGCATTCATTTATTTTCCATACGGCAAGTTAAAGCCCGTATTAATTATCATTAATTTTACATATTACATAATATGTATGATCTTACATATTATACATCCCCTAACAATTTTATTTCCATTGTATCCTATGGTCACTCCATTAGATCACGAGCTTAATCACCATGCCG
# Web-blast search: KC312627.1	Cephalorhynchus hectori maui voucher NI36 mitochondrion, partial genome	Cephalorhynchus hectori maui	595	595	94%	1e-165	99.39%	13343	KC312627.1

########################################
## 4. ASSIGNING SEQUENCES AS CETACEAN ##
########################################

data <- data %>% mutate(species_id=ifelse(sequence_names=="71c396bb977a65796c7e8f8ba0c40300","Tursiops truncatus",
                                  ifelse(sequence_names=="4e9a6f0bd3e6a1ca284be16a317cc540", "Tursiops truncatus",
                                         ifelse(sequence_names=="85999a631af7e63c2137ac037a731ccc", "Cephalorhynchus hectori", "Not cetacean"))))

BP_N_HD_027 <- cbind(aggregate(x=data$BP_N_HD_027, by=list(data$species_id), FUN=sum),"BP_N_HD_027")
BP_S_BD01 <- cbind(aggregate(x=data$BP_S_BD01, by=list(data$species_id), FUN=sum),"BP_S_BD01")
BP_S_HD_021 <- cbind(aggregate(x=data$BP_S_HD_021, by=list(data$species_id), FUN=sum),"BP_S_HD_021")
TIM_HD_043 <- cbind(aggregate(x=data$TIM_HD_043, by=list(data$species_id), FUN=sum),"TIM_HD_043")

BP_N_HD_027[,2] <- BP_N_HD_027[,2]/sum(BP_N_HD_027[,2])
BP_S_BD01[,2] <- BP_S_BD01[,2]/sum(BP_S_BD01[,2])
BP_S_HD_021[,2] <- BP_S_HD_021[,2]/sum(BP_S_HD_021[,2])
TIM_HD_043[,2] <- TIM_HD_043[,2]/sum(TIM_HD_043[,2])

names(BP_N_HD_027) <- c("species","read_count","sample")
names(BP_S_BD01) <- c("species","read_count","sample")
names(BP_S_HD_021) <- c("species","read_count","sample")
names(TIM_HD_043) <- c("species","read_count","sample")

summarised_counts <- rbind(BP_N_HD_027,BP_S_BD01,BP_S_HD_021,TIM_HD_043)

ggplot(summarised_counts, aes(x=sample,y=read_count,fill=species)) +
  geom_bar(stat="identity")
