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

# Very strong difference between cetacean and "junk" based on length. 
blast_results %>% arrange(desc(`1_length`))

# THE FOLLOWING WILL ALL NEED TO BE UPDATED #

#Top 6 results seem to be cetacean based on this
blast_results %>% arrange(desc(`1_length`)) %>% select(seqname,`1_sseqid`,`2_sseqid`)

#seqname                           `1_sseqid`  `2_sseqid` 
#<chr>                             <chr>       <chr>      
#1 >a31e5fa2eeac02d42249625b69fe5c7f NC_012053.1 NC_012061.1
#2 >3d9ff2222c918ac289b6597e534864e1 NC_012059.1 NC_012053.1
#3 >c49b0051f3157b4bcb824c0778598f5e NC_012053.1 NC_012061.1
#4 >5c75dbc6dd3f5a597d86193aa64d9918 NC_012059.1 NC_012053.1
#5 >b9b4cc338ab6d82ecec7f071c6c86a99 NC_060610.1 NC_019577.1
#6 >8a899ba27137c3e8d2e2d9d3b534eeba NC_012059.1 NC_012053.1

# These cetacean matches are...
references %>% filter(reference_names %in% c(">NC_012053.1",">NC_012061.1")) # Delphinus capensis, Stenella coeruleoalba
references %>% filter(reference_names %in% c(">NC_012059.1",">NC_012053.1")) # Tursiops truncatus, Stenella coeruleoalba
references %>% filter(reference_names %in% c(">NC_012053.1",">NC_012061.1")) # Delphinus capensis, Stenella coeruleoalba
references %>% filter(reference_names %in% c(">NC_012059.1",">NC_012053.1")) # Tursiops truncatus, Stenella coeruleoalba
references %>% filter(reference_names %in% c(">NC_060610.1",">NC_019577.1")) # Cephalorhynchus commersonii
references %>% filter(reference_names %in% c(">NC_012059.1",">NC_012053.1")) # Pseudorca crassidens 

#>a31e5fa2eeac02d42249625b69fe5c7f
#CATAAACTATTCCTTGAAAAAGCTTATTGTACAGTTACCACAACACCACAGTACTACGTCAGTATTAAAAGTAATTTGTTTTAAAAACATTTTACTGTACACATTACATATACATACACATC
# Web-blast search: Select seq JN944261.1	Tursiops truncatus haplotype 8Tt069hpl tRNA-Pro gene and control region, partial sequence; mitochondrial	Tursiops truncatus	224	224	99%	2e-54	100.00%	430	JN944261.1

#>3d9ff2222c918ac289b6597e534864e1
#CATAAACTATTCCTTGAAAAAAGCTTATTGTACAGTTACCACAACATCACAGTACTACGTCAGTATTAAAAGTAATTTGTTTTAAAAACATTTTACTGTACACATTACATATACATACACAT
# Web-blast search: Tursiops truncatus gephyreus voucher SEFSC:MMMGL:41Tt216 tRNA-Pro gene, complete sequence; and control region, partial sequence; mitochondrial	Tursiops truncatus gephyreus	226	226	100%	6e-55	100.00%	489	MK105886.1

#>c49b0051f3157b4bcb824c0778598f5e
#CATAAACTATTCCTTGAAAAAGCTTATTGTACAGTTACCACAACACCACAGTACTACGTCAGTATTAAAAGTAATTTGTATTAAAAACATTTTACTGTACACATTACATATACATACACATC
# Web-blast search: Tursiops truncatus haplotype 8Tt069hpl tRNA-Pro gene and control region, partial sequence; mitochondrial	Tursiops truncatus	219	219	99%	1e-52	99.17%	430	JN944261.1

#>5c75dbc6dd3f5a597d86193aa64d9918
#CATAAACTATTCCTTGAAAAAAGCTTATTGTACAGTTACCACAACATCACGGTACTACGTCAGTATTAAAAGTAATTTGTTTTAAAAACATTTTACTGTACACATTACATATACATACACAT
# Web-blast search: Select seq MK105886.1	Tursiops truncatus gephyreus voucher SEFSC:MMMGL:41Tt216 tRNA-Pro gene, complete sequence; and control region, partial sequence; mitochondrial	Tursiops truncatus gephyreus	220	220	100%	3e-53	99.18%	489	MK105886.1

#>b9b4cc338ab6d82ecec7f071c6c86a99
#CATAAACTATTCCTTGAAAAAAGCTTATTGTATAATTACCACAACCCCACAGTGCCACGTCAGTATTAAAAGTAATTTATTTTAAAAACATTTTACTGTACACATTACATATACACATACAC
# Web-blast search: Cephalorhynchus commersonii isolate SRR12437578 mitochondrion, complete genome	Cephalorhynchus commersonii	200	200	90%	4e-47	99.10%	16374	NC_060610.1

#>8a899ba27137c3e8d2e2d9d3b534eeba
#CATCAGTATTAAAAGTAATTTGTTTTAAAAACATTTTACTGTACACATTACATATACATACACATGCGCATGCTAATATTTAGTCTCTCCTTGTAAATATTCATATATACATGCTATGTATT
# Web-blast search: Tursiops truncatus haplotype OTtr63 tRNA-Pro gene and control region, partial sequence; mitochondrial	Tursiops truncatus	226	226	100%	6e-55	100.00%	488	JN944223.1

########################################
## 4. ASSIGNING SEQUENCES AS CETACEAN ##
########################################

data <- data %>% mutate(species_id=ifelse(sequence_names=="a31e5fa2eeac02d42249625b69fe5c7f","Tursiops truncatus",
                                          ifelse(sequence_names=="3d9ff2222c918ac289b6597e534864e1","Tursiops truncatus",
                                                 ifelse(sequence_names=="c49b0051f3157b4bcb824c0778598f5e","Tursiops truncatus",
                                                        ifelse(sequence_names=="5c75dbc6dd3f5a597d86193aa64d9918","Tursiops truncatus",
                                                               ifelse(sequence_names=="b9b4cc338ab6d82ecec7f071c6c86a99","Cephalorhynchus hectori",
                                                                    ifelse(sequence_names=="dd91af9ec9aa7d6e9ee6c5ddce2527b5", "Tursiops truncatus", "Not cetacean")))))))

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
