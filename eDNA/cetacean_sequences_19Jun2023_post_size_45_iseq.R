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

#Checking top 20 results, but suspect only top 14 are cetacean
blast_results %>% arrange(desc(`1_length`)) %>% select(seqname,`1_sseqid`,`2_sseqid`)

## A tibble: 1,490 × 3
#   seqname                           `1_sseqid`  `2_sseqid` 
#   <chr>                             <chr>       <chr>      
# 1 >c56aeaa2e20146e2b600719a8636b8ca NC_019441.1 NC_019590.1
# 2 >6fb709e98fed8e24c76527b3543266a5 NC_019441.1 NC_019590.1
# 3 >dc2362156d12f6395f1cac37a46394c4 NC_060612.1 NC_012061.1
# 4 >b9b4cc338ab6d82ecec7f071c6c86a99 NC_060610.1 NC_019577.1
# 5 >00d3eb5ff7626e23a0ec71b1d3047897 NC_060610.1 NC_019577.1
# 6 >a9efc975a214150079b7a53dddc66341 NC_060610.1 NC_019577.1
# 7 >2af1040b6f0ab4aa22a5c5bed20b4a5e NC_060610.1 NC_035426.1
# 8 >7cd3f01672548ce22287163eae26764a NC_060610.1 NC_020696.1
# 9 >64e01782c07ab080afcbd732aec0ea6f NC_060610.1 NC_019577.1
#10 >3eeab7673e92d88a4db910bf24ba0b18 NC_060610.1 NC_019577.1
#11 >b3b59906612af3bd9bbb17ab1f6e3eff NC_060610.1 NC_019577.1
#12 >d3b6c31a9253146da682e1562425081c NC_060610.1 NC_019577.1
#13 >0f8f54b8998d0491a2d8bbddd3cc8593 NC_060610.1 NC_019577.1
#14 >1802c9d7430c86b30ea549cf6db935a5 NC_060610.1 NC_019577.1
#15 >096d8c95e394208836d5d1644d94436a NC_000845.1 NC_005277.1
#16 >a6b5c8e92f36ebf590a9be762f5f7734 NC_000845.1 NC_005277.1
#17 >a92428e781bce1d985ffda1e0b33420f NC_005269.1 NC_006927.1
#18 >b384b5ab4f1e192b560b931ab1f7b131 NC_034236.1 NC_000845.1
#19 >4e23ccc8fe9ff89db8896f0f0a8bb7e2 NC_006853.1 NC_019441.1
#20 >98c6ff9abcb46484595276c5d6a3672f NC_019591.1 NC_020696.1

# These cetacean matches are...
references %>% filter(reference_names %in% c(">NC_019441.1",">NC_019590.1")) # Orcaella brevirostris, Globicephala melas 
references %>% filter(reference_names %in% c(">NC_019441.1",">NC_019590.1")) # Orcaella brevirostris, Globicephala melas 
references %>% filter(reference_names %in% c(">NC_060612.1",">NC_012061.1")) # Stenella frontalis, Delphinus capensis
references %>% filter(reference_names %in% c(">NC_060610.1",">NC_019577.1")) # Cephalorhynchus commersonii, Pseudorca crassidens 
references %>% filter(reference_names %in% c(">NC_060610.1",">NC_019577.1")) # Cephalorhynchus commersonii, Pseudorca crassidens 
references %>% filter(reference_names %in% c(">NC_060610.1",">NC_019577.1")) # Cephalorhynchus commersonii, Pseudorca crassidens 
references %>% filter(reference_names %in% c(">NC_060610.1",">NC_035426.1")) # Cephalorhynchus commersonii, Lagenorhynchus obliquidens
references %>% filter(reference_names %in% c(">NC_060610.1",">NC_020696.1")) # Cephalorhynchus commersonii, Cephalorhynchus heavisidii 
references %>% filter(reference_names %in% c(">NC_060610.1",">NC_019577.1")) # Cephalorhynchus commersonii, Pseudorca crassidens 
references %>% filter(reference_names %in% c(">NC_060610.1",">NC_019577.1")) # Cephalorhynchus commersonii, Pseudorca crassidens 
references %>% filter(reference_names %in% c(">NC_060610.1",">NC_019577.1")) # Cephalorhynchus commersonii, Pseudorca crassidens 
references %>% filter(reference_names %in% c(">NC_060610.1",">NC_019577.1")) # Cephalorhynchus commersonii, Pseudorca crassidens 
references %>% filter(reference_names %in% c(">NC_060610.1",">NC_019577.1")) # Cephalorhynchus commersonii, Pseudorca crassidens 
references %>% filter(reference_names %in% c(">NC_060610.1",">NC_019577.1")) # Cephalorhynchus commersonii, Pseudorca crassidens 
references %>% filter(reference_names %in% c(">NC_000845.1",">NC_005277.1")) # Sus scrofa, Pontoporia blainvillei
references %>% filter(reference_names %in% c(">NC_000845.1",">NC_005277.1")) # Sus scrofa, Pontoporia blainvillei
references %>% filter(reference_names %in% c(">NC_005269.1",">NC_006927.1")) # Megaptera novaeangliae, Caperea marginata 
references %>% filter(reference_names %in% c(">NC_034236.1",">NC_000845.1")) # Sus scrofa, Delphinapterus leucas
references %>% filter(reference_names %in% c(">NC_006853.1",">NC_019441.1")) # Bos taurus, Globicephala melas
references %>% filter(reference_names %in% c(">NC_019591.1",">NC_020696.1")) # Cephalorhynchus heavisidii, Orcaella heinsohni 

#>c56aeaa2e20146e2b600719a8636b8ca
#CATAAACTTTCCTTGAAAAAGGCTTATTGTAGAGTTACCATAACATTATAGTACTATGTCAGTATTAAAAGTAATTTGTTTTAAAAACATTTTATTGTACGCATCACATACATATATACACA
# Select seq OX596401.1	Stenella coeruleoalba genome assembly, chromosome: 16	Stenella coeruleoalba	220	220	100%	3e-53	99.19%	85750194	OX596401.1
# SUSPECT A LAG - DROPPED
                      
#>6fb709e98fed8e24c76527b3543266a5
#CATAAACTTTCCTTGAAAAAGGCTTATTGTAGAGTTACCATAACATTATAGTACTATGTCAGTATTAAAAGTAATTTGTTTTAAAAACATTTTATTGTACGCATCACATACCTATATACACA
# Select seq OX596401.1	Stenella coeruleoalba genome assembly, chromosome: 16	Stenella coeruleoalba	215	215	100%	2e-51	98.37%	85750194	OX596401.1
# SUSPECT A LAG - DROPPED
                      
#>dc2362156d12f6395f1cac37a46394c4
#CATAAACTATTCCTTGAAAAAAGCTTATTGTACAATTACCACAACCTCACAGTGCTACGTCAGTATTAAAAGTAATTTGTTTTAAAAACATTTTACTGTACACATTACATACACATATACAC
# Select seq EF682622.1	Delphinus delphis isolate DDM29 tRNA-Thr gene, partial sequence; tRNA-Pro gene, complete sequence; and D-loop, partial sequence; mitochondrial	Delphinus delphis	209	209	100%	7e-50	97.54%	605	EF682622.1
# ALSO NOT SUPER CLOSE TO CEPH - COULD BE COMMON DOLPHIN
                      
#>b9b4cc338ab6d82ecec7f071c6c86a99
#CATAAACTATTCCTTGAAAAAAGCTTATTGTATAATTACCACAACCCCACAGTGCCACGTCAGTATTAAAAGTAATTTATTTTAAAAACATTTTACTGTACACATTACATATACACATACAC
# Select seq NC_060610.1	Cephalorhynchus commersonii isolate SRR12437578 mitochondrion, complete genome	Cephalorhynchus commersonii	200	200	90%	4e-47	99.10%	16374	NC_060610.1
                      
#>00d3eb5ff7626e23a0ec71b1d3047897
#CATAAACTATTCCTTGAAAAAAGCTTATTGTACAATTACCACAACCCCACAGTGCCACGTCAGTATTAAAAGTAATTTATTTTAAAAACATTTTACTGTACACATTACATATACACATACAC
# Select seq NC_060610.1	Cephalorhynchus commersonii isolate SRR12437578 mitochondrion, complete genome	Cephalorhynchus commersonii	206	206	90%	9e-49	100.00%	16374	NC_060610.1
                      
#>a9efc975a214150079b7a53dddc66341
#CATAAACTATTCCTTGAAAAAAGCTTATTGTATAATTACCACAACCCCACAGTGCCACGTCAGTATTAAAAGTAATTTATTTTAAAATCATTTTACTGTACACATTACATATACACATACAC
# Select seq NC_060610.1	Cephalorhynchus commersonii isolate SRR12437578 mitochondrion, complete genome	Cephalorhynchus commersonii	195	195	90%	2e-45	98.20%	16374	NC_060610.1
                      
#>2af1040b6f0ab4aa22a5c5bed20b4a5e
#CATAAACTATTCCTTGAAAAAAGCTTATTGTACAATTACCACAACCCCACAGTGCCACGTCAGTATTAAAAGTGATTTATTTTAAAAACATTTTACTGTACACATTACATATACACATACAC
# Select seq NC_060610.1	Cephalorhynchus commersonii isolate SRR12437578 mitochondrion, complete genome	Cephalorhynchus commersonii	200	200	90%	4e-47	99.10%	16374	NC_060610.1
                      
#>7cd3f01672548ce22287163eae26764a
#CATAAACTATTCCTTGAAAAAGGCTTATTGTATAATTACCACAACCCCACAGTGCCACGTCAGTATTAAAAGTAATTTATTTTAAAAACATTTTACTGTACACATTACATATACACATACAC
# Select seq NC_060610.1	Cephalorhynchus commersonii isolate SRR12437578 mitochondrion, complete genome	Cephalorhynchus commersonii	195	195	90%	2e-45	98.20%	16374	NC_060610.1
                      
#>64e01782c07ab080afcbd732aec0ea6f
#CATAAACTATTCCTTGAAAAAAGCTTATTGCATAATTACCACAACCCCACAGTGCCACGTCAGTATTAAAAGTAATTTATTTTAAAAACATTTTACTGTACACATTACATATACACATACAC
# Select seq NC_060610.1	Cephalorhynchus commersonii isolate SRR12437578 mitochondrion, complete genome	Cephalorhynchus commersonii	195	195	90%	2e-45	98.20%	16374	NC_060610.1
                      
#>3eeab7673e92d88a4db910bf24ba0b18
#CATAAACTATTCCTTGAAAAAAGCTTATTGTATAATTACCACAACCCCGCAGTGCCACGTCAGTATTAAAAGTAATTTATTTTAAAAACATTTTACTGTACACATTACATATACACATACAC
# Select seq NC_060610.1	Cephalorhynchus commersonii isolate SRR12437578 mitochondrion, complete genome	Cephalorhynchus commersonii	195	195	90%	2e-45	98.20%	16374	NC_060610.1

#>b3b59906612af3bd9bbb17ab1f6e3eff
#CATAAACTATTCCTTGAAAAAAGCTTATTGTACAATTACCACAACCCCACAGTGCCACGTCAGTATTAAAAGTAATTTATTTTAAAAACATTTTACTGTACACATTACATATACACATACACA
# Select seq NC_060610.1	Cephalorhynchus commersonii isolate SRR12437578 mitochondrion, complete genome	Cephalorhynchus commersonii	206	206	90%	1e-48	100.00%	16374	NC_060610.1
                      
#>d3b6c31a9253146da682e1562425081c
#CATAAACTATTCCTTGAAAAAAGCTTATTGTGTAATTACCACAACCCCACAGTGCCACGTCAGTATTAAAAGTAATTTATTTTAAAAACATTTTACTGTACACATTACATATACACATACAC
# Select seq NC_060610.1	Cephalorhynchus commersonii isolate SRR12437578 mitochondrion, complete genome	Cephalorhynchus commersonii	195	195	90%	2e-45	98.20%	16374	NC_060610.1
                      
#>0f8f54b8998d0491a2d8bbddd3cc8593
#CATAAACTATTCCTTGAAAAAAGCTTATTGTATAATTACCACAACCCCACAGTGCCACGTCAGTATTAAAAGTAATTTATTTTAAAAACATTTTACTGTACACATTAGATATACACATACAC
# Select seq NC_060610.1	Cephalorhynchus commersonii isolate SRR12437578 mitochondrion, complete genome	Cephalorhynchus commersonii	195	195	90%	2e-45	98.20%	16374	NC_060610.1
                      
#>1802c9d7430c86b30ea549cf6db935a5
#TATAAACTATTCCTTGAAAAAAGCTTATTGTACAATTACCACAACCCCACAGTGCCACGTCAGTATTAAAAGTAATTTATTTTAAAAACATTTTACTGTACACATTACATATACACATACAC
# Select seq NC_060610.1	Cephalorhynchus commersonii isolate SRR12437578 mitochondrion, complete genome	Cephalorhynchus commersonii	204	204	90%	3e-48	100.00%	16374	NC_060610.1
                      
#>096d8c95e394208836d5d1644d94436a
#ACTAAACTATTCCCTGCAACCAAAACAAGCATTCCATTCGTATGCAAACCAAAACGCCAAGTACTTAATTACTATCTTTAAAACAAAAAAACCCATAAAAATTGCGCACAAACATACAAATA
# Select seq MK801664.1	Sus scrofa control region, partial sequence; mitochondrial	Sus scrofa	226	226	100%	7e-55	100.00%	713	MK801664.1
# PIG
                      
#>a6b5c8e92f36ebf590a9be762f5f7734
#CCTAAACTATTCCCTGCAACCAAAACAAGCATTCCATTCGTATGCAAACCAAAACGCCAAGTGCTTAATTACTATCTTTAAAACAAAAAAACCCATAAAAATTGCGCACAAACATACAAATA
# Select seq MK801664.1	Sus scrofa control region, partial sequence; mitochondrial	Sus scrofa	219	219	99%	1e-52	99.17%	713	MK801664.1
# PIG
                      
#>a92428e781bce1d985ffda1e0b33420f
#CTTAAACTATTCCCTGAAATTATGACATTTACAAAATTATTAAGTACTGTATCAGTATTAAAACAGTCCATGTTATCACAAACTACTATGTACATCATTCATACTTGTACTACCACATTAAT
# NO SIG SIMILARITY
                      
#>b384b5ab4f1e192b560b931ab1f7b131
#TTCAAAATTATATATCAAAACTCGTTTTTCCCCCAACATTGTGTGTGCGCTCCATACGAACACTGTTCCAGAGATGTCCCGTGAAACCAGCAACCCGTTCACCTCAGATCGGAAGAGCGGTT
# NO SIG SIMILARITY
                      
#>4e23ccc8fe9ff89db8896f0f0a8bb7e2
#TTTAAACTATTCCCTGATGCTTATTAATATAGTTCCACAAAAATCAAGAACTTTATCAGTATTAAATTTCCAAAAAGTTTTAATATTTCAATACAGCTTTCCACTCAACACCCATTTTACAT
# Select seq KF153096.1	Cervus elaphus isolate ELA4 D-loop, partial sequence; mitochondrial	Cervus elaphus	226	226	100%	7e-55	100.00%	1133	KF153096.1
# DEER
                      
#>98c6ff9abcb46484595276c5d6a3672f
#ACGACGTCCTTGCGATGAACGCGTTATTTCAGCTCAACCGCTTCTTCTTCAGAGAATACCTATTTCCATTGTATCCTATGGTCACTCCATTAGATCACGAGCTTAATCACCATGCCGCGTGA
# Select seq MF034189.1	Cephalorhynchus hectori hectori isolate Che11CB053 D-loop, partial sequence; mitochondrial	Cephalorhynchus hectori hectori	115	115	50%	2e-21	100.00%	576	MF034189.1
# PCR CHIMERA PERHAPS - NOT A LOT OF COVERAGE - DID NOT ALIGN WELL SO DROPPED
                      
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
