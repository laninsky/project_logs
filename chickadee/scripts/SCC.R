# This code corresponds to Fig SCC in Alexander et al.
# It generates the input files for the bgc analysis represented in SZ.sh and a vcf
# file (without the header) that can be used in SnpEff

# Necessary files 
# headerless.vcf: to obtain chromosome, and position along chromosome
# ref_guided.snps.map: in order to find which SNP positions correspond to each locus
# chromosome_scaffolds.txt: to find which scaffold corresponds to what chromosome
# popmap.txt: will use this file (and S2.txt) to divide up birds into "blackcapped, "Carolina", and "admixed"
# The file "popmap.txt" was created by running the following code in the ipyrad directory
#  ls fastqs/*R1* | sed 's|fastqs/||g' | sed 's/_R1.*//g' > popmap.txt
# S2.txt: we will use this file (and popmap.txt) to divide the birds into "blackcapped, "Carolina", and "admixed"

# 1. Setting parameters
# required structure assignment to BC to be considered "pure BC"
structure_thres <- 0.99
# required completeness of loci to be included
complete_thres <- 0.75
# Header rows of vcf says vcf nucleotide order is CATG
nucleotide_order <- c("C","A","T","G")

# 2. Loading required library
library(tidyverse)

# 3. Creating a bgc-specific popmap file using the structure threshold above
# This file will be used to filter the vcf file
# Reading in popmap.txt
temppopmap <- read_tsv("popmap.txt",col_names=FALSE)
# Reading in S2 to get structure assignments of each bird
tempstructure <- read_tsv("S2.txt",col_names=TRUE)
tempstructure <- tempstructure %>% 
  select(Catalog_number,Tissue_number,BC_genetic_cluster_assignment) %>% 
  arrange(BC_genetic_cluster_assignment)

# Creating an object to store the names of the individuals as they are represented
# in the sequencing file in the same order as S2.txt
tempnames <- rep(NA,dim(tempstructure)[1])

# replacing names which used tissue number instead of catalog number
tempstructure[(grepl("90612",as.matrix(tempstructure[,1]))),1] <- "3474_"
tempstructure[(grepl("95776",as.matrix(tempstructure[,1]))),1] <- "6281_"
tempstructure[(grepl("131638",as.matrix(tempstructure[,1]))),1] <- "9898_"
tempstructure[(grepl("92269",as.matrix(tempstructure[,1]))),1] <- "7420_"
tempstructure[(grepl("92270",as.matrix(tempstructure[,1]))),1] <- "7421_"

# Getting names as they appear in the read file for each of the birds
for (i in 1:dim(temppopmap)[1]) {
  temp <- as.matrix(temppopmap[grep(as.matrix(tempstructure[i,1]),as.matrix(temppopmap)),1])
  if (length(temp)>0) {
    tempnames[i] <- temp
  } else {
    tempnames[i] <- as.matrix(temppopmap[grep(as.matrix(tempstructure[i,2]),as.matrix(temppopmap)),1])
  }  
}

tempnames <- as_tibble(tempnames)
names(tempnames) <- "seqnames"
tempstructure <- bind_cols(tempnames,tempstructure)
# Removing any NA-only lines
tempstructure <- tempstructure %>% filter(!is.na(seqnames))

# Carrying out filtering based on structure_thres
tempstructure <- tempstructure %>% 
  mutate(popname=ifelse(BC_genetic_cluster_assignment<=(1-structure_thres),"Carolina",
                        ifelse(BC_genetic_cluster_assignment>=structure_thres,"blackcapped","hybrid")))

# Also writing out this file for post bgc analyses
tempstructure <- tempstructure %>% select(seqnames,popname,BC_genetic_cluster_assignment)
write_delim(tempstructure,"popmap_w_structure.txt",col_names = FALSE)

# Loading in the vcf file so it can be filtered
vcf <- read_tsv("headerless.vcf",col_names = TRUE)

# 3. Filtering the vcf file to reduce the number of SNPs
# Removing SNPs where all black_capped or all Carolina 
# or all admixed populations had missing data
rows_to_delete <- NULL

for (i in 1:dim(vcf)[1]) {
  print(paste("Up to ",i," out of ",dim(vcf)[1], " SNPs",sep=""))
  if (all(vcf[i,(which(names(vcf) %in% as.matrix(tempstructure[(which(tempstructure[,2]=="blackcapped")),1])))]=="./.:0:0,0,0,0") |
  all(vcf[i,(which(names(vcf) %in% as.matrix(tempstructure[(which(tempstructure[,2]=="Carolina")),1])))]=="./.:0:0,0,0,0") |
  all(vcf[i,(which(names(vcf) %in% as.matrix(tempstructure[(which(tempstructure[,2]=="hybrid")),1])))]=="./.:0:0,0,0,0")) {
    rows_to_delete <- c(rows_to_delete,i)
  }
}

vcf <- vcf[-rows_to_delete,]

# Removing non-biallelic SNPs
rows_to_delete <- grep(",",vcf$ALT)
vcf <- vcf[-rows_to_delete,]

# Only including SNPs that have the minimum amount of missing data per locus
missingdata <- rep(NA,dim(vcf)[1])
for (i in 1:dim(vcf)[1]) {
  print(paste("Up to ",i," out of ",dim(vcf)[1], " SNPs",sep=""))
  missingdata[i] <- length(grep("./.:0:0,0,0,0",vcf[i,10:dim(vcf)[2]]))
}

missingdata <- as_tibble(missingdata)
names(missingdata) <- "missing_data"

locus <- vcf %>% select(ID) %>% mutate(locus=gsub("_.*","",ID)) %>% select(locus)

vcf <- bind_cols(missingdata,locus,vcf)

vcf <- vcf %>% group_by(locus) %>% filter(missing_data==min(missing_data))

# Only including SNPs that have data for more than complete_thres of the total 
# individuals due to computational constraints running bgc
no_indivs <- dim(vcf)[2]-12+1
vcf <- vcf %>% filter(missing_data<=(no_indivs*(1-complete_thres)))

# Taking the SNP with the largest MAF
maf <- rep(NA,dim(vcf)[1])
for (i in 1:dim(vcf)[1]) {
  print(paste("Up to ",i," out of ",dim(vcf)[1], " SNPs",sep=""))
  temp <- gsub(":.*","",vcf[i,12:dim(vcf)[2]])
  temp <- temp[which(temp!="./.")]
  count0 <- length(which(temp=="0/0"))*2 + length(which(temp=="0/1"))
  count1 <- length(which(temp=="1/1"))*2 + length(which(temp=="0/1")) 
  maf[i] <- min(count0,count1)/(count0+count1)
}

maf <- as_tibble(maf)
names(maf) <- "maf"
vcf <- bind_cols(maf,vcf)

vcf <- vcf %>% group_by(locus) %>% filter(maf==max(maf))

# Taking the first of the remaining SNPs per locus
vcf <- vcf %>% group_by(locus) %>% slice(.,1)

# Reading in chromosome_scaffolds.txt and creating a variable, chromosome,
# which gives a numerical number for each scaffold (required for bgc)
chrom_scaff <- read_tsv("chromosome_scaffolds.txt",col_names = FALSE)

# The grep pattern on the gff file failed to pull out the mtDNA scaffold.
# Here we are finding the "missing" scaffold number from the vcf file that
# corresponds to the mtDNA, so we can add it to chrom_scaff
temprow <- c(as.matrix(unique(vcf$`#CHROM`))[which(!(as.matrix(unique(vcf$`#CHROM`)) %in% as.matrix(chrom_scaff$X1)))],"mtDNA")
names(temprow) <- names(chrom_scaff)
chrom_scaff <- bind_rows(chrom_scaff,temprow)

bgc_chrom <- seq(1,(dim(chrom_scaff)[1]))
bgc_chrom <- as_tibble(bgc_chrom)
names(bgc_chrom) <- "bgc_chrom"
chrom_scaff <- bind_cols(chrom_scaff,bgc_chrom)
names(chrom_scaff) <- c("scaffold","chrom","bgc_chrom")

write_tsv(chrom_scaff,"bgc_chrom_key.txt")

chromosome <- as.matrix(vcf$`#CHROM`)
for (i in 1:dim(chrom_scaff)[1]) {
  replace_items <- which(chromosome %in% as.matrix(chrom_scaff[i,1]))
  chromosome[replace_items] <- as.matrix(chrom_scaff[i,3])
}

chromosome <- as_tibble(chromosome)
names(chromosome) <- "bgc_chrom"
chromosome$bgc_chrom <- as.numeric(chromosome$bgc_chrom)

vcf <- bind_cols(chromosome,vcf)

# 4. Setting up each of the SNP input files for bgc
vcf <- vcf %>% arrange(bgc_chrom,POS)

no_blackcapped <- tempstructure %>% filter(popname=="blackcapped") %>% count() %>% as.numeric()
no_Carolina <- tempstructure %>% filter(popname=="Carolina") %>% count() %>% as.numeric()
no_hybrid <- tempstructure %>% filter(popname=="hybrid") %>% count() %>% as.numeric()

bgc_blackcapped <- matrix(NA,ncol=1,nrow=(no_blackcapped+1)*dim(vcf)[1])
bgc_Carolina <- matrix(NA,ncol=1,nrow=(no_Carolina+1)*dim(vcf)[1])
bgc_hybrid <- matrix(NA,ncol=1,nrow=(no_hybrid+2)*dim(vcf)[1])

for (i in 1:dim(vcf)[1]) {
  # read counts for which nucleotides should be extracted
  bases <- sort(c(which(nucleotide_order %in% as.matrix(vcf$REF[i])),which(nucleotide_order %in% as.matrix(vcf$ALT[i]))))
  
  # Geting bgc_blackcapped together
  tempblackcapped <- vcf[i,(which(names(vcf) %in% as.matrix(tempstructure[(which(tempstructure[,2]=="blackcapped")),1])))]
  tempblackcapped <- gsub(".*:","",tempblackcapped)
  # read counts positions that should be selected out of the total
  snps_to_select <- sort(c(seq(bases[1],no_blackcapped*4,4),seq(bases[2],no_blackcapped*4,4)))
  # read counts across the birds at all SNP positions
  tempblackcapped <- unlist(strsplit(tempblackcapped,","))[snps_to_select]
  # getting position in bgc_blackcapped that this locus corresponds to
  startpos <- (((i-1)*(1+no_blackcapped))+1)
  # locus name
  bgc_blackcapped[startpos,1] <- paste("locus_",i,sep="") 
  # collapsing read counts into individual read counts
  blackcapped_indivs <- matrix(NA,ncol=1,nrow=no_blackcapped)
  for (j in 1:dim(blackcapped_indivs)[1]) {
    blackcapped_indivs[j,1] <- paste(tempblackcapped[(j*2-1):(j*2)],collapse=" ")
  }
  # saving these read counts to the outfile
  bgc_blackcapped[(startpos+1):(startpos+no_blackcapped),1] <- blackcapped_indivs

  # Geting bgc_Carolina together
  tempCarolina <- vcf[i,(which(names(vcf) %in% as.matrix(tempstructure[(which(tempstructure[,2]=="Carolina")),1])))]
  tempCarolina <- gsub(".*:","",tempCarolina)
  # read counts positions that should be selected out of the total
  snps_to_select <- sort(c(seq(bases[1],no_Carolina*4,4),seq(bases[2],no_Carolina*4,4)))
  # read counts across the birds at all SNP positions
  tempCarolina <- unlist(strsplit(tempCarolina,","))[snps_to_select]
  # getting position in bgc_blackcapped that this locus corresponds to
  startpos <- (((i-1)*(1+no_Carolina))+1)
  # locus name
  bgc_Carolina[startpos,1] <- paste("locus_",i,sep="") 
  # collapsing read counts into individual read counts
  Carolina_indivs <- matrix(NA,ncol=1,nrow=no_Carolina)
  for (j in 1:dim(Carolina_indivs)[1]) {
    Carolina_indivs[j,1] <- paste(tempCarolina[(j*2-1):(j*2)],collapse=" ")
  }
  # saving these read counts to the outfile
  bgc_Carolina[(startpos+1):(startpos+no_Carolina),1] <- Carolina_indivs

  # Geting bgc_hybrid together
  temphybrid <- vcf[i,(which(names(vcf) %in% as.matrix(tempstructure[(which(tempstructure[,2]=="hybrid")),1])))]
  temphybrid <- gsub(".*:","",temphybrid)
  # read counts positions that should be selected out of the total
  snps_to_select <- sort(c(seq(bases[1],no_hybrid*4,4),seq(bases[2],no_hybrid*4,4)))
  # read counts across the birds at all SNP positions
  temphybrid <- unlist(strsplit(temphybrid,","))[snps_to_select]
  # getting position in bgc_blackcapped that this locus corresponds to
  startpos <- (((i-1)*(2+no_hybrid))+1)
  # locus name
  bgc_hybrid[startpos,1] <- paste("locus_",i,sep="")
  bgc_hybrid[(startpos+1),1] <- "pop_0"
  # collapsing read counts into individual read counts
  hybrid_indivs <- matrix(NA,ncol=1,nrow=no_hybrid)
  for (j in 1:dim(hybrid_indivs)[1]) {
    hybrid_indivs[j,1] <- paste(temphybrid[(j*2-1):(j*2)],collapse=" ")
  }
  # saving these read counts to the outfile
  bgc_hybrid[(startpos+2):(startpos+no_hybrid+1),1] <- hybrid_indivs
}

# writing these files out
write.table(bgc_blackcapped,"black_capped.txt",quote = FALSE,col.names = FALSE,row.names = FALSE)
write.table(bgc_Carolina,"Carolina.txt",quote = FALSE,col.names = FALSE,row.names = FALSE)
write.table(bgc_hybrid,"admixed.txt",quote = FALSE,col.names = FALSE,row.names = FALSE)

# 5. Setting up the map file
# Looks like it needs to number each marker, for each chromosome starting at 0
# then give chromosome as a number, and then distance in kb across chromosome
# for each of the markers
genetic_map <- vcf %>% group_by(bgc_chrom) %>% 
  mutate(snp_pos=row_number()-1,kbp=POS/1000) %>% select(snp_pos,bgc_chrom,kbp)

write.table(genetic_map,"genetic_map.txt",quote = FALSE,col.names = FALSE,row.names = FALSE)

# 6. Writing out filtered vcf file, for snpeff analysis
vcf_filtered <- vcf %>% select(-bgc_chrom,-maf,-missing_data,-locus)
write.table(vcf_filtered,"filtered_headerless.vcf",quote = FALSE,col.names = TRUE,row.names = FALSE)

sessionInfo()
#R version 3.5.3 (2019-03-11)
#Platform: x86_64-pc-linux-gnu (64-bit)
#Running under: CentOS Linux 7 (Core)

#Matrix products: default
#BLAS/LAPACK: /scale_wlg_persistent/filesets/opt_nesi/CS400_centos7_bdw/imkl/2018.4.274-gimpi-2018b/compilers_and_libraries_2018.5.274/linux/mkl/lib/intel64_lin/libmkl_gf_lp64.so

#locale:
#  [1] LC_CTYPE=en_US.UTF-8       LC_NUMERIC=C              
#[3] LC_TIME=en_US.UTF-8        LC_COLLATE=en_US.UTF-8    
#[5] LC_MONETARY=en_US.UTF-8    LC_MESSAGES=en_US.UTF-8   
#[7] LC_PAPER=en_US.UTF-8       LC_NAME=C                 
#[9] LC_ADDRESS=C               LC_TELEPHONE=C            
#[11] LC_MEASUREMENT=en_US.UTF-8 LC_IDENTIFICATION=C       

#attached base packages:
#  [1] stats     graphics  grDevices utils     datasets  methods   base     

#other attached packages:
#  [1] forcats_0.4.0   stringr_1.4.0   dplyr_0.8.3     purrr_0.3.3    
#[5] readr_1.3.1     tidyr_1.0.0     tibble_2.1.3    ggplot2_3.2.1  
#[9] tidyverse_1.2.1

#loaded via a namespace (and not attached):
#  [1] Rcpp_1.0.1       cellranger_1.1.0 pillar_1.4.2     compiler_3.5.3  
#[5] tools_3.5.3      zeallot_0.1.0    jsonlite_1.6     lubridate_1.7.4 
#[9] lifecycle_0.1.0  gtable_0.2.0     nlme_3.1-137     lattice_0.20-38 
#[13] pkgconfig_2.0.2  rlang_0.4.1      cli_1.1.0        rstudioapi_0.10 
#[17] haven_2.1.1      withr_2.1.2      xml2_1.2.2       httr_1.4.1      
#[21] generics_0.0.2   vctrs_0.2.0      hms_0.5.2        grid_3.5.3      
#[25] tidyselect_0.2.5 glue_1.3.1       R6_2.4.0         readxl_1.3.1    
#[29] modelr_0.1.5     magrittr_1.5     backports_1.1.4  scales_1.0.0    
#[33] rvest_0.3.4      assertthat_0.2.1 colorspace_1.4-0 stringi_1.3.1   
#[37] lazyeval_0.2.1   munsell_0.5.0    broom_0.5.2      crayon_1.3.4  
