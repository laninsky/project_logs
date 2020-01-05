# This code corresponds to Fig SCC in Alexander et al.
# It generates the input files for the bgc analysis represented in SZ.sh

# Necessary files: 
# S2.txt (https://github.com/laninsky/project_logs/blob/master/chickadee/data/S2.txt): will use 
# this file to divide up birds into "parental" and "admixed"
# snp_locus_gene_function.txt (created in Step 1 above): gives genomic coordinates of SNPs to account for LD
# ref_guided.ustr: genotypes per locus and per individual used to create data files

# 1. Loading required library
library(tidyverse)

# 2. Loading in data files and retaining columns of interest
# Reading in "snp_locus_gene_function.txt" and retaining columns of interest
# have to define the column type or chromosome is read in as numeric
snp_locus_map <- read_delim("snp_locus_gene_function.txt", delim=" ",
                   col_types = list(col_double(),
                                    col_character(),
                                    col_character(),
                                    col_character(),
                                    col_double(),
                                    col_character(),
                                    col_character(),
                                    col_character(),
                                    col_character(),
                                    col_character(),
                                    col_character(),
                                    col_character(),
                                    col_character(),
                                    col_character(),
                                    col_character(),
                                    col_character(),
                                    col_character(),
                                    col_character()))

# Reading in S2.txt to get genomic assignments for each bird
structure_assignments <- read_tsv("S2.txt")

# Reading in the SNPs for the birds
SNPs <- read_tsv("ref_guided.str",col_names = FALSE)
cols_to_delete <- NULL
for (i in 1:dim(SNPs)[2]) {
  if (all(is.na(SNPs[,i]))) {
    cols_to_delete <- c(cols_to_delete,i)
  }
}
SNPs <- SNPs[,-cols_to_delete]

# Some minor tweaking of sample names based on tissue number in SNPs vs catalog number in S2
SNPs[which(grepl("3474",as.matrix(SNPs[,1]))),1] <- "90612"
SNPs[which(grepl("6281",as.matrix(SNPs[,1]))),1] <- "95776"
SNPs[which(grepl("9898",as.matrix(SNPs[,1]))),1] <- "131638"
SNPs[which(grepl("7420",as.matrix(SNPs[,1]))),1] <- "92269"
SNPs[which(grepl("7421",as.matrix(SNPs[,1]))),1] <- "92270"

SNPs <- SNPs %>% mutate(X1=gsub("_.*","",X1))

# 3. Splitting the SNPs file into black-capped, Carolina, and admixed birds
black_capped_names <- structure_assignments %>% filter(BC_genetic_cluster_assignment>=0.95) %>% 
  select(Catalog_number) %>% as.matrix()
black_capped_names <- rbind(black_capped_names,(structure_assignments %>% filter(BC_genetic_cluster_assignment>=0.95) %>% 
  select(Tissue_number) %>% as.matrix()))

Carolina_names <- structure_assignments %>% filter(BC_genetic_cluster_assignment<=0.05) %>% 
  select(Catalog_number) %>% as.matrix()
Carolina_names <- rbind(Carolina_names,(structure_assignments %>% 
                                          filter(BC_genetic_cluster_assignment<=0.05) %>% 
                                                  select(Tissue_number) %>% as.matrix()))

admixed_names <- structure_assignments %>% 
  filter(BC_genetic_cluster_assignment>0.05 & BC_genetic_cluster_assignment<0.95) %>% 
  select(Catalog_number) %>% as.matrix()
admixed_names <- rbind(admixed_names,(structure_assignments %>% 
                                          filter(BC_genetic_cluster_assignment>0.05 & BC_genetic_cluster_assignment<0.95) %>% select(Tissue_number) %>% as.matrix()))

black_capped_SNPs <- SNPs %>% filter(X1 %in% black_capped_names)
Carolina_SNPs <- SNPs %>% filter(X1 %in% Carolina_names)
admixed_SNPs <- SNPs %>% filter(X1 %in% admixed_names)

# 4. Setting up each of the SNP input files
black_capped <- matrix(NA,ncol=1,nrow=((dim(SNPs)[2]-1)*2))

black_capped[seq(1,dim(black_capped)[1],2),1] <- paste("locus_",(seq(0,(dim(SNPs)[2]-2),1)),sep="")

Carolina <- black_capped

# admixed
# Different format to above parental populations e.g. individual level
# information, and 'pop 0' header
admixed <- matrix(NA,ncol=1,nrow=((dim(admixed_SNPs)[2]-1)*((dim(admixed_SNPs)[1]/2)+2)))
admixed[seq(1,dim(admixed)[1],((dim(admixed_SNPs)[1]/2)+2)),1] <- paste("locus_",(seq(0,(dim(admixed_SNPs)[2]-2),1)),sep="")
admixed[seq(2,dim(admixed)[1],((dim(admixed_SNPs)[1]/2)+2)),1] <- "pop_0"

for (i in 2:dim(SNPs)[2]) {
  print(paste("Up to ",i-1," SNPs out of ",dim(SNPs)[2]-1,sep=""))
  biallelic <- c(NA,NA)
  
  # Getting a count of individuals for alleles for the i-1 SNP
  # For BC
  BCalleles <- black_capped_SNPs[1:dim(black_capped_SNPs)[1],i]
  names(BCalleles) <- "allele_names"
  BCalleles <- BCalleles %>% group_by(allele_names) %>% 
    summarize(n=n()) %>% filter(allele_names!=-9) %>% 
    mutate(species="BC")

  # For CC
  CCalleles <- Carolina_SNPs[1:dim(Carolina_SNPs)[1],i]
  names(CCalleles) <- "allele_names"
  CCalleles <- CCalleles %>% group_by(allele_names) %>% 
    summarize(n=n()) %>% filter(allele_names!=-9) %>% 
    mutate(species="CC")
  
  # Looking across both species to see what alleles are present
  alleles <- rbind(BCalleles,CCalleles)
  alleles_across_spp <- alleles %>% group_by(allele_names) %>% 
    summarise(n2=n())
  
  # Creating a variable to hold those alleles in
  finalBCalleles <- c(0,0)
  finalCCalleles <- c(0,0)

  # Pulling in the admixed data for this locus
  first_allele <- as.matrix(admixed_SNPs[seq(1,dim(admixed_SNPs)[1]-1,2),i])
  second_allele <- as.matrix(admixed_SNPs[seq(2,dim(admixed_SNPs)[1],2),i])

  # If data for this locus is present in the BC and CC
  if (dim(alleles_across_spp)[1]>0) {

    # If there are more than 2 allelic states, taking the 2 alleles that
    # are found in both parental species and ignoring the singleton
    if(dim(alleles_across_spp)[1]>2) {
      present_in_both <- alleles_across_spp$allele_names[which(alleles_across_spp$n2==max(alleles_across_spp$n2))]
      biallelic <- present_in_both
      
      if (length(biallelic)>2) {
        # For loci with more than two alleles
        while(dim(alleles)[1]>2) {
          minallele <- min(alleles$n)
          if(minallele==max(alleles$n)) {
            alleles <- alleles[-(sample(1:dim(alleles)[1],1)),]
            biallelic <- alleles$allele_names
          } else {
            alleles <- alleles %>% filter(n!=minallele)
            biallelic <- alleles$allele_names
          }
        }
      }
      
      # Replacing any admixed bird alleles not present_in_both with -9
      first_allele[which(!(first_allele %in% biallelic))] <- -9
      second_allele[which(!(second_allele %in% biallelic))] <- -9
      
    } else {
      present_in_both <- alleles_across_spp$allele_names
      biallelic <- present_in_both
    }
  
    for (j in 1:length(biallelic)) {
      finalBCalleles[j] <- sum(black_capped_SNPs[1:dim(black_capped_SNPs)[1],i]==biallelic[j])
      finalCCalleles[j] <- sum(Carolina_SNPs[1:dim(Carolina_SNPs)[1],i]==biallelic[j])
    }
    
  } else {
    # Do we need to record this locus to remove it, because if there
    # is no data for the parentals, it will not be informative?
    # Will try running bgc as is and see if this causes errors first
  }
  # Write out allelic states to their final files for parentals
  Carolina[(i-1)*2,1] <- paste(finalCCalleles,collapse=" ")
  black_capped[(i-1)*2,1] <- paste(finalBCalleles,collapse=" ")
  
  # Writing out individual level data for admixed birds
  pasted_alleles <- paste(first_allele,second_allele)

  # Creating values for biallelic if no parental data recorded
  # Note - if this causes errors for bgc, will be removing these loci
  if (is.na(biallelic[1])) {
    admixedalleles <- table(c(first_allele[which(first_allele!=-9)],second_allele[which(second_allele!=-9)]))
    # For loci with more than two alleles
    while(length(admixedalleles)>2) {
      toremove <- names(admixedalleles)[which(admixedalleles==min(admixedalleles))]
      first_allele[which(first_allele %in% toremove)] <- -9
      second_allele[which(second_allele %in% toremove)] <- -9
      pasted_alleles <- paste(first_allele,second_allele)
      admixedalleles <- table(c(first_allele[which(first_allele!=-9)],second_allele[which(second_allele!=-9)]))
    }
    biallelic <- names(admixedalleles)
  }
  
  # Getting the admixed birds genotypes together
  for (k in 1:length(first_allele)) {
    # For loci where only one allele present
    if (length(biallelic)==1) {
      if (first_allele[k]!=-9) {
        pasted_alleles[k] <- "2 0"
      }
    } else {
      # What to do if two alleles present
      
      if (first_allele[k]==biallelic[1] & second_allele[k]==biallelic[1]) {
        pasted_alleles[k] <- "2 0"
      } else {
        if (first_allele[k]==biallelic[2] & second_allele[k]==biallelic[2]) {
          pasted_alleles[k] <- "0 2"
        } else {
          if (first_allele[k]!=-9) {
            pasted_alleles[k] <- "1 1"
          }
        }
      }
    }  
  }
  end_pos <- ((dim(admixed_SNPs)[1]/2+2)*(i-1))
  start_pos <- end_pos-(dim(admixed_SNPs)[1]/2-1)
  admixed[start_pos:end_pos,1] <- pasted_alleles
} 

# writing these files out
write.table(black_capped,"black_capped.txt",quote = FALSE,col.names = FALSE,row.names = FALSE)
write.table(Carolina,"Carolina.txt",quote = FALSE,col.names = FALSE,row.names = FALSE)
write.table(admixed,"admixed.txt",quote = FALSE,col.names = FALSE,row.names = FALSE)

# 5. Setting up the map file
# Looks like it needs to number each marker, for each chromosome starting at 0
# then give chromosome as a number, and then distance in kb across chromosome
# for each of the markers
genetic_map <- matrix(NA,ncol=3,nrow=dim(snp_locus_map[1]))
genetic_map[1,] <- c(0,1,snp_locus_map$bp_pos[1]/1000)

for (i in 2:dim(snp_locus_map)[1]) {
  if(snp_locus_map$chromosome[i]=="Unknown") {
    if(snp_locus_map$scaffold[i-1]!=snp_locus_map$scaffold[i]) {
      genetic_map[i,1] <- 0
      genetic_map[i,2] <- genetic_map[(i-1),2]+1
      genetic_map[i,3] <- snp_locus_map$bp_pos[i]/1000
    } else {
      genetic_map[i,1] <- genetic_map[(i-1),1]+1
      genetic_map[i,2] <- genetic_map[(i-1),2]
      genetic_map[i,3] <- snp_locus_map$bp_pos[i]/1000
    }
  } else {
    if(snp_locus_map$chromosome[i-1]!=snp_locus_map$chromosome[i]) {
      genetic_map[i,1] <- 0
      genetic_map[i,2] <- genetic_map[(i-1),2]+1
      genetic_map[i,3] <- snp_locus_map$bp_pos[i]/1000
    } else {
      genetic_map[i,1] <- genetic_map[(i-1),1]+1
      genetic_map[i,2] <- genetic_map[(i-1),2]
      genetic_map[i,3] <- snp_locus_map$bp_pos[i]/1000
    }
  }
}

write.table(genetic_map,"genetic_map.txt",quote = FALSE,col.names = FALSE,row.names = FALSE)

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
