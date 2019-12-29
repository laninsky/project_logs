# This code corresponds to Fig SAA in Alexander et al.
# It summarizes genic and chromosomal regions that are graphed in
# using Fig SBB.R in the supplementary materials of Alexander et al.

# R code to summarize RADseq markers based on ref_guided.snps.map
# (in order to find which SNP positions correspond to each locus),
# headerless.vcf (to obtain chromosome, and position along chromosome),
# chromosome_scaffolds.txt (to find which scaffold corresponds to what
# chromosome), and headerless.gff (to see whether SNPs fell in genic regions)

# 1. Loading required library
library(tidyverse)

# 2. Loading in data files and retaining columns of interest
# Reading in ref_guided.snps.map and retaining columns of interest
snp_locus_map <- read_tsv("ref_guided.snps.map",col_names = FALSE)
snp_locus_map <- snp_locus_map %>% mutate(filepos=row_number(),locus=gsub("_.*","",X2), snp=gsub(".*_","",X2)) %>% select(filepos,locus,snp)

# Reading in headerless.vcf and retaining columns of interest
vcf <- read_tsv("headerless.vcf",col_names = FALSE)
vcf <- vcf %>% mutate(filepos=row_number(),scaffold=X1,bp_pos=X2) %>% select(filepos,scaffold,bp_pos)

# Combining vcf and snp_locus_map
snp_locus_map <- full_join(snp_locus_map,vcf,by="filepos")
rm(vcf)
snp_locus_map
## A tibble: 63,889 x 5
#filepos locus snp   scaffold    bp_pos
#<int> <chr> <chr> <chr>        <dbl>
#  1       1 rad1  snp0  NC_031768.1  21921
#2       2 rad1  snp1  NC_031768.1  21923
#3       3 rad1  snp2  NC_031768.1  21925
#4       4 rad1  snp3  NC_031768.1  21978
#5       5 rad1  snp4  NC_031768.1  22016
#6       6 rad1  snp5  NC_031768.1  22017
#7       7 rad1  snp6  NC_031768.1  22076
#8       8 rad1  snp7  NC_031768.1  22089
#9       9 rad1  snp8  NC_031768.1  22222
#10      10 rad1  snp9  NC_031768.1  22231

# Reading in chromosome_scaffolds.txt and creating a variable, chromosome,
# which gives the chromosome number for each scaffold
chrom_scaff <- read_tsv("chromosome_scaffolds.txt",col_names = FALSE)
chromosome <- snp_locus_map$scaffold
for (i in 1:dim(chrom_scaff)[1]) {
  replace_items <- which(chromosome %in% as.matrix(chrom_scaff[i,1]))
  chromosome[replace_items] <- as.matrix(chrom_scaff[i,2])
}

chromosome <- cbind(chromosome,seq(1,dim(snp_locus_map)[1]))
chromosome <- as_tibble(chromosome)
names(chromosome) <- c("chromosome","filepos")
chromosome$filepos <- as.numeric(chromosome$filepos)

# Combining chromosome and snp_locus_map
snp_locus_map <- full_join(snp_locus_map,chromosome,by="filepos")
rm(chromosome)
snp_locus_map 
## A tibble: 63,889 x 6
#filepos locus snp   scaffold    bp_pos chromosome
#<dbl> <chr> <chr> <chr>        <dbl> <chr>     
#  1       1 rad1  snp0  NC_031768.1  21921 1         
#2       2 rad1  snp1  NC_031768.1  21923 1         
#3       3 rad1  snp2  NC_031768.1  21925 1         
#4       4 rad1  snp3  NC_031768.1  21978 1         
#5       5 rad1  snp4  NC_031768.1  22016 1         
#6       6 rad1  snp5  NC_031768.1  22017 1         
#7       7 rad1  snp6  NC_031768.1  22076 1         
#8       8 rad1  snp7  NC_031768.1  22089 1         
#9       9 rad1  snp8  NC_031768.1  22222 1         
#10      10 rad1  snp9  NC_031768.1  22231 1         
## … with 63,879 more rows

# Reading in the gff file
gff <- read_tsv("headerless.gff",col_names = FALSE)
gff <- gff %>% filter(X2!="RefSeq" & X3!="region") %>% mutate(scaffold=X1,region=X3,startpos=X4,endpos=X5) %>% select(scaffold,region,startpos,endpos)

# Obtaining the specific regions that SNPs could be in
gff %>% select(region) %>% unique()
## A tibble: 10 x 1
#region        
#<chr>         
#  1 gene          
#2 mRNA          
#3 exon          
#4 CDS           
#5 lnc_RNA       
#6 transcript    
#7 tRNA          
#8 C_gene_segment
#9 V_gene_segment
#10 pseudogene  

# 3. Summarizing region information for each SNP
gene_info <- matrix(NA,ncol=11,nrow=dim(snp_locus_map)[1])
gene_info[,1] <- seq(1,dim(snp_locus_map)[1])
names_gene_info <- c("filepos","gene", "mRNA", "exon", "CDS", "lnc_RNA", "transcript", "tRNA", "C_gene_segment", "V_gene_segment", "pseudogene")

for (i in 1:dim(snp_locus_map)[1]) {
  print(paste("Up to ",i, " out of ",dim(snp_locus_map)[1]," loci",sep=""))
  temp_regions <- gff %>% filter(scaffold==snp_locus_map$scaffold[i]) %>% filter(startpos <= snp_locus_map$bp_pos[i] & endpos >= snp_locus_map$bp_pos[i]) %>% select(region) %>% as.matrix()
  gene_info[i,which(names_gene_info %in% temp_regions)] <- "Yes"
}

gene_info <- as_tibble(gene_info)
names(gene_info) <- names_gene_info
gene_info$filepos <- as.numeric(gene_info$filepos)

snp_locus_map <- full_join(snp_locus_map,gene_info,by="filepos")

# 4. Extracting eulerr-compatible columns to summarize data
snp_locus_map <- snp_locus_map %>% mutate(gene_mRNA_exon_CDS=paste(ifelse(!is.na(gene),"gene&",""),ifelse(!is.na(mRNA),"mRNA&",""),ifelse(!is.na(exon),"exon&",""),ifelse(!is.na(CDS),"CDS&",""),sep="")) %>% mutate(gene_mRNA_exon_CDS=gsub("&$","",gene_mRNA_exon_CDS))

snp_locus_map <- snp_locus_map %>% mutate(add_break_down=paste(ifelse(!is.na(lnc_RNA),"lnc_RNA&",""),ifelse(!is.na(transcript),"transcript&",""),ifelse(!is.na(tRNA),"tRNA&",""),ifelse(!is.na(C_gene_segment),"C_gene_segment&",""),ifelse(!is.na(pseudogene),"pseudogene&",""),sep="")) %>% mutate(add_break_down=gsub("&$","",add_break_down))

write_delim(snp_locus_map,"snp_locus_gene_function.txt")

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
#[25] tidyselect_0.2.5 glue_1.3.1       R6_2.4.0         fansi_0.4.0     
#[29] readxl_1.3.1     modelr_0.1.5     magrittr_1.5     backports_1.1.4 
#[33] scales_1.0.0     rvest_0.3.4      assertthat_0.2.1 colorspace_1.4-0
#[37] utf8_1.1.4       stringi_1.3.1    lazyeval_0.2.1   munsell_0.5.0   
#[41] broom_0.5.2      crayon_1.3.4   