# This code corresponds to Fig SBB in Alexander et al.
# It creates Fig. SZ (summarizing genic and chromosomal regions) in the 
# supplementary materials of Alexander et al.

# 1. Loading required library
library(tidyverse)
library(eulerr)

# 2. Setwd
setwd("chickadee/output/")

# 3. Loading in data files and retaining columns of interest
# have to define the column type, or chromosome is read in as numeric
temp <- read_delim("../data/snp_locus_gene_function.txt", delim=" ",
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

# 4. Summarizing genic positions (for each individual SNP, incuding multiple SNPs per locus)
temp %>% group_by(gene_mRNA_exon_CDS) %>% summarise(n())
#1 exon                  19
#2 gene                1545
#3 gene&exon            378
#4 gene&mRNA          27303
#5 gene&mRNA&exon      2657
#6 gene&mRNA&exon&CDS  4634
#7 NA                 27353

eulerr_options(pointsize=22)

plot(venn(c("exon"=19,"gene"=1545,"gene&exon"=378,"gene&mRNA"=27303,"gene&mRNA&exon"=2657,"gene&mRNA&exon&CDS"=4634),quantities=list(type="counts",cex=2)),fills=c("green3","dodgerblue3","yellow","red"),cex=4)

# Saved plot manually as 500 pixels wide
# gene_mRNA_exon_CDS.png

# Further breakdown in categories
temp %>% filter(gene_mRNA_exon_CDS=="exon") %>% group_by(add_break_down) %>% summarise(n())
# All 'exon only' entries are "pseudogene"

temp %>% filter(gene_mRNA_exon_CDS=="gene") %>% group_by(add_break_down) %>% summarise(n())
# # A tibble: 3 x 2
# add_break_down `n()`
# <chr>          <int>
#  1 C_gene_segment     8
# 2 lnc_RNA         1463
# 3 transcript        74

temp %>% filter(gene_mRNA_exon_CDS=="gene&exon") %>% group_by(add_break_down) %>% summarise(n())
## A tibble: 2 x 2
#add_break_down `n()`
#<chr>          <int>
#  1 lnc_RNA          370
#2 tRNA               8

temp %>% filter(gene_mRNA_exon_CDS=="gene&mRNA") %>% group_by(add_break_down) %>% summarise(n())
## A tibble: 4 x 2
#add_break_down `n()`
#<chr>          <int>
#  1 lnc_RNA          117
#2 pseudogene        10
#3 transcript      1397
#4 NA             25779

temp %>% filter(gene_mRNA_exon_CDS=="gene&mRNA&exon") %>% group_by(add_break_down) %>% summarise(n())
## A tibble: 4 x 2
#add_break_down     `n()`
#<chr>              <int>
#  1 lnc_RNA              177
#2 lnc_RNA&transcript    12
#3 transcript           123
#4 NA                  2345

temp %>% filter(gene_mRNA_exon_CDS=="gene&mRNA&exon&CDS") %>% group_by(add_break_down) %>% summarise(n())
## A tibble: 4 x 2
#add_break_down     `n()`
#<chr>              <int>
#  1 lnc_RNA               21
#2 lnc_RNA&transcript     3
#3 transcript           247
#4 NA                  4363

temp %>% filter(is.na(gene_mRNA_exon_CDS)) %>% group_by(add_break_down) %>% summarise(n())
## A tibble: 2 x 2
#add_break_down `n()`
#<chr>          <int>
#  1 pseudogene        22
#2 NA             27331

# 5. Summarizing chromosome positions (for each individual SNP, incuding multiple SNPs per locus)
# colouring by exon, mRNA, CDS and gene annotation
bargraph <- temp %>% group_by(chromosome,gene_mRNA_exon_CDS) %>% summarise(count=n()) %>% arrange(as.numeric(gsub("[A-Z,a-z]+.*","",chromosome)))
bargraph$gene_mRNA_exon_CDS[is.na(bargraph$gene_mRNA_exon_CDS)] <- "NA"
bargraph$chromosome <- as_factor(bargraph$chromosome)

ggplot() +
  geom_col(data=bargraph,aes(x=as.factor(chromosome),y=count,fill=gene_mRNA_exon_CDS),color="black") +
  theme_bw(base_size=20) +
  scale_x_discrete(name="Chromosome",expand=c(0,0)) +
  theme(axis.title=element_text(size=24,face="bold")) +
  theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank()) +
  scale_y_continuous(name="Number of SNPs",expand=c(0,0)) + 
  theme(legend.position = "none") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_fill_manual(values=c("#00CD00","#1974CD","#4EA081","#B4B58D","#93BF6B","#BAA452","#BFBFBF"
))

# Saved manually as a plot 2100 pixels wide by 1500 pixels high: chromsome_pos.png
