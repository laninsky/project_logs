# Loading in libraries
library(tidyverse)
library(eulerr)

# Reading in file
fst_file <- read_csv("/Users/alanaalexander/Downloads/wild_domestic_feral_all_fst.csv")

# Counting numbers in each category
fst_file %>% 
  mutate(venn=ifelse((`1_0 top 10%`=="N" & `1_2 top 10%`=="N" & `0_2 top 10%`=="N"),"Not sig.",
                     ifelse((`1_0 top 10%`=="Y" & `1_2 top 10%`=="Y" & `0_2 top 10%`=="Y"),"1_0&1_2&0_2",
                     ifelse((`1_0 top 10%`=="Y" & `1_2 top 10%`=="N" & `0_2 top 10%`=="N"),"1_0",
                            ifelse((`1_0 top 10%`=="N" & `1_2 top 10%`=="Y" & `0_2 top 10%`=="N"),"1_2",
                                   ifelse((`1_0 top 10%`=="N" & `1_2 top 10%`=="N" & `0_2 top 10%`=="Y"),"0_2",ifelse((`1_0 top 10%`=="Y" & `1_2 top 10%`=="Y" & `0_2 top 10%`=="N"),"1_0&1_2",
              ifelse((`1_0 top 10%`=="Y" & `1_2 top 10%`=="N" & `0_2 top 10%`=="Y"),"1_0&0_2",
                     ifelse((`1_0 top 10%`=="N" & `1_2 top 10%`=="Y" & `0_2 top 10%`=="Y"),"1_2&0_2",NA
                            ))))))))) %>% group_by(venn) %>% summarise(n())

# Checking the totals from above add up to the total number of loci in the file
(215+275+30+9+229+69+30744+7)==dim(fst_file)[1]

# Setting up the Venn diagram
plot(venn(c("0 vs 2"=215,"1 vs 0"=275,"1 vs 0&0 vs 2"=30,"1 vs 0&1 vs 2"=9,"1 vs 2"=229,"1 vs 2&0 vs 2"=69),quantities=list(type="counts",cex=2)))

# Setting up Eulerr diagram
plot(euler(c("0 vs 2"=215,"1 vs 0"=275,"1 vs 0&0 vs 2"=30,"1 vs 0&1 vs 2"=9,"1 vs 2"=229,"1 vs 2&0 vs 2"=69)),quantities=TRUE)
