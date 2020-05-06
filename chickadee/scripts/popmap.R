# The file "popmap.txt" was created by running the following code in the ipyrad directory
#  ls fastqs/*R1* | sed 's|fastqs/||g' | sed 's/_R1.*//g' > popmap.txt
# this file has the name of the individuals as represented in their fastq file.

# Loading in required libraries
library(tidyverse)

# Reading in popmap.txt
temppopmap <- read_tsv("popmap.txt",col_names=FALSE)
# Reading in Table S1 to get structure assignments of each bird
tempstructure <- read_tsv("Table_S1.txt",col_names=TRUE)
tempstructure <- tempstructure %>% 
  select(Catalog_number,Tissue_number,BC_genetic_cluster_assignment) %>% 
  arrange(BC_genetic_cluster_assignment)

# Creating an object to store the names of the individuals as they are represented
# in the sequencing file in the same order as Table_S1.txt
tempnames <- rep(NA,dim(tempstructure)[1])


tempstructure[(grepl("90612",as.matrix(tempstructure[,1]))),1] <- "3474_"
tempstructure[(grepl("95776",as.matrix(tempstructure[,1]))),1] <- "6281_"
tempstructure[(grepl("131638",as.matrix(tempstructure[,1]))),1] <- "9898_"
tempstructure[(grepl("92269",as.matrix(tempstructure[,1]))),1] <- "7420_"
tempstructure[(grepl("92270",as.matrix(tempstructure[,1]))),1] <- "7421_"

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

tempstructure <- tempstructure %>% 
  mutate(popname=ifelse(BC_genetic_cluster_assignment<=0.05,"Carolina",
                        ifelse(BC_genetic_cluster_assignment>=0.95,"blackcapped","hybrid")))

# For post bgc analyses
tempstructure <- tempstructure %>% select(seqnames,popname,BC_genetic_cluster_assignment)
write_delim(tempstructure,"popmap_w_structure.txt",col_names = FALSE)

# For setting up bgc file
tempstructure <- tempstructure %>% select(seqnames,popname)

write_delim(tempstructure,"popmap_final.txt",col_names = FALSE)


# After writing the file out, check that there are no NA only lines at the end of the document
# Also need to add the following line to give the minimum number of samples required to have
# data per population (include the # symbol):
# Carolina:1 hybrid:1 blackcapped:1

