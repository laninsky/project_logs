sample_phylip_alleles <- function(path_to_file,suffix_A,suffix_B) {
    # This script takes a phylip file with SNP genotypes
    # e.g. an "allele 1" row and "allele 2" row for each individual for each SNP
    # This script randomly samples one allele for each SNP for each individual
    # and outputs a phylip file with just one row per individual
  
    # The function requires the path and name of your file
    # The suffix for the "allele 1s" in your phylip file
    # The suffix for the "allele 2s" in your phylip file
    # e.g. sample_phylip_alleles("Tim_phylip_file_16Mar2020.phy","_A","_B")
  
    # Loading required libraries
    if (!require('tidyverse')) install.packages('tidyverse'); library('tidyverse')
  
    # Reading in file
    phylip_file <- read_table2(path_to_file)
    
    # Getting new number of taxa and creating beginning of output
    new_taxa <- as.numeric(names(phylip_file)[1])/2
    output <- c(new_taxa,as.numeric(names(phylip_file)[2]))
    
    # Stripping off the suffixes from the sample names
    gsub_pattern <- paste(suffix_A,"$","|",suffix_B,"$",sep="")
    phylip_file[,1] <- gsub(gsub_pattern,"",as.matrix(phylip_file[,1]))
    
    # Obtaining list of unique sample names
    sample_names <- as.matrix(unique(phylip_file[,1]))
    
    # Going through the sample names and randomly sampling allele states at each SNP
    for (i in sample_names) {
      # Getting the rows corresponding to the ith sample name
      temp <- phylip_file[which(as.matrix(phylip_file[,1]) %in% i),]
      # Creating a random array to select which allele will represent each SNP
      rand_array <- runif(as.numeric(names(phylip_file)[2]))
      # Creating an output variable to record the individual's data, and populating it with allele1
      indiv_SNPs <- unlist(strsplit(as.matrix(temp[1,2]),split=""))
      # Doing the same thing for allele2, because we'll replace some of indiv_SNPs (i.e. allele1)
      # with these allele states
      allele2 <- unlist(strsplit(as.matrix(temp[2,2]),split=""))
      # For each SNP, if its associated random number in rand_array is less than 0.5
      # then replacing the allele in indiv_SNPs with the allele2 allele
      indiv_SNPs[which(rand_array<=0.5)] <- allele2[which(rand_array<=0.5)] 
      
      # Recording the output for the individual
      temp_output <- c(i,paste(indiv_SNPs,collapse=""))
      
      # Binding this to the output variable
      output <- rbind(output,temp_output)
      
    }
    # Writing out the resultant file
    write.table(output,"individual_level_phylip.phy",col.names = FALSE,row.names = FALSE,quote = FALSE)
}
