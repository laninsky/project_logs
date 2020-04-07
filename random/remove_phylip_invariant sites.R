remove_phylip_invariant sites <-  function(path_to_file) {
  # This script takes a phylip file with SNP genotypes where invariant sites are present
  # It strips the invariant sites out and updates the total number of sites in the alignment 
  # and outputs a phylip file suffixed by no_invariant.phy
  # e.g. remove_phylip_invariant sites("/users/alanaalexander/Downloads/select_pig_data.phy")
  
  # Loading required libraries
  if (!require('tidyverse')) install.packages('tidyverse'); library('tidyverse')
  
  # Reading in file
  file <- read_table2(path_to_file)
  
  # Setting up a matrix to save the "split" alignment into (site by site)
  split_alignment <- matrix(NA,nrow=as.numeric(names(file)[1]),ncol=as.numeric(names(file)[2]))
  
  # Populating this matrix with the sequence data split out by site
  for (i in 1:dim(split_alignment)[1]) {
    split_alignment[i,] <- unlist(strsplit(as.matrix(file[i,2]),""))
  }
  
  # Finding the sites that are invariant across the alignment
  invariant_sites <- NULL
  # For each site in the alignment
  for (i in 1:dim(split_alignment)[2]) {
    # If the total number of SNP states (excluding missing data) is less than 2
    # (i.e. the site is invariable across the individuals included in the alignment)
    if((length(which(unique(split_alignment[,i])!="-"))) < 2) {
      # Then add this site to the list of invariant sites
      invariant_sites <- c(invariant_sites,i)
    }
  }
  
  # Removing these sites from the alignment
  split_alignment <- split_alignment[,-invariant_sites]
  
  # Getting the new number of sites for the output
  output <- c(as.numeric(names(file)[1]),dim(split_alignment)[2])
  
  # Creating a matrix to capture the output
  output_sequence <- matrix(NA,nrow=as.numeric(names(file)[1]),ncol=2)
  # Populating this with the sequence names
  output_sequence[,1] <- as.matrix(file[,1])
  
  
  # Collapsing the sequence data to a string for each individual
  for (i in 1:dim(split_alignment)[1]) {
    output_sequence[i,2] <- paste(split_alignment[i,],collapse = "")
  }
  
  # Getting the final output file together
  output <- rbind(output,output_sequence)
  
  # Writing out the resultant file
  write.table(output,paste(path_to_file,"no_invariant.phy",sep=""),col.names = FALSE,row.names = FALSE,quote = FALSE)
  
}  
