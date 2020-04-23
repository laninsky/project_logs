thin_phylip <- function(path_to_file,path_to_id_group_file,groups_comma_delimited) {
  # This function thins out a phylip file to just one representative per group
  # except for groups specified in the groups_comma_delimited string, where all 
  # individuals are retained.
  
  # The function requires the path and name of your phylip file [path_to_file];
  # a tab-delimited file with individual ID in the first column (matching the phylip file) 
  # and the group in the second [path_to_id_group_file]; and a comma-delimited string
  # of the breeds to be completely retained [breeds_comma_delimited].
  
  # It outputs a phylip file with the same name as the input file, appended by "thinned.phy" 
  # e.g. thin_phylip("/Users/alanaalexander/Downloads/individual_level_phylip.phy","/Users/alanaalexander/Downloads/sample_id_and_breed_columns.txt","Canterbury_pig,Auckland_Island_feral_pig,North_Island_pig,Ossabaw")
  
  # Loading required libraries
  if (!require('tidyverse')) install.packages('tidyverse'); library('tidyverse')
  if (!require('ape')) install.packages('ape'); library('ape')
  
  # Reading in phylip file
  phylip_file <- read_table2(path_to_file)
  
  # Reading in the tab delimited file with groups
  speciesid_files <- read_delim(path_to_id_group_file,delim="\t")
  
  # Getting the list of breeds to completely retain
  breed_list <- unlist(strsplit(breeds_comma_delimited,","))
  
  # Getting taxa names of breeds that won't be completely retain
  thinned_taxa <- as.matrix(speciesid_files %>% 
                              filter(!(.[[2]] %in% breed_list)) %>% 
                              group_by(.[[2]]) %>% slice(1))[,1]
  
  # Getting taxa names of breeds that will be completely retained
  non_thinned_taxa <- as.matrix(speciesid_files %>% 
                                  filter(.[[2]] %in% breed_list))[,1]
  
  # Retaining the rows in the phylip file pertaining to the samples we want to keep
  phylip_file_thinned <- phylip_file[(which(as.matrix(phylip_file[,1]) %in% c(thinned_taxa,non_thinned_taxa))),]
  
  # Adjusting the number of taxa in the phylip title row
  output <- c(dim(phylip_file_thinned)[1],as.numeric(names(phylip_file)[2]))
  
  # Binding the data to the output row
  output <- rbind(output,as.matrix(phylip_file_thinned))
  
  # Writing out the resultant file
  write.table(output,paste(path_to_file,".thinned.phy",sep=""),col.names = FALSE,row.names = FALSE,quote = FALSE)
}