prune_tree <- function(path_to_newick_tree,path_to_id_group_file) {
    # This script takes a newick tree file and a tab-delimited file with
    # individual ID in the first column (matching the tree) and the group in the second
    # This script goes through each of the groups in the path_to_id_group_file
    # and retains every member of the given group, but thins the rest of the tree to
    # one individual per the rest of the groups. The purpose is to check for
    # the monophyly of groups of interest for trees with a lot of taxa.
  
    # It outputs treefiles appended by the name of the focal group not thinned
    # To call the function:
    # e.g. prune_tree("/Users/alanaalexander/Downloads/individual_level_phylip.phy.treefile","/Users/alanaalexander/Downloads/sample_id_and_breed_columns.txt")
  
    # Loading required libraries
    if (!require('tidyverse')) install.packages('tidyverse'); library('tidyverse')
    if (!require('ape')) install.packages('ape'); library('ape')
  
    # Reading in treefile
    treefile <- read.tree(path_to_newick_tree)
    
    # Reading in the tab delimited file
    speciesid_files <- read_delim(path_to_id_group_file,delim="\t")

    # Getting a list of the groups in the file
    groups <- unique(as.matrix(speciesid_files[,2]))
    
    # Stepping through each group
    for (i in 1:dim(groups)[1]) {
      # In turn, each group becomes 'tempgroup'
      tempgroup <- groups[i]
      # And all the rest of the groups become othergroups
      othergroups <- groups[which(!(groups[,1] %in% tempgroup)),1]
      # We make a "temp tree" that we can drop tips off
      temptree <- treefile
      
      monophyletic_status <- is.monophyletic(treefile, as.matrix(speciesid_files[which(as.matrix(speciesid_files)[,2]==tempgroup),1]))

      print(paste("Is the group ",tempgroup," monophyletic? ", monophyletic_status,sep=""))
      
      if (monophyletic_status==TRUE) {
        # For each of the other groups
        for (j in othergroups) {
          # Get the list of sample IDs that correspond to that group, but exclude the first sample in that list
          toremove <- as.matrix(speciesid_files[which(as.matrix(speciesid_files)[,2]==j),1])[-1,]
          # Drop these sample IDs from "temp tree"
          temptree <- drop.tip(temptree,toremove)
        }
        # Write out the tree file, suffixed by the group where all the individuals are kept
          write.tree(temptree,paste(path_to_newick_tree,".",groups[i],".tre",sep=""))  
        }
    }   
}