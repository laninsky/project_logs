
generate_downsampled <- function(fasta_location,output_dir,reps,loci_per_rep) {
  print("This function generates downsampled sets of data i.e. for bootstrapping")
  print("As arguments it expects the path to a folder containing fasta files, 'fasta_location'")
  print("The location where you want to put the downsampled datasets, 'output_dir'")
  print("The number of replicate downsampled datasets you want to create, 'reps'")
  print("The number of loci you want to downsample to, 'loci_per_rep'")
  print("e.g.")
  print("generate_downsampled(fasta_location,output_dir,reps,loci_per_rep)")
  print("Example usage:")
  cat('generate_downsampled("/Users/alanaalexander/Dropbox/UCE_data_for_Alana/50perc_data_matrix/50perc_internal_fasta","/Users/alanaalexander/Dropbox/beetles/75_downsamples",100,305)\n')

  
  # get current working directory so we can return at the end
  orig_dir <- getwd()
  
  # moving to our target directory and getting a list of the files
  setwd(fasta_location)
  fasta_files <- list.files(pattern="fasta")
  
  # creating our output directory
  dir.create(output_dir)
  
  
  for (i in 1:reps) {
    dir.create(paste(output_dir,i,sep="/"))
    files_to_copy <- sample(fasta_files,loci_per_rep,replace=FALSE)
    for (j in 1:loci_per_rep) {
      file.copy(files_to_copy[j],paste(output_dir,i,files_to_copy[j],sep="/"))
    }
  }
  # return to original working directory
  setwd(orig_dir)
}
