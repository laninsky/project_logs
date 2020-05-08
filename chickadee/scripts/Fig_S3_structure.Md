# This code corresponds to Fig. S3 in Alexander et al.
# It runs the structure through structure threader
# on output from the ipyrad pipeline

# Loading required python module
module load Python/3.8.2-gimkl-2020a

# Installing structure_threader
pip3 install structure_threader --prefix=/nesi/nobackup/uoo00105/chickadees/bin/structure_threader

# Adding structure_threader to PATH
export PATH=/nesi/nobackup/uoo00105/chickadees/bin/structure_threader/bin:$PATH

# Using custom R code to filter out singletons as per
# Linck and Battey (2019). Code run in following directory:
#/scale_wlg_nobackup/filesets/nobackup/uoo00105/chickadees/structure
```
# loading necessary libraries
library(tidyverse)

# Reading in input file
input_stru <- read_table2("/nesi/nobackup/uoo00105/chickadees/ipyrad/chickadee_ref_outfiles/chickadee_ref.ustr",col_names=FALSE)

# Removing rows corresponding to individual 649257
# (characterized by extremely low sequencing coverage)
input_stru <- input_stru %>% filter(X1!="649257_")

# Defining variable to store cols to be deleted
cols_to_del <- NULL

# For each column
for (i in 2:dim(input_stru)[2]) {
  # Creating a table of counts per allele
  temp <- table(input_stru[,i])
  # Excluding missing data if present
  if (-9 %in% names(temp)) {
    temp <- temp[-(which(names(temp)==-9))]
  }
  # If following the removal of 649257_
  # the site is no longer variable at all
  if (length(temp)==0) {
    # Adding it to the cols to delete
    cols_to_del <- c(cols_to_del,i)
  } else {
  # If the minor allele is a singleton
  if (min(temp)==1) {
    # Adding it to the cols to delete
    cols_to_del <- c(cols_to_del,i)
    }
  }
}

# removing the singleton columns
output_stru <- input_stru[,-cols_to_del]

# Printing number of samples and SNPs
print(paste(dim(output_stru)[1]/2,"samples"))
print(paste((dim(output_stru)[2]-1),"SNPs"))

# Writing out file
write_delim(output_stru,"chickadee_singleton_filtered.stru",col_names=FALSE)
```

# Assembling necessary files for structure run
# extraparams is a blank file with one line return in 
# it. mainparams (for inferring lamda) is as follows 
# (including the hash at the beginning of the line)

#define OUTFILE /nesi/nobackup/uoo00105/chickadees/structure 
#define INFILE /nesi/nobackup/uoo00105/chickadees/structure/chickadee_singleton_filtered.stru 
#define NUMINDS 164
#define NUMLOCI 8056 
#define LABEL 1 
#define POPDATA 0 
#define POPFLAG 0 
#define LOCDATA 0 
#define PHENOTYPE 0 
#define MARKERNAMES 0 
#define MAPDISTANCES 0 
#define ONEROWPERIND 0 
#define PHASEINFO 0 
#define PHASED 0 
#define RECESSIVEALLELES 0 
#define EXTRACOLS 0
#define MISSING -9
#define PLOIDY 2
#define MAXPOPS 1
#define BURNIN 50000
#define NUMREPS 100000


#define NOADMIX 0
#define LINKAGE 0
#define USEPOPINFO 0

#define LOCPRIOR 0
#define INFERALPHA 1
#define ALPHA 1.0
#define POPALPHAS 0 
#define UNIFPRIORALPHA 1 
#define ALPHAMAX 10.0
#define ALPHAPROPSD 0.025


#define INFERLAMBDA 1 

#define FREQSCORR 0
#define POPSPECIFICLAMBDA 0 
#define LAMBDA 1.0
#define COMPUTEPROB 1 
#define PFROMPOPFLAGONLY 0 
#define ANCESTDIST 0 
#define STARTATPOPINFO 0 
#define METROFREQ 10


#define UPDATEFREQ 1 


# Running structure_threader at K=1 to infer lambda

#!/bin/bash -e

#SBATCH -A uoo00105 
#SBATCH -J infer_lambda_stru
#SBATCH --ntasks 1
#SBATCH -c 18
#SBATCH -t 8:00:00
#SBATCH --mem=54G
#SBATCH -D /nesi/nobackup/uoo00105/chickadees/structure 
#SBATCH --mail-type=ALL
#SBATCH --mail-user=alana.alexander@otago.ac.nz
#SBATCH -N 1
#SBATCH --hint=nomultithread
#SBATCH --partition=large

module load Python/3.8.2-gimkl-2020a
export PATH=/nesi/nobackup/uoo00105/chickadees/bin/structure_threader/bin:$PATH
export PYTHONPATH=/nesi/nobackup/uoo00105/chickadees/bin/structure_threader/lib/python3.8/site-packages:$PYTHONPATH
structure_threader run -K 1 -R 1 -i chickadee_singleton_filtered.stru -o infer_lambda -st /nesi/nobackup/uoo00105/chickadees/bin/structure_threader/bin/structure -t 18 --no_plots TRUE --no_tests TRUE

# Lambda was inferred as  0.3710
# Mainparams for "actual" structure runs

#define OUTFILE /nesi/nobackup/uoo00105/chickadees/structure/lambda_0_3710
#define INFILE /nesi/nobackup/uoo00105/chickadees/structure/chickadee_singleton_filtered.stru 
#define NUMINDS 164
#define NUMLOCI 8056
#define LABEL 1 
#define POPDATA 0 
#define POPFLAG 0 
#define LOCDATA 0 
#define PHENOTYPE 0 
#define MARKERNAMES 0 
#define MAPDISTANCES 0 
#define ONEROWPERIND 0 
#define PHASEINFO 0 
#define PHASED 0 
#define RECESSIVEALLELES 0 
#define EXTRACOLS 0
#define MISSING -9
#define PLOIDY 2
#define MAXPOPS 1
#define BURNIN 50000
#define NUMREPS 100000


#define NOADMIX 0
#define LINKAGE 0
#define USEPOPINFO 0

#define LOCPRIOR 0
#define INFERALPHA 1
#define ALPHA 1.0
#define POPALPHAS 0 
#define UNIFPRIORALPHA 1 
#define ALPHAMAX 10.0
#define ALPHAPROPSD 0.025


#define FREQSCORR 1 
#define ONEFST 0
#define FPRIORMEAN 0.01
#define FPRIORSD 0.05


#define INFERLAMBDA 0 
#define LAMBDA 0.3710
#define COMPUTEPROB 1 
#define PFROMPOPFLAGONLY 0 
#define ANCESTDIST 0 
#define STARTATPOPINFO 0 
#define METROFREQ 10


#define UPDATEFREQ 1

# Sbatch for doing "actual" structure runs
# Following code runs K1 through 5
# Job finished in ~11 hours and didn't use more than 1GB
# so those SBATCH parameters could be adjusted for future runs

#!/bin/bash -e

#SBATCH -A uoo00105 
#SBATCH -J K1-5
#SBATCH --ntasks 1
#SBATCH -c 18
#SBATCH -t 72:00:00
#SBATCH --mem=25G
#SBATCH -D /nesi/nobackup/uoo00105/chickadees/structure 
#SBATCH --mail-type=ALL
#SBATCH --mail-user=alana.alexander@otago.ac.nz
#SBATCH -N 1
#SBATCH --hint=nomultithread
#SBATCH --partition=large

module load Python/3.8.2-gimkl-2020a
export PATH=/nesi/nobackup/uoo00105/chickadees/bin/structure_threader/bin:$PATH
export PYTHONPATH=/nesi/nobackup/uoo00105/chickadees/bin/structure_threader/lib/python3.8/site-packages:$PYTHONPATH
structure_threader run -K 5 -R 5 -i chickadee_singleton_filtered.stru -o results -st /nesi/nobackup/uoo00105/chickadees/bin/structure_threader/bin/structure -t 18 --no_plots TRUE --no_tests TRUE

