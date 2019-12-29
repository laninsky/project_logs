# This code corresponds to Fig SZ in Alexander et al.
# It runs programs/scripts to summarize the genic regions covered
# by the ddRADseq sequencine and generating genomic cline analyses in 
# Alexander et al.

# 1. Summazing genic content and chromosome position of RADseq loci
# Copy gff file to directory to where ipyrad outfiles are located
cp GCF_001522545.3_Parus_major1.1_genomic.gff.gz ref_guided_outfiles/

# Obtain chromosome labels for scaffolds
zgrep -E "RefSeq" GCF_001522545.3_Parus_major1.1_genomic.gff.gz | grep "region" | grep "chromosome" | sed 's/RefSeq.*Name=//g' | sed 's/;chromosome.*//g' > chromosome_scaffolds.txt

# Strip comment rows off the output vcf file and GFF file
grep -v "#" ref_guided.vcf > headerless.vcf
zgrep -v "#" GCF_001522545.3_Parus_major1.1_genomic.gff.gz > headerless.gff

# Run R code to summarize RADseq markers based on ref_guided.snps.map
# (in order to find which SNP positions correspond to each locus),
# headerless.vcf (to obtain chromosome, and position along chromosome),
# chromosome_scaffolds.txt (to find which scaffold corresponds to what
# chromosome), and headerless.gff (to see whether SNPs fell in genic regions)
Rscript SAA.R

# 1. Compiling the bgc software
# Loading required modules
module load HDF5/1.10.5-gimkl-2018b
module load GSL/2.4-GCC-7.4.0
# Compiling based on instructions at https://popgencode.wordpress.com/2015/04/08/hello-world/
h5c++ -Wall -O2 -L/usr/include/gsl -I/usr/include/gsl -o bgc bgc_main.C bgc_func_readdata.C bgc_func_initialize.C bgc_func_mcmc.C bgc_func_write.C bgc_func_linkage.C bgc_func_ngs.C bgc_func_hdf5.C mvrandist.c -lgsl -lgslcblas


* NEED TO DEFINE PARENTAL POPULATIONS BASED ON 0.95 ASSIGNMENT *
* ADMIXED INDIVIDUALS THE REMAINING INDIVIDUALS *

 We based parameter estimates on 25 000 MCMC samples taken every other iteration and following a 5000 iteration burnin
 
 No nested population effects
 
 Run second chain to check convergence
 
 Don't need to incorporate genotype uncertainty
 
 Parameter estimates were based on the median and 95 per cent equal tail probability interval of marginal posterior probability distributions. 