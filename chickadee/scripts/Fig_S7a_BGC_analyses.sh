# This code corresponds to Fig. S7(a) in Alexander et al.
# It runs programs/scripts to summarize the genic regions covered
# by the ddRADseq sequencing and generates genomic cline analyses 
# via bgc (and outputs Fig 4, Table S6, and Fig S11) in
# Alexander et al.

# 1. Getting bgc inputs together
# Making directory and copying necessary inputs into it
mkdir bgc
cd bgc

# Following files need to be copied into the bgc folder
# chickadee_ref.vcf 
# *.snps.map or *.snpsmap from ipyrad outfiles
# S7b_generate_bgc_inputs.R
# S7c_stationarity_convergence_results.R
# Table_S1.txt

#2. Downloading annotations for black-capped chickadee genome
wget https://ftp.ncbi.nlm.nih.gov/genomes/all/GCA/011/421/415/GCA_011421415.1_CUB_Patr_1.0/GCA_011421415.1_CUB_Patr_1.0_genomic.gbff.gz

# Summazing genic content and chromosome position of RADseq loci
# Obtain chromosome labels for scaffolds from a gff file (when previously using P. major as reference)
# zgrep -E "RefSeq" GCF_001522545.3_Parus_major1.1_genomic.gff.gz | grep "region" | grep "chromosome" | sed 's/RefSeq.*Name=//g' | sed 's/;chromosome.*//g' > chromosome_scaffolds.txt

# Obtain chromosome labels for scaffolds from a gbff file (current code)
gunzip GCA_011421415.1_CUB_Patr_1.0_genomic.gbff.gz
grep VERSION GCA_011421415.1_CUB_Patr_1.0_genomic.gbff | awk '{ print $2 }' > scaffold_names.txt
grep DEFINITION GCA_011421415.1_CUB_Patr_1.0_genomic.gbff | sed 's/DEFINITION  Poecile atricapillus chromosome //g' | sed 's/DEFINITION  Poecile atricapillus scaffold[0-9]*-unlocalized-//g' | sed 's/DEFINITION  Poecile atricapillus //g' | sed 's/, .*//g' | sed 's/ .*//g' > chromosomes.txt
paste scaffold_names.txt chromosomes.txt > chromosome_scaffolds.txt

# Example of what chromosome_scaffolds.txt looks like
CM022157.1	1
JAAMOC010000001.1	1
JAAMOC010000002.1	1
JAAMOC010000003.1	1
JAAMOC010000004.1	2
JAAMOC010000005.1	2
JAAMOC010000550.1	2
JAAMOC010000551.1	2
JAAMOC010000552.1	2
JAAMOC010000010.1	3

# Strip comment rows off the output vcf file and GFF file
grep -v "##" chickadee_ref.vcf > headerless.vcf
grep -F -e "##" chickadee_ref.vcf > header_rows.txt

# Using R code to summarize RADseq markers based on ref_guided.snps.map
# (in order to find which SNP positions correspond to each locus),
# headerless.vcf (to obtain chromosome, and position along chromosome),
# chromosome_scaffolds.txt (to find which scaffold corresponds to what chromosome)
# *.snps.map or *.snpsmap from ipyrad outfiles: in order to find which SNP 
# positions correspond to each locus (in this case chickadee_ref.snpsmap )
module load R/3.6.2-gimkl-2020a 
Rscript Fig_S7b_generate_bgc_inputs.R

# 3. Compiling the bgc software in bin directory
# Obtaining the bgc tar
wget https://sites.google.com/site/bgcsoftware/home/bgcdist1.03.tar.gz
tar -zvxf bgcdist1.03.tar.gz
cd 
# Loading required modules
module load HDF5/1.10.5-gimkl-2018b
module load GSL/2.4-GCC-7.4.0
# Compiling based on instructions at https://popgencode.wordpress.com/2015/04/08/hello-world/
h5c++ -Wall -O2 -L/usr/include/gsl -I/usr/include/gsl -o bgc bgc_main.C bgc_func_readdata.C bgc_func_initialize.C bgc_func_mcmc.C bgc_func_write.C bgc_func_linkage.C bgc_func_ngs.C bgc_func_hdf5.C mvrandist.c -lgsl -lgslcblas
# Compiling estpost
h5c++ -Wall -O3  -o estpost estpost_h5.c -lgsl -lgslcblas

# 4. Bgc input options explanation (guided by mix of bgc manual and Taylor et al. 2014)
# -a Infile with genetic data for parental population 0.
# -b Infile with genetic data for parental population 1.
# -h Infile with genetic data for admixed population(s).
# -M Infile with genetic map, only used for ICARrho model
# -O format to write MCMC samples (2=HDF5 and ascii text)
# -x Number of MCMC steps for the analysis
# -n Discard the first n MCMC samples as a burn-in
# -t Thin MCMC samples by recording every nth value
# -p Specifies which parameter samples to print: 1 = also print precision parameters
# -q Boolean, calculate and print cline parameter quantiles [default = 0]
# -i Boolean, calculate and print interspecific-heterozygosity [default = 0].
# -N Boolean, use genotype-uncertainty model
# -E Boolean, use sequence error model, only valid in conjunction with the genotype-
# uncertainty model [default = 0].
# -m Boolean, use ICARrho model for linked loci [default = 0].
# -d Boolean, all loci are diploid
# -s Boolean, sum-to-zero constraint on locus cline parameters
# -o Boolean, assume a constant population-level cline parameter variance for all loci
# -I Select algorithm to initialize MCMC [default = 1]. 0 = use information from the
# data to initialize ancestry and hybrid index [0 only works with read data not called genotypes]
# -D Maximum distance between loci, free recombination [default = 0.5]. [In contrast to manual
# set this to 1252.497 as this was the maximum size of scaffolds in kb mapping to LG in the
# black-capped chickadee genome: manual was in cM in comparison]
# -T If non-zero, use a truncated gamma prior for tau with this upper bound [default =
# 0]. Otherwise use a full gamma prior.
# -u MCMC tuning parameter, maximum deviate from uniform for proposed hybrid index
# hybrid index [default = 0.1].
# -g MCMC tuning parameter, standard deviation for Gaussian proposal of cline param-
# eter gamma [default = 0.05].
# -z MCMC tuning parameter, standard deviation for Gaussian proposal of cline parameter zeta [default = 0.05]
# -e MCMC tuning paramteer, standard deviation for Gaussian proposal of cline parameters eta and kappa [default = 0.02]

# 4. Sbatch script for running bgc
#!/bin/bash -e

#SBATCH -A uoo00105 
#SBATCH -J bgc
#SBATCH --ntasks 1
#SBATCH -c 1
#SBATCH -t 72:00:00
#SBATCH --mem=3G
#SBATCH -D /nesi/nobackup/uoo00105/chickadees/bgc 
#SBATCH --mail-type=ALL
#SBATCH --mail-user=alana.alexander@otago.ac.nz
#SBATCH -N 1
#SBATCH --hint=nomultithread
#SBATCH --partition=large

module load HDF5/1.10.5-gimkl-2018b
module load GSL/2.4-GCC-7.4.0
export PATH=/nesi/nobackup/uoo00105/chickadees/bin/bgcdist:$PATH

bgc -a black_capped.txt -b Carolina.txt -h admixed.txt -M genetic_map.txt -O 2 -x 50000 -n 25000 -t 5 -p 1 -q 1 -i 0 -N 1 -E 0.0001 -m 1 -d 1 -s 1 -o 0 -I 0 -D 1252.497 -T 0 -u 0.1 -g 0.08 -z 0.05 -e 0.2

# 5. After copying out outputfiles into run1_output, using same code as above, 
# ran a second shorter chain (-x 25000 -n 12500) to check convergence and copied
# these files into run2_output

# Summary of output files
 
# LnL output.txt: Each line in this file contains the log likelihood for a single MCMC step.
# For this and all other files only post-burnin post-thinning MCMC samples are included.

# alpha output.txt: Each line begins with the locus number (in input order, but simply
# numbered 0 to N, where N is one less than the number of loci). This is followed by the
# cline parameter α for each population. Populations are separated by commas. Each line
# corresponds to a single MCMC step and additional lines give samples for subsequent MCMC
# steps.

# beta output.txt: This file contains MCMC samples for cline parameter β and follows the
# format described for alpha output.txt.

# hi output.txt: This file contains MCMC samples of hybrid index (h). Each line corresponds
# to a single MCMC step and contains MCMC samples of hybrid index for each individual.
# The parameter values for each individual are separated by commas and appear in the order
# that individuals were listed in the input file.

# gamma output.txt: This file contains the quantile of each γ cline parameter in the
# estimated genome-wide distribution. Each line corresponds to a single MCMC step and
# gives the quantiles for each locus in order. Values are comma separated.

# zeta output.txt: This file contains the quantile of each ζ cline parameter in the estimated
# genome-wide distribution and follows the format described for q gamma output.txt.

# 6. Summarizing bgc output
# Within the run1_output and run2_output folders:

# -i Infile, MCMC results from bgc in HDF5 format.
# -o Outfile [default = postout].
# -p Name of parameter to summarize, possibilities include: ’LnL’, ’alpha’, ’beta’,
# ’eta’, ’eta-quantile’, ’gamma-quantile’, ’gamma-quantile-local’, ’hi’, ’interspecific-
# het’, ’kappa’, ’kappa-quantile’, ’rho’, ’tau-alpha’, ’tau-beta’, ’zeta-quantile’, and
# ’zeta-quantile-local’.
# -c Credible interval to calculate [default = 0.95].
# -b Number of additional (beyond the burn-in passed to bgc) MCMC samples to discard
# for burn-in [default = 0]. This burn-in is based on the number of thinned samples.
# -h Number of bins for posterior sample histogram [default = 20].
# -s Which summary to perform: 0 = posterior estimates and credible intervals, 1 =
# histogram of posterior samples, 2 = convert to plain text.
# -w Write parameter identification and headers to file, boolean [default = 1].

# Point estimates and CI (-s 0): This file contains an optional header row and parameter
# identification column (parameters are ordered and number as they were ordered in the input
# files but starting with 0). Each line gives the mean, median, and lower and upper bounds of
# the specified credible interval (this is an equal-tail probability interval).

# Posterior point estimates and 95% ETPIs for the α and β parameters and cline parameter quantiles
# With Bonferroni correction for 6,748 loci
#Alpha:
estpost -i mcmcout.hdf5 -o alphaest.txt -p alpha -s 0 -c 0.99999259039 -w 1
#Beta
estpost -i mcmcout.hdf5 -o betaest.txt -p beta -s 0 -c 0.99999259039 -w 1
#Gamma
estpost -i mcmcout.hdf5 -o gammaest.txt -p gamma-quantile -s 0 -c 0.99999259039 -w 1
#Zeta
estpost -i mcmcout.hdf5 -o zetaest.txt -p zeta-quantile -s 0 -c 0.99999259039 -w 1
#Hi
estpost -i mcmcout.hdf5 -o hi.txt -p hi -s 0 -c 0.99999259039 -w 1

# 7a. Checking for stationarity and convergence
Rscript Fig_S7c_stationarity_convergence_results.R

# Based on the stationarity/convergence results, it might be a good idea to remove additional burn-in
# In our case, 1500 states were removed for both run1 and run2, and these are the summarized files
# in the github repository

#Alpha:
estpost -i mcmcout.hdf5 -o alphaest.txt -p alpha -s 0 -c 0.99999259039 -w 1 -b 1500
#Beta
estpost -i mcmcout.hdf5 -o betaest.txt -p beta -s 0 -c 0.99999259039 -w 1 -b 1500
#Gamma
estpost -i mcmcout.hdf5 -o gammaest.txt -p gamma-quantile -s 0 -c 0.99999259039 -w 1 -b 1500
#Zeta
estpost -i mcmcout.hdf5 -o zetaest.txt -p zeta-quantile -s 0 -c 0.99999259039 -w 1 -b 1500
#Hi
estpost -i mcmcout.hdf5 -o hi.txt -p hi -s 0 -c 0.99999259039 -w 1 -b 1500

# 7b. Re-checking for stationarity and convergence and presenting results
Rscript Fig_S7c_stationarity_convergence_results.R

# 8. Loading necessary modules for pulling out genes of interest
module load seqtk/1.3-gimkl-2018b

# Copying across the annotations for the other black-capped chickadee genome (less contiguous than the one we
# used for reference mapping, but with annotations available)
wget https://ftp.ncbi.nlm.nih.gov/genomes/genbank/vertebrate_other/Poecile_atricapillus/latest_assembly_versions/GCA_013398625.1_ASM1339862v1/GCA_013398625.1_ASM1339862v1_cds_from_genomic.fna.gz
gunzip GCA_013398625.1_ASM1339862v1_cds_from_genomic.fna.gz

# downloading magic-blast (need splice-aware method, as the nucleotide CDS we've downloaded will exclude intons)
wget https://ftp.ncbi.nlm.nih.gov/blast/executables/magicblast/LATEST/ncbi-magicblast-1.5.0-x64-linux.tar.gz
tar -zvxf ncbi-magicblast-1.5.0-x64-linux.tar.gz 
export PATH=/nesi/nobackup/uoo00105/chickadees/bin/ncbi-magicblast-1.5.0/bin:$PATH

number_of_matches=`wc -l Table_S6_outlying_marker_bed_format.bed | awk '{print $1}'`

# Outputting a list of genes found in the regions of interest (potential inversions)
for i in `seq 1 $number_of_matches`;
  do bedline=`head -n $i Table_S6_outlying_marker_bed_format.bed | tail -n 1`;
  echo $bedline > temp.bed;
  seqname=`echo $bedline | awk '{print $1}'`;
  seqtk subseq GCA_011421415.1_CUB_Patr_1.0_genomic.fna temp.bed > $seqname.$i.fa;
  magicblast -query GCA_013398625.1_ASM1339862v1_cds_from_genomic.fna -subject $seqname.$i.fa -perc_identity 99 -outfmt tabular -no_unaligned > $seqname.$i.matches.txt;
  grep -v "#" $seqname.$i.matches.txt | awk '{print $1}' > temp_sequence_name.txt;
  seqtk subseq GCA_013398625.1_ASM1339862v1_cds_from_genomic.fna temp_sequence_name.txt | grep -F "[gene=" | sed 's/.*\[gene=//g' | sed 's/\].*//g' > $seqname.$i.gene_matches.txt;
  rm temp.bed;
  rm temp_sequence_name.txt;
done

# Combintions of these genes were then used in geneontology.org
# Chromosome Z
cat CM022174*gene_matches.txt | sort | uniq
# Region by region chromosome Z
cat CM022174.1.2.gene_matches.txt
cat CM022174.1.3.gene_matches.txt
cat CM022174.1.4.gene_matches.txt
cat CM022174.1.5.gene_matches.txt
cat CM022174.1.6.gene_matches.txt
cat CM022174.1.7.gene_matches.txt
cat CM022174.1.8.gene_matches.txt
cat CM022174.1.9.gene_matches.txt
cat CM022174.1.10.gene_matches.txt
cat CM022174.1.11.gene_matches.txt

#Chromosome 1A
cat JAAMOC010000547.1.1.gene_matches.txt

# All putative inversions
cat *gene_matches.txt

# Repeating a similar analysis on each of the SNPs. Creating a subfolder to hold all the data
mkdir positive_beta_SNPs
cd positive_beta_SNPs
# Place Table_S6_positive_beta_SNPs_bed_format.bed into this folder
number_of_matches=`wc -l Table_S6_positive_beta_SNPs_bed_format.bed | awk '{print $1}'`

# Outputting a list of genes found in the regions of interest
for i in `seq 1 $number_of_matches`;
  do bedline=`head -n $i Table_S6_positive_beta_SNPs_bed_format.bed | tail -n 1`;
  echo $bedline > temp.bed;
  seqname=`echo $bedline | awk '{print $1}'`;
  seqtk subseq ../GCA_011421415.1_CUB_Patr_1.0_genomic.fna temp.bed > $seqname.$i.fa;
  magicblast -query ../GCA_013398625.1_ASM1339862v1_cds_from_genomic.fna -subject $seqname.$i.fa -perc_identity 99 -outfmt tabular -no_unaligned > $seqname.$i.matches.txt;
  grep -v "#" $seqname.$i.matches.txt | awk '{print $1}' > temp_sequence_name.txt;
  seqtk subseq ../GCA_013398625.1_ASM1339862v1_cds_from_genomic.fna temp_sequence_name.txt | grep -F "[gene=" | sed 's/.*\[gene=//g' | sed 's/\].*//g' > $seqname.$i.gene_matches.txt;
  rm temp.bed;
  rm temp_sequence_name.txt;
done

# Obtaining a list of uniq genes to run through Gene Ontology
cat *gene_matches.txt | sort | uniq

# Finding the genes that are within 5,000 bp of multiple outlying SNPs
cat *gene_matches.txt | sort | uniq -c | grep -v "1 "

# Getting a summary of what genes were associated with what SNPs to add to Table S6
for i in `seq 1 $number_of_matches`; 
  do filename=`echo *.$i.gene_matches.txt`;
  seqs=`cat $filename | tr '\n' '\t'`;
  echo $filename $seqs >> ordered_gene_matches.txt;
done
