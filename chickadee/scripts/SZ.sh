# This code corresponds to Fig SZ in Alexander et al.
# It runs programs/scripts to summarize the genic regions covered
# by the ddRADseq sequencine and generating genomic cline analyses in 
# Alexander et al.

# 1. Summazing genic content and chromosome position of RADseq loci
# Obtain chromosome labels for scaffolds
zgrep -E "RefSeq" GCF_001522545.3_Parus_major1.1_genomic.gff.gz | grep "region" | grep "chromosome" | sed 's/RefSeq.*Name=//g' | sed 's/;chromosome.*//g' > chromosome_scaffolds.txt

# Strip comment rows off the output vcf file and GFF file
grep -v "##" ref_guided.vcf > headerless.vcf
grep -F -e "##" ref_guided.vcf > header_rows.txt

# Using R code to summarize RADseq markers based on ref_guided.snps.map
# (in order to find which SNP positions correspond to each locus),
# headerless.vcf (to obtain chromosome, and position along chromosome),
# chromosome_scaffolds.txt (to find which scaffold corresponds to what chromosome)
# popmap_final.txt (to divide up birds into "Carolina", "blackcapped", "hybrid"
Rscript SCC.R

# 2. Compiling the bgc software in bin directory
# Loading required modules
module load HDF5/1.10.5-gimkl-2018b
module load GSL/2.4-GCC-7.4.0
# Compiling based on instructions at https://popgencode.wordpress.com/2015/04/08/hello-world/
h5c++ -Wall -O2 -L/usr/include/gsl -I/usr/include/gsl -o bgc bgc_main.C bgc_func_readdata.C bgc_func_initialize.C bgc_func_mcmc.C bgc_func_write.C bgc_func_linkage.C bgc_func_ngs.C bgc_func_hdf5.C mvrandist.c -lgsl -lgslcblas
# Compiling estpost
h5c++ -Wall -O3  -o estpost estpost_h5.c -lgsl -lgslcblas

# 3. Running bgc
# Making directory and copying necessary inputs into it
mkdir bgc
mv black_capped.txt bgc
mv Carolina.txt bgc
mv admixed.txt bgc
mv genetic_map.txt bgc

# Loading required modules and setting path
module load HDF5/1.10.5-gimkl-2018b
module load GSL/2.4-GCC-7.4.0
export PATH=/nesi/nobackup/uoo00105/bin/bgcdist:$PATH

# Bgc input explanation (guided by mix of bgc manual and Taylor et al. 2014)
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
# set this to 522.727 as this was the maximum distance in kb of any of the LG in the
# P. major genome: manual was in cM in comparison]
# -T If non-zero, use a truncated gamma prior for tau with this upper bound [default =
# 0]. Otherwise use a full gamma prior.
# -u MCMC tuning parameter, maximum deviate from uniform for proposed hybrid index
# hybrid index [default = 0.1].
# -g MCMC tuning parameter, standard deviation for Gaussian proposal of cline param-
# eter gamma [default = 0.05].
# -z MCMC tuning parameter, standard deviation for Gaussian proposal of cline parameter zeta [default = 0.05]
# -e MCMC tuning paramteer, standard deviation for Gaussian proposal of cline parameters eta and kappa [default = 0.02]

bgc -a black_capped.txt -b Carolina.txt -h admixed.txt -M genetic_map.txt -O 2 -x 50000 -n 25000 -t 5 -p 1 -q 1 -i 0 -N 1 -E 0.0001 -m 1 -d 1 -s 1 -o 0 -I 0 -D 522.727 -T 0 -u 0.1 -g 0.08 -z 0.05 -e 0.2
# After copying out outputfiles, using same code as above, ran a second chain to check convergence
 
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

 
# 4. Summarizing bgc output
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
# With Bonferroni correction for 5,722 loci
#Alpha:
estpost -i mcmcout.hdf5 -o alphaest.txt -p alpha -s 0 -c 0.9999912618 -w 1
#Beta
estpost -i mcmcout.hdf5 -o betaest.txt -p beta -s 0 -c 0.9999912618 -w 1
#Gamma
estpost -i mcmcout.hdf5 -o gammaest.txt -p gamma-quantile -s 0 -c 0.9999912618 -w 1
#Zeta
estpost -i mcmcout.hdf5 -o zetaest.txt -p zeta-quantile -s 0 -c 0.9999912618 -w 1
#Hi
estpost -i mcmcout.hdf5 -o hi.txt -p hi -s 0 -c 0.9999912618 -w 1

# Checking for stationarity and presenting results
Rscript SDD.R


# 



The parameter arguments gamma-quantile and zeta-quantile refer to the locus effects for
α (γ) and β (ζ; Gompert & Buerkle 2011a). We use the credbile intervals to identify loci with
excess ancestry and the quantile estimates to identify outlier loci (Lexer et al. 2007; Gompert
& Buerkle 2011a; Gompert et al. 2012a).

