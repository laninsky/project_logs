# This code corresponds to Fig. S2 in Alexander et al.
# It runs the ipyrad pipeline to assemble fastq reads
# (available on the SRA) to the black-capped chickadee
# reference

# THE FOLLOWING STEPS ONLY NEED TO BE RUN ONCE TO INSTALL IPYRAD 
# Obtaining miniconda and installing it
curl -O https://repo.anaconda.com/miniconda/Miniconda3-latest-Linux-x86_64.sh
sh Miniconda3-latest-Linux-x86_64.sh
# Set /nesi/nobackup/uoo00105/chickadees/bin/miniconda3 as installation location
# 'No' to intialization: intialize it "manually" by:
source /nesi/nobackup/uoo00105/chickadees/bin/miniconda3/bin/activate
conda init

## Adding necessary channels (in this order, or shared libraries for samtools will not be correct:
# "error while loading shared libraries: libcrypto.so.1.0.0: cannot open shared object file: No such file or directory"
conda config --add channels defaults
conda config --add channels bioconda
conda config --add channels conda-forge

## Installing ipyrad
conda install ipyrad

# AFTER INSTALLATION, THESE COMMANDS RUN TO RUN IPYRAD
# Before running any ipyrad commands need to run following command
source /nesi/nobackup/uoo00105/chickadees/bin/miniconda3/bin/activate

# Downloading the black-capped chickadee reference
wget https://ftp.ncbi.nlm.nih.gov/genomes/all/GCA/011/421/415/GCA_011421415.1_CUB_Patr_1.0/GCA_011421415.1_CUB_Patr_1.0_genomic.fna.gz

# Verify md5checksum from:
#https://ftp.ncbi.nlm.nih.gov/genomes/all/GCA/011/421/415/GCA_011421415.1_CUB_Patr_1.0/md5checksums.txt
# 08b9e7adb99e02398514da5108993554  ./GCA_011421415.1_CUB_Patr_1.0_genomic.fna.gz
 md5sum GCA_011421415.1_CUB_Patr_1.0_genomic.fna.gz 
# 08b9e7adb99e02398514da5108993554  GCA_011421415.1_CUB_Patr_1.0_genomic.fna.gz

# Unzipping reference
gunzip GCA_011421415.1_CUB_Patr_1.0_genomic.fna.gz

# Removing ambiguity codes in the reference and replacing with N
module load EMBOSS/6.6.0-gimkl-2017a
maskambignuc GCA_011421415.1_CUB_Patr_1.0_genomic.fna
# Rename output file to GCA_011421415.1_CUB_Patr_1.0_genomic.fna

# Getting params file together:
------- ipyrad params file (v.0.9.51)-------------------------------------------
chickadee_ref                  ## [0] [assembly_name]: Assembly name. Used to name output directories for assembly steps
/scale_wlg_nobackup/filesets/nobackup/uoo00105/chickadees ## [1] [project_dir]: Project dir (made in curdir if not present)
                               ## [2] [raw_fastq_path]: Location of raw #non-demultiplexed fastq files
                               ## [3] [barcodes_path]: Location of barcodes file
/nesi/nobackup/uoo00105/chickadees/fastqs/*.gz ## [4] [sorted_fastq_path]: Location of demultiplexed/sorted fastq files
reference               ## [5] [assembly_method]: Assembly method (denovo, reference)
GCA_011421415.1_CUB_Patr_1.0_genomic.fna ## [6] [reference_sequence]: Location of reference sequence file
pairddrad                      ## [7] [datatype]: Datatype (see docs): rad, gbs, ddrad, etc.
ACGTCC, GG                     ## [8] [restriction_overhang]: Restriction overhang (cut1,) or (cut1, cut2)
4                              ## [9] [max_low_qual_bases]: Max low quality base calls (Q<20) in a read
33                             ## [10] [phred_Qscore_offset]: phred Q score offset (33 is default and very standard)
6                              ## [11] [mindepth_statistical]: Min depth for statistical base calling
6                              ## [12] [mindepth_majrule]: Min depth for majority-rule base calling
10000                          ## [13] [maxdepth]: Max cluster depth within samples
0.88                           ## [14] [clust_threshold]: Clustering threshold for de novo assembly
0                              ## [15] [max_barcode_mismatch]: Max number of allowable mismatches in barcodes
2                              ## [16] [filter_adapters]: Filter for adapters/primers (1 or 2=stricter)
35                             ## [17] [filter_min_trim_len]: Min length of reads after adapter trim
2                              ## [18] [max_alleles_consens]: Max alleles per site in consensus sequences
0.05                           ## [19] [max_Ns_consens]: Max N's (uncalled bases) in consensus
0.05                           ## [20] [max_Hs_consens]: Max Hs (heterozygotes) in consensus
4                              ## [21] [min_samples_locus]: Min # samples per locus for output
0.2                            ## [22] [max_SNPs_locus]: Max # SNPs per locus
8                              ## [23] [max_Indels_locus]: Max # of indels per locus
0.5                            ## [24] [max_shared_Hs_locus]: Max # heterozygous sites per locus
0, 0, 0, 0                     ## [25] [trim_reads]: Trim raw read edges (R1>, <R1, R2>, <R2) (see docs)
0, 0, 0, 0                     ## [26] [trim_loci]: Trim locus edges (see docs) (R1>, <R1, R2>, <R2)
p, s, l, k, s, v               ## [27] [output_formats]: Output formats (see docs)
/nesi/nobackup/uoo00105/chickadees/popmap_final.txt ## [28] [pop_assign_file]: Path to population assignment file
                               ## [29] [reference_as_filter]: Reads mapped to this reference are removed in step 3

# Getting popmap_final.txt together
132046_32564 unknown
132092_32553 unknown
132097_32589 unknown
7420_ carolina
132051_32626 unknown
132068_32609 unknown
132089_32560 unknown
132091_32585 unknown
132120_32545 unknown
649278_ unknown
649246_ unknown
132041_32562 unknown
132044_32606 unknown
132053_32624 unknown
132055_32621 unknown
132067_32600 unknown
132070_32598 unknown
132072_32603 unknown
132081_32576 unknown
132086_32575 unknown
132108_32570 unknown
7421_ carolina
649248_ unknown
649317_ unknown
132069_32610 unknown
649315_ unknown
649321_ unknown
132042_32561 unknown
132043_32565 unknown
132052_32625 unknown
132075_32592 unknown
132085_32566 unknown
132109_32569 unknown
132110_32558 unknown
649279_ unknown
649342_ unknown
649247_ unknown
649348_ unknown
649313_ unknown
649316_ unknown
649233_ unknown
132048_32619 unknown
132066_32599 unknown
132088_32559 unknown
649344_ unknown
649253_ unknown
132054_32618 unknown
132047_32563 unknown
132049_32602 unknown
132050_32620 unknown
649254_ unknown
132057_32623 unknown
649231_ unknown
649346_ unknown
649256_ unknown
132082_32577 unknown
132112_32581 unknown
132106_32541 unknown
649249_ unknown
132065_32601 unknown
649229_ unknown
132056_32622 unknown
132101_32588 unknown
132104_32579 unknown
132045_32605 unknown
649240_ unknown
132099_32584 unknown
132111_32557 unknown
132113_32536 unknown
649237_ unknown
132117_32583 unknown
132121_32546 unknown
649322_ unknown
132115_32543 unknown
132071_32604 unknown
132116_32537 unknown
132098_32590 unknown
132083_32572 unknown
649314_ unknown
132124_32552 unknown
649257_ unknown
649234_ unknown
132123_32551 unknown
132127_32549 unknown
132119_32554 unknown
649323_ unknown
132129_32540 unknown
649232_ unknown
649324_ unknown
649363_ unknown
649251_ unknown
132103_32578 unknown
649236_ unknown
649230_ unknown
132078_32595 unknown
132096_32574 unknown
132105_32542 unknown
132114_32534 unknown
132132_32539 unknown
132102_32567 unknown
132130_32535 unknown
132087_32556 unknown
649318_ unknown
132118_32582 unknown
132125_32548 unknown
132076_32608 unknown
132100_32587 unknown
649356_ unknown
132126_32547 unknown
132107_32580 unknown
649365_ unknown
649362_ unknown
132122_32538 unknown
132131_32597 unknown
649239_ unknown
132095_32571 unknown
132094_32555 unknown
132077_32607 unknown
9898_ blackcapped
649364_ unknown
132073_32593 unknown
649357_ unknown
132080_32594 unknown
132093_32568 unknown
132063_32614 unknown
649327_ unknown
649353_ unknown
132060_32615 unknown
6281_ blackcapped
649361_ unknown
132079_32596 unknown
649358_ unknown
649355_ unknown
649255_ unknown
649330_ unknown
649359_ unknown
132090_32586 unknown
649325_ unknown
649328_ unknown
649252_ unknown
132061_32612 unknown
132062_32611 unknown
132074_32591 unknown
649329_ unknown
649319_ unknown
649320_ unknown
649235_ unknown
649238_ unknown
132084_32573 unknown
132128_32550 unknown
3474_ blackcapped
649332_ unknown
649333_ unknown
649354_ unknown
649326_ unknown
649334_ unknown
649360_ unknown
649331_ unknown
649352_ unknown
649241_ unknown
132058_32617 unknown
132059_32616 unknown
132064_32613 unknown
649258_ unknown
649259_ unknown

# unknown:1 blackcapped:1 carolina:1

# Sbatch script for running ipyrad
#!/bin/bash -e

#SBATCH -A uoo00105 
#SBATCH -J ipyrad
#SBATCH --ntasks 1
#SBATCH -c 36
#SBATCH -t 48:00:00
#SBATCH --mem=105G
#SBATCH -D /nesi/nobackup/uoo00105/chickadees 
#SBATCH --mail-type=ALL
#SBATCH --mail-user=alana.alexander@otago.ac.nz
#SBATCH -N 1
#SBATCH --hint=nomultithread
#SBATCH --partition=large

source /nesi/nobackup/uoo00105/chickadees/bin/miniconda3/bin/activate

ipyrad -p params-chickadee_ref.txt -s 1234567 -c 36


# Obtained the following error during step 7
Encountered an Error.
Message: KeyError: 86

