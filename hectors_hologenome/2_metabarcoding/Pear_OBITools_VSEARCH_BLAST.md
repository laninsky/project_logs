### Pear/OBITools/VSEARCH/BLAST
The first approach involved PEAR for merging reads, OBiTools for demultiplexing, USEARCH for cleaning the data and clustering, and a homemade BLAST script to get taxonomic data. I'm subbing in VSEARCH instead of USEARCH for a few of the steps. I did this on NeSI following the guidelines at: https://otagomohio.github.io/workshops/eDNA_Metabarcoding.html

#### Installation/modules required
```
module load USEARCH/11.0.667-i86linux32
```
After signing up online at https://www.h-its.org/downloads/pear-academic/ downloaded pear files to local computer and then uploaded them to NeSI via scp
```
tar -zvxf pear-0.9.11-linux-x86_64.tar.gz
export PATH=/nesi/nobackup/uoo02423/bin/pear/pear-0.9.11-linux-x86_64/bin:$PATH # path to pear
```
Installed obitools using the conda channel because the python get-obitools recommended on the webpage will not work on NeSI.
```
conda install -c bioconda obitools
```

Gert-Jan demultiplexed and merged my reads via PEAR for me with the following code https://otagomohio.github.io/workshops/eDNA_Metabarcoding.html#chapter_2:_assembling_paired_reads  
https://otagomohio.github.io/workshops/eDNA_Metabarcoding.html#chapter_3:_demultiplexing  

The input read file size was 8.1GB (for calculating memory use when rerunning this on other samples)  

I then took these samples through the obigrep and onwards steps at:  
https://otagomohio.github.io/workshops/eDNA_Metabarcoding.html#chapter_3:_demultiplexing  

#### Submission script for obigrep steps
Discarding all sequences larger than 393 bp (10 bp larger than our expected contig size). This step took about 40 minutes, and doesn't appear to be able to be multithreaded (and can't make use of hyperthreading) based on a test run. Used 8.63MB of RAM.
```
#!/bin/bash -e 
#SBATCH -A uoo02423
#SBATCH -J obigrep
#SBATCH -n 1
#SBATCH -c 1 
#SBATCH -t 1:00:00
#SBATCH --mem=3G
#SBATCH -D /nesi/nobackup/uoo02423/hectors/pilot_water_eDNA 
#SBATCH -N 1
#SBATCH --hint=nomultithread

module load GCCcore/7.4.0
export PATH=/nesi/nobackup/uoo02423/bin/miniconda2/bin:$PATH

srun obigrep -L 393 Alana_assigned_combined.fastq > Alana_assigned_combined_L393.fastq
```

Same sbatch parameters for obiannotate: also took about 40 minutes. Used 8.67MB of RAM.
```
srun obiannotate -k sample Alana_assigned_combined_L393.fastq > Alana_assigned_combined_L393_annotated.fastq
```

Same sbatch parameters for obisplit: also took about 15 minutes. Used 8.21MB of RAM.
```
obisplit -t sample Alana_assigned_combined_L393_annotated.fastq
```

To prepare for [Chapter 4: Quality filtering](https://otagomohio.github.io/workshops/eDNA_Metabarcoding.html#chapter_4:_quality_filtering) copied files into a new directory called QC
```
mkdir QC
mv *.fastq QC
mv QC/Alana* ./
cd QC
```

#### [Chapter 4: Quality filtering](https://otagomohio.github.io/workshops/eDNA_Metabarcoding.html#chapter_4:_quality_filtering) with USEARCH/VSEARCH

Going to get a list of fastq files together so that can run the next steps as an array
```
ls *.fastq > filelist.txt
```

sbatch script for USEARCH steps. This was super quick (e.g. < 1 min), and had very low memory overheads (e.g. 30GB was definitely overkill).
```
#!/bin/bash -e 
#SBATCH -A uoo02423
#SBATCH -J usearch
#SBATCH -n 1
#SBATCH -c 10 
#SBATCH -t 1:00:00
#SBATCH --mem=30G
#SBATCH -D /nesi/nobackup/uoo02423/hectors/pilot_water_eDNA/QC 
#SBATCH -N 1
#SBATCH --array=1-42

module load USEARCH/11.0.667-i86linux32

fq=`head -n ${SLURM_ARRAY_TASK_ID}  filelist.txt | tail -n 1`

usearch -fastq_filter $fq -fastq_maxee 1 -fastq_minlen 353 -fastq_maxns 0 -relabel $fq. -fastaout $fq.fasta -fastqout $fq.fastq
```
Samples had ~93-95% reads pass filter. Following this step pooled sequences together

```
cat *.fastq.fastq > pooled.fastq
cat *.fasta > pooled.fasta
tr ‘[:lower:]’ ‘[:upper:]’ < pooled.fasta > pooled_upper.fasta
```

For the next step I used a cut-off of 10. I could not use USEARCH for this step because I hit the maximum memory threshold. I used VSEARCH instead. I would like to experiment with how a singleton cutoff would potentially affect results and potentially will eventually need to come up with some kind of rule of thumb for this cut-off based on depth distribution. Took less than a minute and didn't come close to touching even a GB of RAM.

```
#!/bin/bash -e 
#SBATCH -A uoo02423
#SBATCH -J vsearch
#SBATCH -n 1
#SBATCH -c 10 
#SBATCH -t 1:00:00
#SBATCH --mem=30G
#SBATCH -D /nesi/nobackup/uoo02423/hectors/pilot_water_eDNA/QC 
#SBATCH -N 1

module load VSEARCH/2.4.3-gimkl-2017a

# usearch -fastx_uniques pooled_upper.fasta -fastaout uniques_10.fasta -relabel Uniq -sizeout -minuniquesize 10
vsearch --derep_fulllength pooled_upper.fasta --output uniques_10.fasta --relabel Uniq --sizeout --minuniquesize 10 --threads 10
```
Output ranges from 353 to 393 bp, which is a little concerning because this product is meant to range between 363-383 bp. Off-target sequence? I guess we'll find out...high number of clusters discarded at this threshold too.
```
vsearch v2.4.3_linux_x86_64, 125.8GB RAM, 72 cores
https://github.com/torognes/vsearch

Reading file pooled_upper.fasta 100%
2459366620 nt in 6609403 seqs, min 353, max 393, avg 372
Dereplicating 100%
Sorting 100%
1845977 unique sequences, avg cluster 3.6, median 1, max 134523
Writing output file 100%
44635 uniques written, 1801342 clusters discarded (97.6%)
```

#### [Chapter 5: Denoising or clustering](https://otagomohio.github.io/workshops/eDNA_Metabarcoding.html#chapter_5:_denoising_or_clustering) with VSEARCH
(seeing as we hit the usearch memory limit in the last step, going to stick with vsearch)  

Denoise = cluster at 100% (rather than trying to cluster into OTU at lower % threshold). Qiime referse to these as ASV (actual/amplicon sequence variance), however ZOTU (zero-radius operational taxonomic unit) was used as a term first so GJ recommends sticking with this term. In this first step, sorting our remaining sequences by depth (size). Took 1s, and < 1GB of RAM.
```
#!/bin/bash -e 
#SBATCH -A uoo02423
#SBATCH -J vsearch
#SBATCH -n 1
#SBATCH -c 10 
#SBATCH -t 1:00:00
#SBATCH --mem=30G
#SBATCH -D /nesi/nobackup/uoo02423/hectors/pilot_water_eDNA/QC 
#SBATCH -N 1

module load VSEARCH/2.4.3-gimkl-2017a
vsearch --sortbysize uniques_10.fasta --output uniques_10_sorted.fasta --threads 10
```
output:
```
vsearch v2.4.3_linux_x86_64, 125.8GB RAM, 72 cores
https://github.com/torognes/vsearch

Reading file uniques_10.fasta 100%
16609178 nt in 44635 seqs, min 353, max 393, avg 372
Getting sizes 100%
Sorting 100%
Median abundance: 19
Writing output 100%
```

Next step is to denoise the dataset. Switching back to usearch (and crossing my fingers) on this one because I'm not entirely sure what the commands should be in vsearch. Took 20s and used 75MB of RAM.
```
#!/bin/bash -e 
#SBATCH -A uoo02423
#SBATCH -J usearch
#SBATCH -n 1
#SBATCH -c 10 
#SBATCH -t 1:00:00
#SBATCH --mem=30G
#SBATCH -D /nesi/nobackup/uoo02423/hectors/pilot_water_eDNA/QC 
#SBATCH -N 1

module load USEARCH/11.0.667-i86linux32
usearch -unoise3 uniques_10_sorted.fasta -zotus zotus_10.fasta -tabbedout unoise3_10.txt
```
output:
```
00:00 59Mb    100.0% Reading uniques_10_sorted.fasta
00:00 31Mb      0.0% 0 amplicons, 0 bad (size >= 134523)
WARNING: Shifted sequences detected

00:03 87Mb    100.0% 4748 amplicons, 1390066 bad (size >= 10)
01:39 94Mb    100.0% 3604 good, 1144 chimeras                
01:39 94Mb    100.0% Writing zotus 
```

Before we run the next step to generate the ZOTU table, we need to remove any hyphens from our sample names:
```
sed 's/-/_/g' pooled_upper.fasta > pooled_upper_changed.fasta
```
Then:
```
#!/bin/bash -e 
#SBATCH -A uoo02423
#SBATCH -J usearch
#SBATCH -n 1
#SBATCH -c 10 
#SBATCH -t 1:00:00
#SBATCH --mem=30G
#SBATCH -D /nesi/nobackup/uoo02423/hectors/pilot_water_eDNA/QC 
#SBATCH -N 1

module load USEARCH/11.0.667-i86linux32
usearch -otutab pooled_upper_changed.fasta -zotus zotus_10.fasta -otutabout zotutab_10_changed.txt
```
