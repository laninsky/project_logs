## 2. Metabarcoding
This sub-sub-repository contains analyses relating to the metabarcoding component of:
“Hologenomics for conservation: a first test of utility.”  
We experimented with a few different approaches for analysing our datasets.

Following guide at: https://otagomohio.github.io/workshops/eDNA_Metabarcoding.html

### Pear/OBITools/VSEARCH/BLAST
The first approach involved PEAR for merging reads, OBiTools for demultiplexing, USEARCH for cleaning the data and clustering, and a homemade BLAST script to get taxonomic data. I'm subbing in VSEARCH instead of USEARCH. I did this on NeSI.

#### Installation/modules required
```
module load USEARCH/9.2.64-i86linux32
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

#### [Chapter 4: Quality filtering](https://otagomohio.github.io/workshops/eDNA_Metabarcoding.html#chapter_4:_quality_filtering) with USEARCH

Going to get a list of fastq files together so that can run the next steps as an array
```
ls *.fastq > filelist.txt
```

sbatch script for USEARCH steps
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

module load USEARCH/9.2.64-i86linux32

fq=`head -n ${SLURM_ARRAY_TASK_ID}  filelist.txt | tail -n 1`

usearch -fastq_filter $fq -fastq_maxee 1 -fastq_minlen 353 -fastq_maxns 0 -relabel $fq. -fastaout $fq.fasta -fastqout $fq.fastq
```


```
cat *.fastq.fastq > pooled.fastq
cat *.fasta > pooled.fasta
tr ‘[:lower:]’ ‘[:upper:]’ < pooled.fasta > pooled_upper.fasta
```
I will eventually need to come up with some kind of rule of thumb for this cut-off
```
usearch -fastx_uniques pooled_upper.fasta -fastaout uniques_10.fasta -relabel Uniq -sizeout -minuniquesize 10
```
denoise = 100% (rather than trying to cluster into OTU at lower % threshold)
Qiime = ASV (actual/amplicon sequence variance)
However ZOTU (zero-radius operational taxonomic unit) was used as a term first.



