## 2. Metabarcoding
This sub-sub-repository contains analyses relating to the metabarcoding component of:
“Hologenomics for conservation: a first test of utility.”  
We experimented with a few different approaches for analysing our datasets.

Following guide at: https://otagomohio.github.io/workshops/eDNA_Metabarcoding.html

### Pear/OBITools/VSEARCH/BLAST
The first approach involved PEAR for merging reads, OBiTools for demultiplexing, USEARCH for cleaning the data and clustering, and a homemade BLAST script to get taxonomic data. I'm subbing in VSEARCH instead of USEARCH. I did this on NeSI.

#### Installation/modules required
```
module load VSEARCH/2.4.3-gimkl-2017a #instead of USEARCH
```
After signing up online at https://www.h-its.org/downloads/pear-academic/ downloaded pear files to local computer and then uploaded them to NeSI via scp
```
tar -zvxf pear-0.9.11-linux-x86_64.tar.gz
export PATH=/nesi/nobackup/uoo02423/bin/pear/pear-0.9.11-linux-x86_64/bin:$PATH # path to pear
```
Then followed instructions at https://pythonhosted.org/OBITools/welcome.html#installing-the-obitools for installing OBITools
```
module load GCCcore/7.4.0 # OBITools dependency
wget https://repo.anaconda.com/miniconda/Miniconda2-latest-Linux-x86_64.sh # obtained miniconda installer
bash Miniconda2-latest-Linux-x86_64.sh # installed miniconda2
vi /home/alana.alexander/.bashrc # deleted the bit it added to my bashrc file (so it doesn't conflict with other projects)
export PATH=/nesi/nobackup/uoo02423/bin/miniconda2/bin:$PATH # allowing it to find the minconda2 python
```
Install obitools using the conda channel because the python get-obitools recommended on the webpage will not work on NeSI.

```
conda install -c bioconda obitools
```

Gert-Jan demultiplexed and merged my reads via PEAR for me with the following code https://otagomohio.github.io/workshops/eDNA_Metabarcoding.html#chapter_2:_assembling_paired_reads  
https://otagomohio.github.io/workshops/eDNA_Metabarcoding.html#chapter_3:_demultiplexing  


#### Submission script for obigrep step
Discarding all sequences larger than 393 bp (10 bp larger than our expected contig size)
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

module load VSEARCH/2.4.3-gimkl-2017a
export PATH=/nesi/nobackup/uoo02423/bin/pear/pear-0.9.11-linux-x86_64/bin:$PATH
module load GCCcore/7.4.0
export PATH=/nesi/nobackup/uoo02423/bin/miniconda2/bin:$PATH

srun obigrep -L 393 Alana_assigned_combined.fastq > Alana_assigned_combined_L393.fastq
```
