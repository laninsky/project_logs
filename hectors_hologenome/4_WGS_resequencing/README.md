## Extracting and cleaning data
Barcode file available at [../barcode_file.txt](barcode_file.txt)
```
# Extracting the data
tar -zvxf 201126_FD09251656.tar.gz

# Trimming adaptors and poly-G with fastp

#!/bin/bash -e 
#SBATCH -A uoo02423
#SBATCH -J fastp
#SBATCH -n 1
#SBATCH -c 8 
#SBATCH -t 5:00:00
#SBATCH --mem=10G
#SBATCH -D /nesi/nobackup/uoo02423/WGS/201126_FD09251656/inputFastq
#SBATCH -N 1
#SBATCH --array=2-55

# The dimension of the array is the length of the barcode file
# Excluding the first header line
# lineno=`wc -l $barcode_file_path | awk '{print $1}'`

module load fastp/0.20.0-GCCcore-7.4.0
barcode_file_path=/nesi/nobackup/uoo02423/WGS/barcode_file.txt

line_contents=`head -n ${SLURM_ARRAY_TASK_ID} $barcode_file_path | tail -n 1`;
F_barcode=`echo $line_contents | awk '{print $3}'`;
R_barcode=`echo $line_contents | awk '{print $5}'`;
sample_name=`echo $line_contents | awk '{print $1}'`;
fastp -i *_1_*${F_barcode}-${R_barcode}*R1.fastq.gz -o ${sample_name}_1_trimmed_R1.fastq.gz -I *_1_*${F_barcode}-${R_barcode}*R2.fastq.gz -O ${sample_name}_1_trimmed_R2.fastq.gz -a AGATCGGAAGAGCACACGTCTGAACTCCAGTCA --adapter_sequence_r2 AGATCGGAAGAGCGTCGTGTAGGGAAAGAGTGT --trim_poly_g --thread 16;
fastp -i *_2_*${F_barcode}-${R_barcode}*R1.fastq.gz -o ${sample_name}_2_trimmed_R1.fastq.gz -I *_2_*${F_barcode}-${R_barcode}*R2.fastq.gz -O ${sample_name}_2_trimmed_R2.fastq.gz -a AGATCGGAAGAGCACACGTCTGAACTCCAGTCA --adapter_sequence_r2 AGATCGGAAGAGCGTCGTGTAGGGAAAGAGTGT --trim_poly_g --thread 16;
cat ${sample_name}_1_trimmed_R1.fastq.gz ${sample_name}_2_trimmed_R1.fastq.gz >> ${sample_name}_combined_trimmed_R1.fastq.gz;
cat ${sample_name}_1_trimmed_R2.fastq.gz ${sample_name}_2_trimmed_R2.fastq.gz >> ${sample_name}_combined_trimmed_R2.fastq.gz;
```

## Using ORTHOSKIM to extract mtDNA contigs from the WGS data
### Installation following instructions at https://github.com/cpouchon/ORTHOSKIM
```
# Loading conda for the conda install
module load Miniconda3/4.10.3

# Cloning the ORTHOSKIM repo 
wget https://github.com/cpouchon/ORTHOSKIM/archive/master.zip

# Unzipping the ORTHOSKIM repo and navigating into this directory
unzip master.zip
cd ./ORTHOSKIM-master/

# Creating the ORTHOSKIM conda environment and activating it
conda env create --prefix /nesi/nobackup/uoo02423/bin/ORTHOSKIM --file orthoskim-env.yml
conda activate /nesi/nobackup/uoo02423/bin/ORTHOSKIM
```
### Getting input files together following instructions at https://github.com/cpouchon/ORTHOSKIM
Modified [../config_orthoskim.txt](config_orthoskim.txt) following instructions

```
/nesi/nobackup/uoo02423/bin/ORTHOSKIM-master/orthoskim
```
