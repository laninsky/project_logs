# Cutadapt and fastqc
Following running fastqc on the original files, and then transferring these to a "fastqc" folder, the following code was run in each separate fastq location (as some of the files were from re-runs and had identical sample names).

```
#!/bin/bash -e
#SBATCH -A uoo03004 # account
#SBATCH -J cut1 # This is the same as job name we used previously
#SBATCH --time 2:00:00 # We are allowing a total of two hours
#SBATCH -N 1 # This is the same as number of tasks - just one because we aren't using fancy MPI parallelnes
#SBATCH -c 8 # This is the same as the number of cpus = we are asking for 8
#SBATCH -n 1 # We are asking for one node (the cpus are grouped into nodes, and most things we work on will need to be restricted to one node)
#SBATCH --mem=24G # Total amount of memory. Generally the formula is 3GB/cpu for optimal "billing"
#SBATCH --partition=large # Same one as last time
#SBATCH --mail-user=joanne.gillum@otago.ac.nz # email address
#SBATCH --mail-type=ALL
#SBATCH --output cutadapt_mar2020.%j.out
#SBATCH --error cutadapt_mar2020.%j.err
#SBATCH --chdir=/nesi/nobackup/uoo03004/raw_data/nzgl_raw-2015-07-13/150701_M00933_0099_000000000-D0H15/fQsequences # pathway to where all the fastq files are

## load modules
module load cutadapt/2.3-gimkl-2018b-Python-3.7.3
module load FastQC/0.11.7

# This script depends on a key.txt file being available in the folder where the fastq.gz files are located
# (i.e. in the directory listed in the sbatch commands above). This key.txt file should have the R1 filename
# followed by a space and then the sample name, and on the next line the R2 filenmae followed by a space and
# then the sample name.

no_lines_in_key=`wc -l key.txt | awk '{print $1}'` # we are creating a variable that saves the number of lines in key.txt

# This for loop is going to step through every second line in our key.txt file
# i.e. is going to do cutadapt on a per sample basis, not a per file basis
for line_no in `seq 1 2 $no_lines_in_key`;
        # The following variable captures the contents of the key.txt file for the sample we are up to
        do sample_name=`head -n $line_no key.txt | tail -n 1 | awk '{print $2}'`;
        R1_name=`head -n $line_no key.txt | tail -n 1 | awk '{print $1}'`;
        R2_name=`head -n $(expr $line_no + 1) key.txt | tail -n 1 | awk '{print $1}'`;
        cutadapt -j 8 -a AGATCGGAAGAGCACACGTCTGAACTCCAGTCA \
                -A AGATCGGAAGAGCGTCGTGTAGGGAAAGAGTGT \
                -m 100 \
                -q 10 \
                -o /nesi/nobackup/uoo03004/trimmed_data/${sample_name}.trimmed.R1.fastq.gz \
                -p /nesi/nobackup/uoo03004/trimmed_data/${sample_name}.trimmed.R2.fastq.gz \
                ${R1_name} \
                ${R2_name}

        fastqc -t 8 /nesi/nobackup/uoo03004/trimmed_data/${sample_name}.trimmed.*
```

# Double checking fastqc (in R)
In a "fastqc" folder with all your fastqc.zip folders for all your samples, both pre- and post- adaptor removal/quality trimming etc (important to run it in here because otherwise it deletes all your files!), the following bash code extracts the summary.txt file from each fastq file, and appends it into fastqc_summary.txt.
```
for i in *.zip;
do foldername=`echo $i | sed 's/.zip//g'`;
unzip $i;
cat $foldername/summary.txt >> fastqc_summary.txt;
rm -rf $foldername;
done;
```

`fastqc_summary.R` (in this repository) can them be used to compare before and after trimming fastqc results. It requires the fastqc_summary.txt as well as a key file (called `key.txt`) with all of the original data in it (concatenate the key files created in the previous cutadapt step).

Example key.txt file:
```
D76JK-5027-01-0-1_S1_L001_R1_001.fastq.gz 89534
D76JK-5027-01-0-1_S1_L001_R2_001.fastq.gz 89534
D76JK-5027-02-0-1_S2_L001_R1_001.fastq.gz 94149
D76JK-5027-02-0-1_S2_L001_R2_001.fastq.gz 94149
D76JK-5027-03-0-1_S3_L001_R1_001.fastq.gz 94354
D76JK-5027-03-0-1_S3_L001_R2_001.fastq.gz 94354
D76JK-5027-04-0-1_S4_L001_R1_001.fastq.gz 94841
D76JK-5027-04-0-1_S4_L001_R2_001.fastq.gz 94841
```
