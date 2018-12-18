### Indexed reference and made 'trimmed' directory
In the working directory, made a trimmed directory for the raw reads to be moved into once trimmed, and indexed the reference genome (northern fur seal)
```
module load bowtie2/2.3.4.2
module load fastqc/0.11.5
module load samtools/1.7

bowtie2-build GCA_003265705.1_ASM326570v1_genomic.fna northern_fur_seal

mkdir trimmed
```

### Trimming raw reads and assembling them against the northern fur seal reference (example for Sample_12-D099)
Sed'd the name to substitute in other samples' names. Ran cutadapt on raw reads, used bowtie2 to assemble them to northern fur seal reference, ran fastqc to compare the trimmed reads to the raw reads, converted \*.sam to \*.bam.
```
#!/bin/bash -e

#SBATCH --account=uoo02423
#SBATCH --job-name=Sample_12-D099
#SBATCH -n 1
#SBATCH --cpus-per-task=16
#SBATCH --time=24:00:00
#SBATCH --mem-per-cpu=1G
#SBATCH --hint=nomultithread
#SBATCH --partition=large
#SBATCH -D /nesi/nobackup/uoo02423/uploaded 
#SBATCH --mail-type=ALL
#SBATCH --mail-user=alana.alexander@otago.ac.nz
#SBATCH -N 1

module load SAMtools/1.8-gimkl-2017a
module load cutadapt/1.16-gimkl-2017a-Python-3.6.3
module load Bowtie2/2.3.2-gimkl-2017a
module load FastQC/0.11.7

i=Sample_12-D099.zip;
basename=`echo $i | sed 's/.zip//g'`;
echo $basename >> $i.ref_log.txt
echo $basename >> $i.bam_log.txt
unzip $i;
cd $basename;
for j in *R1*.fastq.gz;
do trimname1=`echo $j | sed 's/.fastq/.trimmed.fastq/g'`;
trimname2=`echo $trimname1 | sed 's/R1/R2/g'`;
reverse=`echo $j | sed 's/R1/R2/g'`;
cutadapt -j 16 -a AGATCGGAAGAGC -A AGATCGGAAGAGC -o $trimname1 -p $trimname2 $j $reverse -q 5,15 -m 25 >> ../$i.cutadapt.log;
done;
R1=`echo *R1*trimmed.fastq.gz | sed 's/ /,/g'`;
R2=`echo *R2*trimmed.fastq.gz | sed 's/ /,/g'`;
bowtie2 --fr -p 16 -x /nesi/nobackup/uoo02423/uploaded/northern_fur_seal -1 $R1 -2 $R2 -S /nesi/nobackup/uoo02423/uploaded/$basename.sam &>> /nesi/nobackup/uoo02423/uploaded/$i.ref_log.txt
cd /nesi/nobackup/uoo02423/uploaded;
fastqc $basename/*fastq.gz -o trimmed -t 16;
mv $basename/*trimmed* trimmed;
samtools view --threads 16 -bS -o $basename.bam $basename.sam &>> $i.bam_log.txt;
rm -rf $basename.sam; 
rm -rf $basename;
```
