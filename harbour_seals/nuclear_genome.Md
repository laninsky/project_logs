### Confirmed md5sums matched between transferred files/folders
```
#!/bin/bash -e

#SBATCH --account=uoo00106
#SBATCH --job-name=md5
#SBATCH -n 1
#SBATCH --cpus-per-task=1
#SBATCH --time=2:00:00
#SBATCH --mem-per-cpu=1G
#SBATCH --hint=nomultithread
#SBATCH --partition=large
#SBATCH -D /nesi/nobackup/uoo00106
#SBATCH --mail-type=ALL
#SBATCH --mail-user=alana.alexander@otago.ac.nz
#SBATCH -N 1

md5sum * >> md5sum.log
```

### Indexed reference and made 'trimmed' directory
In the working directory, made a trimmed directory for the raw reads to be moved into once trimmed, and indexed the reference genome (northern fur seal)
```
#!/bin/bash -e

#SBATCH --account=uoo00106
#SBATCH --job-name=sample1
#SBATCH -n 1
#SBATCH --cpus-per-task=16
#SBATCH --time=2:00:00
#SBATCH --mem-per-cpu=1G
#SBATCH --hint=nomultithread
#SBATCH --partition=large
#SBATCH -D /nesi/nobackup/uoo00106/uploaded
#SBATCH --mail-type=ALL
#SBATCH --mail-user=alana.alexander@otago.ac.nz
#SBATCH -N 1

module load Bowtie2/2.3.2-gimkl-2017a

bowtie2-build --threads 16 GCA_003265705.1_ASM326570v1_genomic.fna northern_fur_seal

mkdir trimmed
```

### Trimming raw reads and assembling them against the northern fur seal reference (example for Sample_19-D135)
Ran cutadapt on raw reads, used bowtie2 to assemble them to northern fur seal reference, ran fastqc to compare the trimmed reads to the raw reads, converted \*.sam to \*.bam.
```
#!/bin/bash -e

#SBATCH --account=uoo02423
#SBATCH --job-name=Sample_19-D135
#SBATCH -n 1
#SBATCH --cpus-per-task=18
#SBATCH --time=24:00:00
#SBATCH --mem=52G
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

i=Sample_19-D135.zip;
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
To create a sample-specific \*.sh file for each \*zip based on Sample_19-D135.sh and then start these jobs off:
```
for i in *.zip; 
  do basename=`echo $i | sed 's/.zip//g'`;
  cp Sample_19-D135.sh $basename.sh;
  sed -i "s/Sample_19-D135/$basename/g" $basename.sh;
  sbatch $basename.sh;
done  
```
### Pulled out cutadapt stats to record them (in R)
First had to load the R module
```
module load R/3.5.1-gimkl-2017a
```
And then...
```
library(stringr)
files <- list.files(pattern=".cutadapt.log")

output <- NULL

for (i in files) {
  tempfile <- readLines(i)
  samplename <- gsub(".cutadapt.log","",i)
  tempoutput <- matrix(tempfile[grep("Command line parameters",tempfile)],nrow=1)
  if (length(unique(t(tempoutput)))<dim(tempoutput)[2]) {
    print(paste(samplename," has duplicate cutadapt runs so will need to be processed manually",sep=""))
    next
  }    
  tempoutput <- rbind(tempoutput,tempfile[grep("Read 1 with adapter",tempfile)])
  tempoutput <- rbind(tempoutput,tempfile[grep("Read 2 with adapter",tempfile)])
  tempoutput <- rbind(tempoutput,tempfile[grep("Pairs that were too short",tempfile)])
  tempoutput <- rbind(tempoutput,tempfile[grep("Pairs written \\(passing filters\\)",tempfile)])
  tempoutput <- rbind(tempoutput,tempfile[grep("Total basepairs processed",tempfile)])
  tempoutput <- rbind(tempoutput,tempfile[grep("Total basepairs processed",tempfile) + 1])
  tempoutput <- rbind(tempoutput,tempfile[grep("Total basepairs processed",tempfile) + 2])
  tempoutput <- rbind(tempoutput,tempfile[grep("Quality-trimmed",tempfile)])
  tempoutput <- rbind(tempoutput,tempfile[grep("Quality-trimmed",tempfile) + 1])
  tempoutput <- rbind(tempoutput,tempfile[grep("Quality-trimmed",tempfile) + 2]) 
  tempoutput <- rbind(tempoutput,tempfile[grep("Total written \\(filtered\\)",tempfile)])
  tempoutput <- rbind(tempoutput,tempfile[grep("Total written \\(filtered\\)",tempfile) + 1])
  tempoutput <- rbind(tempoutput,tempfile[grep("Total written \\(filtered\\)",tempfile) + 2]) 
  tempoutput <- rbind(tempoutput,tempfile[grep("=== First read: Adapter 1 ===",tempfile) + 2]) 
  tempoutput <- rbind(tempoutput,tempfile[grep("=== First read: Adapter 1 ===",tempfile) + 8]) 
  tempoutput <- rbind(tempoutput,tempfile[grep("=== First read: Adapter 1 ===",tempfile) + 9]) 
  tempoutput <- rbind(tempoutput,tempfile[grep("=== First read: Adapter 1 ===",tempfile) + 10]) 
  tempoutput <- rbind(tempoutput,tempfile[grep("=== First read: Adapter 1 ===",tempfile) + 11]) 
  tempoutput <- rbind(tempoutput,tempfile[grep("=== First read: Adapter 1 ===",tempfile) + 12])
  tempoutput <- rbind(tempoutput,tempfile[grep("=== Second read: Adapter 2 ===",tempfile) + 2]) 
  tempoutput <- rbind(tempoutput,tempfile[grep("=== Second read: Adapter 2 ===",tempfile) + 8]) 
  tempoutput <- rbind(tempoutput,tempfile[grep("=== Second read: Adapter 2 ===",tempfile) + 9]) 
  tempoutput <- rbind(tempoutput,tempfile[grep("=== Second read: Adapter 2 ===",tempfile) + 10]) 
  tempoutput <- rbind(tempoutput,tempfile[grep("=== Second read: Adapter 2 ===",tempfile) + 11]) 
  tempoutput <- rbind(tempoutput,tempfile[grep("=== Second read: Adapter 2 ===",tempfile) + 12])
  
  header <- rep(samplename,dim(tempoutput)[2])
  
  tempoutput <- rbind(header,tempoutput)
  
  output <- cbind(output,tempoutput)
}  
  
finalheader <- c("sample_name","seq_file_name","Read_1_with_adapter","Read_2_with_adapter","Pairs_that_were_too_short","Pairs_written_passing_filters"

output[2,] <- gsub(" ",",",gsub(".*trimmed.fastq.gz ","",gsub(" -q .*","",output[2,])))
output[3,] <- gsub("  Read 1 with adapter: *","",output[3,])
output[4,] <- gsub("  Read 2 with adapter: *","",output[4,]) 
output[5,] <- gsub("Pairs that were too short: *","",output[5,])
output[6,] <- gsub("Pairs written \\(passing filters\\): *","",output[6,])


  tempfile <- tempfile[(grep(" reads; of these:",tempfile)):(grep("overall alignment rate",tempfile))]

  tempfile <- str_trim(tempfile, side = "left")
  tempfile <- strsplit(tempfile," ")
  tempfile <- unlist(lapply(1:length(tempfile),function(x) { tempfile[[x]][1] }))
  tempfile <- tempfile[c(-6,-7,-9)]
  output[(i+1),] <- c(samplename,tempfile)
}
  
write.table(output,"bowtie_ref.txt",quote=FALSE,row.name=FALSE,col.name=FALSE)  


```

### Pulled out reference mapping stats to record them (in R)
First had to load the R module
```
module load R/3.5.1-gimkl-2017a
```
And then...
```
library(stringr)
files <- list.files(pattern=".ref_log.txt")
                
output <- matrix("",nrow=(length(files)+1),ncol=13)

output[1,] <- c("File_name","Reads","Number_paired","aligned_concordantly_0_times","aligned_concordantly_exactly_1_time","aligned_concordantly_>1_times","aligned_discordantly_1_time","aligned_0_times_concordantly_or_discordantly","aligned_0_times_concordantly_or_discordantly_single_read_count","single_read_aligned_0_times", "aligned_exactly_1_time", "aligned_>1_times", "overall_alignment_rate")

for (i in 1:length(files)) {
  tempfile <- readLines(files[i])
  tempfile <- tempfile[(grep(" reads; of these:",tempfile)):(grep("overall alignment rate",tempfile))]
  samplename <- gsub(".ref_log.txt","",files[i])
  tempfile <- str_trim(tempfile, side = "left")
  tempfile <- strsplit(tempfile," ")
  tempfile <- unlist(lapply(1:length(tempfile),function(x) { tempfile[[x]][1] }))
  tempfile <- tempfile[c(-6,-7,-9)]
  output[(i+1),] <- c(samplename,tempfile)
}
  
write.table(output,"bowtie_ref.txt",quote=FALSE,row.name=FALSE,col.name=FALSE)  
```
