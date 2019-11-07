## Steps
[Confirmed md5sums matched between transferred files/folders](https://github.com/laninsky/project_logs/blob/master/harbour_seals/nuclear_genome.Md#confirmed-md5sums-matched-between-transferred-filesfolders)  
[Indexed reference](https://github.com/laninsky/project_logs/blob/master/harbour_seals/nuclear_genome.Md#indexed-reference)  
[Trimming raw reads and assembling them against the harbour seal reference (example for Sample_1-D079)](https://github.com/laninsky/project_logs/blob/master/harbour_seals/nuclear_genome.Md#trimming-raw-reads-and-assembling-them-against-the-harbour-seal-reference-example-for-sample_1-d079)  
[Double checking fastqc (in R)](https://github.com/laninsky/project_logs/blob/master/harbour_seals/nuclear_genome.Md#double-checking-fastqc-in-r)  
[Pulled out reference mapping stats to record them (in R)](https://github.com/laninsky/project_logs/blob/master/harbour_seals/nuclear_genome.Md#pulled-out-reference-mapping-stats-to-record-them-in-r)  
[Estimated individual heterozygosity using genomescope and jellyfish](https://github.com/laninsky/project_logs/blob/master/harbour_seals/nuclear_genome.Md#estimated-individual-heterozygosity-using-genomescope-and-jellyfish-example-for-sample_1-d079)

### Confirmed md5sums matched between transferred files/folders
```
#!/bin/bash -e

#SBATCH --account=uoo02423
#SBATCH --job-name=md5
#SBATCH -n 1
#SBATCH --cpus-per-task=1
#SBATCH --time=2:00:00
#SBATCH --mem=3G
#SBATCH --hint=nomultithread
#SBATCH --partition=large
#SBATCH -D /nesi/nobackup/uoo02423/harbour_seal/zips 
#SBATCH --mail-type=ALL
#SBATCH --mail-user=alana.alexander@otago.ac.nz
#SBATCH -N 1

md5sum * >> md5sum.log
```

### Indexed reference
Grabbed the harbour seal reference from DNA Zoo via `wget https://www.dropbox.com/s/dqtlf4qcu0k7453/GSC_HSeal_1.0_HiC.fasta.gz?dl=0` and then renamed it by `mv GSC_HSeal_1.0_HiC.fasta.gz\?dl\=0 GSC_HSeal_1.0_HiC.fasta.gz`. In the working directory then indexed the reference genome (harbour seal)
```
#!/bin/bash -e

#SBATCH --account=uoo02423
#SBATCH --job-name=indexing_ref
#SBATCH -n 1
#SBATCH --cpus-per-task=18
#SBATCH --time=2:00:00
#SBATCH --mem=52G
#SBATCH --hint=nomultithread
#SBATCH --partition=large
#SBATCH -D /nesi/nobackup/uoo02423/harbour_seal
#SBATCH --mail-type=ALL
#SBATCH --mail-user=alana.alexander@otago.ac.nz
#SBATCH -N 1

module load Bowtie2/2.3.5-GCC-7.4.0

bowtie2-build --threads 18 GSC_HSeal_1.0_HiC.fasta.gz harbour_seal
```

### Trimming raw reads and assembling them against the harbour seal reference (example for Sample_1-D079)
Before running this script I created a `trimmed` and `sam` directory in the working directory (`/nesi/nobackup/uoo02423/harbour_seal`).

Ran cutadapt on raw reads, used bowtie2 to assemble them to the harbour seal reference, ran fastqc to compare the trimmed reads to the raw reads, converted \*.sam to \*.bam. This script is modified from original cutadapt [loop](https://github.com/laninsky/project_logs/blob/master/harbour_seals/unused_code/README.Md) because of the presence of adapter dimer. The bioinformatics troubleshooting for this is explained [here](https://github.com/laninsky/project_logs/blob/master/harbour_seals/unused_code/README.Md)

The first `-a` and `-A` option for forward and reverse reads respectively corresponds to the actual expected adapter sequence, the second corresponds to the adaptor dimer sequence. After confirming that this script worked for the first sample, I then used a for loop and sed to rename the script and replace Sample_1-D079.zip with the remaining samples (copying the script into zip file for the rename step and then copying the job submission scripts out again):
```
for i in *.zip; 
  do basename=`echo $i | sed 's/.zip//g'`;
  cp Sample_1-D079.sh $basename.sh;
  sed -i "s/Sample_1-D079/$basename/g" $basename.sh;
  sbatch $basename.sh;
done  
```
Example script for Sample_1-D079
```
#!/bin/bash -e

#SBATCH --account=uoo02423
#SBATCH --job-name=Sample_1-D079.zip
#SBATCH -n 1
#SBATCH --cpus-per-task=18
#SBATCH --time=24:00:00
#SBATCH --mem=52G
#SBATCH --hint=nomultithread
#SBATCH --partition=large
#SBATCH -D /nesi/nobackup/uoo02423/harbour_seal
#SBATCH --mail-type=ALL
#SBATCH --mail-user=alana.alexander@otago.ac.nz
#SBATCH -N 1

module load SAMtools/1.9-GCC-7.4.0
module load cutadapt/2.3-gimkl-2018b-Python-3.7.3
module load Bowtie2/2.3.5-GCC-7.4.0
module load FastQC/0.11.7

working_dir=/nesi/nobackup/uoo02423/harbour_seal
zipped_folder_location=/nesi/nobackup/uoo02423/harbour_seal/zips
scripts_folder=/nesi/nobackup/uoo02423/harbour_seal/scripts_logs
trimmed_folder=/nesi/nobackup/uoo02423/harbour_seal/trimmed
sam_folder=/nesi/nobackup/uoo02423/harbour_seal/sam
fastqc_folder=/nesi/nobackup/uoo02423/harbour_seal/fastqc

i=Sample_1-D079.zip;
basename=`echo $i | sed 's/.zip//g'`;
echo $basename >> $scripts_folder/$i.ref_log.txt
echo $basename >> $scripts_folder/$i.bam_log.txt
unzip $zipped_folder_location/$i;
cd $basename;
for j in *R1*.fastq.gz;
  do trimname1=`echo $j | sed 's/.fastq/.trimmed.fastq/g'`;
  trimname2=`echo $trimname1 | sed 's/R1/R2/g'`;
  reverse=`echo $j | sed 's/R1/R2/g'`;
  cutadapt -j 18 -a AGATCGGAAGAGCACACGTCTGAACTCCAGTCA -a ATCGGAAGAGCACACGTCTGAACTCCAGTCA -A AGATCGGAAGAGCGTCGTGTAGGGAAAGAGTGT -A ATCGGAAGAGCGTCGTGTAGGGAAAGAGTGT -o $trimname1 -p $trimname2 $j $reverse -q 5,15 -m 25 >> $scripts_folder/$i.cutadapt.log;
done;
R1=`echo *R1*trimmed.fastq.gz | sed 's/ /,/g'`;
R2=`echo *R2*trimmed.fastq.gz | sed 's/ /,/g'`;
bowtie2 --fr -p 18 -x /nesi/nobackup/uoo02423/harbour_seal/harbour_seal -1 $R1 -2 $R2 -S $sam_folder/$basename.sam &>> $scripts_folder/$i.ref_log.txt
fastqc *fastq.gz -o $fastqc_folder -t 18;
mv *trimmed* $trimmed_folder;
samtools view --threads 18 -bS -o $sam_folder/$basename.bam $sam_folder/$basename.sam &>> $scripts_folder/$i.bam_log.txt;
rm -rf $sam_folder/$basename.sam;
cd $working_dir;
rm -rf $basename;
```

For the slurm-output, I prepended them with sample name using the code below.
```
for i in slurm*.out; do samplename=`tail -n 1 $i | sed 's/Analysis complete for //g' | sed 's/_L00.*//g'`; mv $i $samplename.$i; done
mkdir scripts_logs/unzip_fastqc_slurm_out
mv *.out scripts_logs/unzip_fastqc_slurm_out 
```
Moved the submission scripts into the scripts_logs folder too, just to keep everything nicely organized
```
mkdir scripts_logs/unzip_trim_fastqc_asssemble_submission
mv *.sh scripts_logs/unzip_trim_fastqc_asssemble_submission
```
Inside scripts_logs, created a cutadapt folder for the cutadapt output
```
mkdir cutadapt_logs
mv *cutadapt.log cutadapt_logs
```
Inside scripts_logs, double checked that the \*.bam_log.txt files didn't contain any errors (via `less`) and then deleted them
```

```


### Double checking fastqc (in R)
In the fastqc folder (important to run it in here because otherwise it deletes all your files!), I ran the following bash code to extract the summary.txt file from each fastq file, and then [fastqc_summary.R](https://github.com/laninsky/project_logs/blob/master/harbour_seals/fastqc_summary.R) (in this repository) to compare before and after trimming fastqc results:
```
for i in *.zip;
do foldername=`echo $i | sed 's/.zip//g'`;
unzip $i;
cat $foldername/summary.txt >> fastqc_summary.txt;
rm -rf $foldername;
done;
```
Note: have to `module load R/3.6.1-gimkl-2018b` before running the R code

### Pulled out reference mapping stats to record them (in R)
First had to load the R module
```
module load R/3.5.1-gimkl-2017a
```
And then...
```
library(stringr)
library(tidyverse)
library(gridExtra)
library(grid)
library(ggplot2)
library(lattice)
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


output[,13] <- gsub("%","",output[,13],fixed=TRUE)
outputtibble <- tibble(output[2:(dim(output)[1]),1])

for (i in 2:dim(output)[2]) {
  outputtibble <- cbind(outputtibble,tibble(as.numeric(output[2:(dim(output)[1]),i])))  
}

names(outputtibble) <- output[1,]
outputtibble <- as_tibble(outputtibble)

corgraph <- ggpairs(outputtibble, cardinality_threshold=length(unique(outputtibble$File_name)),mapping=(aes(fill = File_name,color=File_name)),
                    columns=(names(outputtibble)[-1]),
                    upper=list(continuous="blank",combo="blank"),
                    lower=list(continuous=wrap("points",size=5,pch=21,alpha=0.5,color="black"),combo=wrap("facetdensity")),
                    diag=list(discrete="blankDiag",continuous=wrap("blankDiag",alpha=0.5)))+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))  + theme(legend.text=element_text(size=8))

# Then for every column (except the last one) in the ggpairs correlation graph
# Then for every column (except the last one) in the ggpairs correlation graph
for (i in 1:(corgraph$ncol-1)) {
  # Make a list, and populate the first item in the list with the column's variable self comparison
  # It's going to be in its own row (including a legend)
  plotlist <- list()
  plotlist[[1]] <- corgraph[(i+1),i]
  # For the remaining comparisons of that variable to others, if there isn't an odd number
  if ((corgraph$ncol-i-1) %% 2 == 0) {
    # Adding them in to the plot list
    for (j in (i+2):corgraph$ncol) {
      plotlist[[j-i]] <- corgraph[j,i] + theme(legend.position = "none")
    }
  } else {
    # Or else, add a blank element in first
    plotlist[[2]] <- grid.rect(gp = gpar(col = "white"))
    for (j in (i+2):corgraph$ncol) {
      plotlist[[j-i+1]] <- corgraph[j,i] + theme(legend.position = "none")
    }
  }
  # Creating the layout (first self-self plot with the legend in first row
  # Every other plot half the width i pairs
  lay <- matrix(c(2:length(plotlist)),ncol=2,byrow=TRUE)
  lay <- rbind(c(1,1),lay)
  tempgraphs <- grid.arrange(grobs=plotlist,layout_matrix=lay)
  # Saving the set of correlations as the variable name
  ggsave(filename=gsub("<","",(gsub(">","",(gsub("#","no",(gsub("%","perc",paste(names(outputtibble)[i+1],"correlations.pdf",sep="_"),fixed=TRUE)),fixed=TRUE)),fixed=TRUE)),fixed=TRUE),tempgraphs,width=8,height=4*(dim(lay)[1]),units="in",limitsize=FALSE)
}

```
Most interested in the correlations between overall alignment rate and read number to see "how good" the samples are:
```
# Using the output object created above
output[,1] <- gsub(".zip","",output[,1],fixed=TRUE)
output[,13] <- gsub("%","",output[,13],fixed=TRUE)
outputtibble <- tibble(output[2:(dim(output)[1]),1])

for (i in 2:dim(output)[2]) {
  outputtibble <- cbind(outputtibble,tibble(as.numeric(output[2:(dim(output)[1]),i])))  
}

names(outputtibble) <- output[1,]
outputtibble <- as_tibble(outputtibble)

ggplot(outputtibble,mapping=aes(x=Reads,y=overall_alignment_rate,color=File_name,fill=File_name)) + 
  geom_point(size=8,pch=21,alpha=0.5)+ theme_bw() + ylab("Overall alignment rate (%)") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1,size=16,colour="black")) + 
  theme(axis.text.y = element_text(size=16,colour="black")) +
  scale_x_continuous(labels = scales::comma) +
  theme(axis.title.x = element_text(face="bold",size=20)) +
  theme(axis.title.y = element_text(face="bold",size=20))
  ```

### Estimated individual heterozygosity using genomescope and jellyfish (example for Sample_1-D079)
A kmer of 17 did not appear to be long enough (underestimated genome length and appeared to overestimate heterozygosity)
```
#!/bin/bash -e 
#SBATCH -A uoo02423
#SBATCH -J 1-D029_jellyfish
#SBATCH -n 1
#SBATCH -c 36
#SBATCH -t 1:00:00
#SBATCH --mem=105G
#SBATCH -D /nesi/nobackup/uoo02423/data
#SBATCH --mail-type=ALL
#SBATCH --mail-user=laninsky@gmail.com
#SBATCH -N 1
#SBATCH --hint=nomultithread option

module load Jellyfish/2.2.6-gimkl-2017a
zcat trimmed/1-D079*.gz >> 1-D079_merged_data.fastq
jellyfish count -m 21 -o 1-D079_fastq.counts.jf -C 1-D079_merged_data.fastq -s 10000000000 -U 500 -t 36
rm 1-D079_merged_data.fastq
jellyfish histo -o 1-D079_fastq.counts.histo 1-D079_fastq.counts.jf
rm 1-D079_fastq.counts.jf
```
To run on each of the samples by modifying the script for Sample_1-D079...
```
for i in trimmed/*001_R1_001*;
do newname=`echo $i | sed "s'trimmed/''g" | sed 's/_S.*//g'`;
cp Sample_1-D079_jellyfish.sh Sample_${newname}_jellyfish.sh;
sed -i "s/1-D079/$newname/g" Sample_${newname}_jellyfish.sh
sbatch Sample_${newname}_jellyfish.sh
done
```
