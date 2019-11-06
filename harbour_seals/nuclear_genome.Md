## Steps
[Confirmed md5sums matched between transferred files/folders](https://github.com/laninsky/project_logs/blob/master/harbour_seals/nuclear_genome.Md#confirmed-md5sums-matched-between-transferred-filesfolders)  
[Indexed reference](https://github.com/laninsky/project_logs/blob/master/harbour_seals/nuclear_genome.Md#indexed-reference)  
[Trimming raw reads and assembling them against the harbour seal reference (example for Sample_1-D079)](https://github.com/laninsky/project_logs/blob/master/harbour_seals/nuclear_genome.Md#trimming-raw-reads-and-assembling-them-against-the-harbour-seal-reference-example-for-sample_1-d079)  
[Pulled out cutadapt stats to record them (in R)](https://github.com/laninsky/project_logs/blob/master/harbour_seals/nuclear_genome.Md#pulled-out-cutadapt-stats-to-record-them-in-r)  
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
cd $zipped_folder_location/$basename;
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
rm -rf $zipped_folder_location/$basename;
```

For the slurm-output, I prepended them with sample name using the code below.
```
mkdir scripts
ls *.zip > zip.txt
ls *.out > slurm.txt
for i in `seq $ziplines`; do samplename=` tail -n $i zip.txt | head -n 1 | sed 's/.zip//g'`; oldslurm=`tail -n $i slurm.txt | head -n 1`; newslurm=`echo $samplename.$oldslurm`; mv $oldslurm scripts/$newslurm; done
```

### Double checking fastqc
In the fastqc folder, I ran the following bash code to extract the summary.txt file from each fastq file, and then the R code to compare before and after trimming fastqc results:
```
for i in *.zip;
do foldername=`echo $i | sed 's/.zip//g'`;
unzip $i;
cat $foldername/summary.txt >> fastqc_summary.txt;
rm -rf $foldername;
done;
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
  
  # Need to create a for loop given this is now going to be more than one adaptor
  # probably something like this WORK IN PROGRESS
  read1names <- unique(tempfile[grep("=== First read: Adapter [0-9]+ ===",tempfile)])
  for (j in read1names) { 
    tempoutput <- rbind(tempoutput,tempfile[grep(j,tempfile) + 2]) 
    tempoutput <- rbind(tempoutput,tempfile[grep(j,tempfile) + 8]) 
    tempoutput <- rbind(tempoutput,tempfile[grep(j,tempfile) + 9]) 
    tempoutput <- rbind(tempoutput,tempfile[grep(j,tempfile) + 10]) 
    tempoutput <- rbind(tempoutput,tempfile[grep(j,tempfile) + 11]) 
    tempoutput <- rbind(tempoutput,tempfile[grep(j,tempfile) + 12])
  }
  
  read2names <- unique(tempfile[grep("=== Second read: Adapter [0-9]+ ===",tempfile)])
  for (j in read2names) { 
    tempoutput <- rbind(tempoutput,tempfile[grep(j,tempfile) + 2]) 
    tempoutput <- rbind(tempoutput,tempfile[grep(j,tempfile) + 8]) 
    tempoutput <- rbind(tempoutput,tempfile[grep(j,tempfile) + 9]) 
    tempoutput <- rbind(tempoutput,tempfile[grep(j,tempfile) + 10]) 
    tempoutput <- rbind(tempoutput,tempfile[grep(j,tempfile) + 11]) 
    tempoutput <- rbind(tempoutput,tempfile[grep(j,tempfile) + 12])
  }  
  
  header <- rep(samplename,dim(tempoutput)[2])
  
  tempoutput <- rbind(header,tempoutput)
  
  output <- cbind(output,tempoutput)
}  

# Need to check to make sure previous has pulled out all of the Adaptors (think it will have based on the grep
# Need to look into how to adapt this for an arbritrary number of adaptors
# Also comment this whole bit of code better

finalheader <- c("sample_name","seq_file_name","Read_1_with_adapter_#","Read_1_with_adapter_%","Read_2_with_adapter_#","Read_2_with_adapter_%","Pairs_that_were_too_short_#","Pairs_that_were_too_short_%","Pairs_written_passing_filters_#","Pairs_written_passing_filters_%","Total_basepairs_processed_bp","Read_1_processed_bp","Read_2_processed_bp","Quality_trimmed_bp","Quality_trimmed_%","Read_1_quality_trimmed","Read_2_quality_trimmed","Total_bp_written","Total_%_written","Read_1_bp_written","Read_2_bp_written","No_times_adapter_1_trimmed","%_times_A_precedes_adapter1_seq","%_times_C_precedes_adapter1_seq","%_times_G_precedes_adapter1_seq","%_times_T_precedes_adapter1_seq","%_times_none_other_precedes_adapter1_seq","No_times_adapter_2_trimmed","%_times_A_precedes_adapter2_seq","%_times_C_precedes_adapter2_seq","%_times_G_precedes_adapter2_seq","%_times_T_precedes_adapter2_seq","%_times_none_other_precedes_adapter2_seq")

output[2,] <- gsub(" ",",",gsub(".*trimmed.fastq.gz ","",gsub(" -q .*","",output[2,])))
temp3 <- gsub("  Read 1 with adapter: *","",output[3,])
output[3,] <- gsub(" .*","",temp3)
temp3 <- gsub("%\\)","",gsub(".*\\(","",temp3))
output <- rbind(output[1:3,],temp3,output[4:27,])
temp5 <- gsub("  Read 2 with adapter: *","",output[5,])
output[5,] <- gsub(" .*","",temp5)
temp5 <- gsub("%\\)","",gsub(".*\\(","",temp5))
output <- rbind(output[1:5,],temp5,output[6:28,])
temp7 <- gsub("Pairs that were too short: *","",output[7,])
output[7,] <- gsub(" .*","",temp7)
temp7 <- gsub("%\\)","",gsub(".*\\(","",temp7))
output <- rbind(output[1:7,],temp7,output[8:29,])
temp9 <- gsub("Pairs written \\(passing filters\\): *","",output[9,])
output[9,] <- gsub(" .*","",temp9)
temp9 <- gsub("%\\)","",gsub(".*\\(","",temp9))
output <- rbind(output[1:9,],temp9,output[10:30,])
output[11,] <- gsub(" bp","",gsub("Total basepairs processed: *","",output[11,]))
output[12,] <- gsub(" bp","",gsub("  Read 1: *","",output[12,]))
output[13,] <- gsub(" bp","",gsub("  Read 2: *","",output[13,]))
temp14 <- gsub("Quality-trimmed:  *","",output[14,])
output[14,] <- gsub(" .*","",temp14)
temp14 <- gsub("%\\)","",gsub(".*\\(","",temp14))
output <- rbind(output[1:14,],temp14,output[15:31,])
output[16,] <- gsub(" bp","",gsub("  Read 1: *","",output[16,]))
output[17,] <- gsub(" bp","",gsub("  Read 2: *","",output[17,]))
temp18 <- gsub("Total written \\(filtered\\): *","",output[18,])
output[18,] <- gsub(" .*","",temp18)
temp18 <- gsub("%\\)","",gsub(".*\\(","",temp18))
output <- rbind(output[1:18,],temp18,output[19:32,])
output[20,] <- gsub(" bp","",gsub("  Read 1: *","",output[20,]))
output[21,] <- gsub(" bp","",gsub("  Read 2: *","",output[21,]))
output[22,] <- gsub(" times.","",gsub(".*Trimmed: *","",output[22,]))
output[23,] <- gsub("%","",gsub("  A: *","",output[23,]))
output[24,] <- gsub("%","",gsub("  C: *","",output[24,]))
output[25,] <- gsub("%","",gsub("  G: *","",output[25,]))
output[26,] <- gsub("%","",gsub("  T: *","",output[26,]))
output[27,] <- gsub("%","",gsub("  none/other: *","",output[27,]))
output[28,] <- gsub("%","",gsub(" times.","",gsub(".*Trimmed: *","",output[28,])))
output[29,] <- gsub("%","",gsub("  A: *","",output[29,]))
output[30,] <- gsub("%","",gsub("  C: *","",output[30,]))
output[31,] <- gsub("%","",gsub("  G: *","",output[31,]))
output[32,] <- gsub("%","",gsub("  T: *","",output[32,]))
output[33,] <- gsub("%","",gsub("  none/other: *","",output[33,]))

write.table(t(cbind(finalheader,output)),"cutadapt_log_summary.txt",quote=FALSE,row.name=FALSE,col.name=FALSE)  

```
I downloaded and opened cutadapt_log_summary.txt to my own computer so I could pull it through Rstudio and eyeball plots as I went to make sure there weren't any squiffy correlations and to do some sample summaries
```
# Loading in the libraries
library(readr)
library(tidyr)
library(dplyr)
library(GGally)
library(gridExtra)
library(grid)
library(ggplot2)
library(lattice)

# the path to the file generated above
temp <- read_table2("C://Users//Alana//Dropbox//seal//Emma//genome//logs//cutadapt//cutadapt_log_summary.txt")

# What this tibble looks like
temp
## A tibble: 56 x 33
#   sample_name seq_file_name `Read_1_with_ad… `Read_1_with_ad… `Read_2_with_ad… `Read_2_with_ad…
#   <chr>       <chr>                    <dbl>            <dbl>            <dbl>            <dbl>
# 1 Sample_12-… 12-D099_S7_L…          1139203              3.2          1117572              3.1
# 2 Sample_12-… 12-D099_S7_L…          1173278              3.2          1159492              3.2
# 3 Sample_12-… 12-D099_S7_L…          1050600              3.2          1028700              3.2
# 4 Sample_12-… 12-D099_S7_L…          1070980              3.2          1047569              3.2
# 5 Sample_14-… 14-D084_S11_…           897505              3.1           882116              3.1
# 6 Sample_14-… 14-D084_S11_…           925149              3.1           916051              3.1
# 7 Sample_14-… 14-D084_S11_…           826148              3.2           812282              3.1
# 8 Sample_14-… 14-D084_S11_…           847822              3.2           826900              3.1
# 9 Sample_15-… 15-591_S13_L…          1011762              3            1000638              2.9
#10 Sample_15-… 15-591_S13_L…          1039595              3            1038096              3  
## ... with 46 more rows, and 27 more variables: `Pairs_that_were_too_short_#` <dbl>,
##   `Pairs_that_were_too_short_%` <dbl>, `Pairs_written_passing_filters_#` <dbl>,
##   `Pairs_written_passing_filters_%` <dbl>, Total_basepairs_processed_bp <dbl>,
##   Read_1_processed_bp <dbl>, Read_2_processed_bp <dbl>, Quality_trimmed_bp <dbl>,
##   `Quality_trimmed_%` <dbl>, Read_1_quality_trimmed <dbl>, Read_2_quality_trimmed <dbl>,
##   Total_bp_written <dbl>, `Total_%_written` <dbl>, Read_1_bp_written <dbl>,
##   Read_2_bp_written <dbl>, No_times_adapter_1_trimmed <int>,
##   `%_times_A_precedes_adapter1_seq` <dbl>, `%_times_C_precedes_adapter1_seq` <dbl>,
##   `%_times_G_precedes_adapter1_seq` <dbl>, `%_times_T_precedes_adapter1_seq` <dbl>,
##   `%_times_none_other_precedes_adapter1_seq` <dbl>, No_times_adapter_2_trimmed <int>,
##   `%_times_A_precedes_adapter2_seq` <dbl>, `%_times_C_precedes_adapter2_seq` <dbl>,
##   `%_times_G_precedes_adapter2_seq` <dbl>, `%_times_T_precedes_adapter2_seq` <dbl>,
##   `%_times_none_other_precedes_adapter2_seq` <dbl>

# Doing a ggpairs plot on everything bar the first two columns, coloring by sample_name
corgraph <- ggpairs(temp, cardinality_threshold=length(unique(temp$sample_name)),mapping=(aes(fill = sample_name,color=sample_name)),
                    columns=(names(temp)[-1:-2]),
                    upper=list(continuous="blank",combo="blank"),
                    lower=list(continuous=wrap("points",size=5,pch=21,alpha=0.5,color="black"),combo=wrap("facetdensity")),
                    diag=list(discrete="blankDiag",continuous=wrap("densityDiag",alpha=0.5)))+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))  + theme(legend.text=element_text(size=8))

# The following is going to generate a pdf for each quantitative cutadapt variable
# with graphs of that variable against all other quantitative variable

# Then for every column (except the last one) in the ggpairs correlation graph
for (i in 1:(corgraph$ncol-1)) {
  # Make a list, and populate the first item in the list with the column's variable self comparison
  # It's going to be in its own row (including a legend)
  plotlist <- list()
  plotlist[[1]] <- corgraph[i,i]
  # For the remaining comparisons of that variable to others, if there isn't an odd number
  if ((corgraph$ncol-i) %% 2 == 0) {
  # Adding them in to the plot list
    for (j in (i+1):corgraph$ncol) {
      plotlist[[j-i+1]] <- corgraph[j,i] + theme(legend.position = "none")
    }
  } else {
    # Or else, add a blank element in first
    plotlist[[2]] <- grid.rect(gp = gpar(col = "white"))
    for (j in (i+1):corgraph$ncol) {
      plotlist[[j-i+2]] <- corgraph[j,i] + theme(legend.position = "none")
    }
  }
  # Creating the layout (first self-self plot with the legend in first row
  # Every other plot half the width i pairs
  lay <- matrix(c(2:length(plotlist)),ncol=2,byrow=TRUE)
  lay <- rbind(c(1,1),lay)
  tempgraphs <- grid.arrange(grobs=plotlist,layout_matrix=lay)
  # Saving the set of correlations as the variable name
  ggsave(filename=gsub("#","no",gsub("%","perc",paste(names(temp)[i+2],"correlations.pdf",sep="_"),fixed=TRUE),fixed=TRUE),tempgraphs,width=8,height=4*(dim(lay)[1]),units="in",limitsize=FALSE)
}

```


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
