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
#SBATCH -D /nesi/nobackup/uoo02423/uploaded 
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
#SBATCH -D /nesi/nobackup/uoo02423/uploaded 
#SBATCH --mail-type=ALL
#SBATCH --mail-user=alana.alexander@otago.ac.nz
#SBATCH -N 1

module load Bowtie2/2.3.2-gimkl-2017a

working_dir=/nesi/nobackup/uoo02423/uploaded/done/redo
mkdir $working_dir
mkdir $working_dir/trimmed

bowtie2-build --threads 16 GCA_003265705.1_ASM326570v1_genomic.fna northern_fur_seal
```

### Trimming raw reads and assembling them against the northern fur seal reference (example for Sample_1-D079)
Ran cutadapt on raw reads, used bowtie2 to assemble them to northern fur seal reference, ran fastqc to compare the trimmed reads to the raw reads, converted \*.sam to \*.bam. This script is modified from original cutadapt [loop](https://github.com/laninsky/project_logs/blob/master/harbour_seals/unused_code/README.Md) because of the presence of adaptor dimer. The bioinformatics troubleshooting for this is explained [here](https://github.com/laninsky/project_logs/blob/master/harbour_seals/unused_code/README.Md)
```
#!/bin/bash -e

#SBATCH --account=uoo02423
#SBATCH --job-name=Sample_1-D079
#SBATCH -n 1
#SBATCH --cpus-per-task=18
#SBATCH --time=24:00:00
#SBATCH --mem=52G
#SBATCH --hint=nomultithread
#SBATCH --partition=large
#SBATCH -D /nesi/nobackup/uoo02423/uploaded/done 
#SBATCH --mail-type=ALL
#SBATCH --mail-user=alana.alexander@otago.ac.nz
#SBATCH -N 1

module load SAMtools/1.8-gimkl-2017a
module load cutadapt/1.16-gimkl-2017a-Python-3.6.3
module load Bowtie2/2.3.2-gimkl-2017a
module load FastQC/0.11.7

working_dir=/nesi/nobackup/uoo02423/uploaded/done/redo
zipped_folder_location=/nesi/nobackup/uoo02423/uploaded/done

i=Sample_1-D079.zip;
basename=`echo $i | sed 's/.zip//g'`;
echo $basename >> $working_dir/$i.ref_log.txt
echo $basename >> $working_dir/$i.bam_log.txt
unzip $i;
cd $basename;
for j in *R1*.fastq.gz;
  do trimname1=`echo $j | sed 's/.fastq/.trimmed.fastq/g'`;
  trimname2=`echo $trimname1 | sed 's/R1/R2/g'`;
  reverse=`echo $j | sed 's/R1/R2/g'`;
  cutadapt -j 18 -a AGATCGGAAGAGC -a ATCGGAAGAGCACACGTCTGAACTCCAGTCACCCGCGGTTATCTCGTATGCCGTCTTCTGCTTGAAAAAA -A AGATCGGAAGAGC -A ATCGGAAGAGCGTCGTGTAGGGAAAGAGTGTCTAGCGCTGTGTAGATCTCGGTGGTCGCCGTATCATTAAAAAA -o $trimname1 -p $trimname2 $j $reverse -q 5,15 -m 25 >> $working_dir/$i.cutadapt.log;
done;
R1=`echo *R1*trimmed.fastq.gz | sed 's/ /,/g'`;
R2=`echo *R2*trimmed.fastq.gz | sed 's/ /,/g'`;
bowtie2 --fr -p 18 -x /nesi/nobackup/uoo02423/uploaded/northern_fur_seal -1 $R1 -2 $R2 -S $working_dir/$basename.sam &>> $working_dir/$i.ref_log.txt
cd $zipped_folder_location
fastqc $basename/*fastq.gz -o $working_dir/trimmed -t 18;
mv $basename/*trimmed* $working_dir/trimmed;
samtools view --threads 18 -bS -o $working_dir/$basename.bam $working_dir/$basename.sam &>> $working_dir/$i.bam_log.txt;
rm -rf $working_dir/$basename.sam;
rm -rf $basename;
```
To create a sample-specific \*.sh file for each \*zip based on Sample_19-D135.sh and then start these jobs off:
```
for i in *.zip; 
  do basename=`echo $i | sed 's/.zip//g'`;
  cp Sample_1-D079.sh $basename.sh;
  sed -i "s/Sample_1-D079/$basename/g" $basename.sh;
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
  
  # Need to create a for loop given this is now going to be more than one adaptor
  # probably something like this WORK IN PROGRESS
  read1names <- unique(tempfile[grep("=== First read: Adapter [0-9]+ ===",tempfile)])
  for (j in $read1names) { 
    tempoutput <- rbind(tempoutput,tempfile[grep($j,tempfile) + 2]) 
    tempoutput <- rbind(tempoutput,tempfile[grep($j,tempfile) + 8]) 
    tempoutput <- rbind(tempoutput,tempfile[grep($j,tempfile) + 9]) 
    tempoutput <- rbind(tempoutput,tempfile[grep($j,tempfile) + 10]) 
    tempoutput <- rbind(tempoutput,tempfile[grep($j,tempfile) + 11]) 
    tempoutput <- rbind(tempoutput,tempfile[grep($j,tempfile) + 12])
  }
  
  read2names <- unique(tempfile[grep("=== Second read: Adapter [0-9]+ ===",tempfile)])
  for (j in $read2names) { 
    tempoutput <- rbind(tempoutput,tempfile[grep($j,tempfile) + 2]) 
    tempoutput <- rbind(tempoutput,tempfile[grep($j,tempfile) + 8]) 
    tempoutput <- rbind(tempoutput,tempfile[grep($j,tempfile) + 9]) 
    tempoutput <- rbind(tempoutput,tempfile[grep($j,tempfile) + 10]) 
    tempoutput <- rbind(tempoutput,tempfile[grep($j,tempfile) + 11]) 
    tempoutput <- rbind(tempoutput,tempfile[grep($j,tempfile) + 12])
  }  
  
  header <- rep(samplename,dim(tempoutput)[2])
  
  tempoutput <- rbind(header,tempoutput)
  
  output <- cbind(output,tempoutput)
}  

# WORK IN PROGRESS - need to do a finalheader for this one too.

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
library(readr)
library(tidyr)
library(dplyr)
library(GGally)

temp <- read_table2("cutadapt_log_summary.txt")

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

# A lot of variables to plot all at once, so can do these in batches by modifying startpos and endpos
# samplename is in column 1, read file name is in column 2, so including 1 in every plot, and excluding 2
# (2 shows up as the separate points plotted for each sample)

startpos <- 3
endpos <- dim(temp)[2]

corgraph <- ggpairs(temp, mapping=(aes(fill = sample_name,color=sample_name)),
        columns=c(names(temp)[c(1,startpos:endpos)]),
        upper=list(continuous="blank",combo="blank"),
        lower=list(continuous=wrap("points",size=5,pch=21,alpha=0.5,color="black"),combo=wrap("facetdensity")),
        diag=list(discrete="blankDiag",continuous=wrap("densityDiag",alpha=0.5)),legend = (endpos-startpos+3))

corgraph[(endpos-startpos+2),1] <- corgraph[(endpos-startpos+2),1] + theme(axis.text.x = element_blank(),
                           axis.ticks = element_blank())  

# This adjusts the width of the plot for how many samples we are plotting
plotwidthmultiplier <- round(length(unique(temp$sample_name)),-1)

ggsave(filename="cutadapt_correlations.pdf",corgraph,width=(endpos-startpos)*plotwidthmultiplier,height=(endpos-startpos)*plotwidthmultiplier,units="cm",limitsize=FALSE)

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
