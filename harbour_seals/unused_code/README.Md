### Previous cutadapt code
This is the cutadapt command I originally used in this [script:]( https://github.com/laninsky/project_logs/blob/master/harbour_seals/nuclear_genome.Md#trimming-raw-reads-and-assembling-them-against-the-northern-fur-seal-reference-example-for-sample_19-d135)

However, there appeared to be adapter/adaptor dimer/concatamer (this troubleshooting explained below) so I switched the loop to remove any sequences with the contaminating sequence present in the 5' part of the read (the code currently in the loop [here]( https://github.com/laninsky/project_logs/blob/master/harbour_seals/nuclear_genome.Md#trimming-raw-reads-and-assembling-them-against-the-northern-fur-seal-reference-example-for-sample_19-d135)). 

```
for j in *R1*.fastq.gz;
  do trimname1=`echo $j | sed 's/.fastq/.trimmed.fastq/g'`;
  trimname2=`echo $trimname1 | sed 's/R1/R2/g'`;
  reverse=`echo $j | sed 's/R1/R2/g'`;
  cutadapt -j 16 -a AGATCGGAAGAGC -A AGATCGGAAGAGC -o $trimname1 -p $trimname2 $j $reverse -q 5,15 -m 25 >> ../$i.cutadapt.log;
done;
```

### How did I come to the conclusion this was adaptor dimer?
After initial trimming of reads for quality/3’-adaptor contamination using the code above, FastQC found truseq/illumina sequencing primers were still over-represented. The presence of the Illumina adaptor/primers in the "over-represented sequences" module of FastQC but not in the actual "adaptor sequences" module suggested this adaptor might not be at the 3' end of the reads. A quick grep for the over-represented sequences suggested this was very much the case e.g.
```
zgrep ^ATCGGAAGAGCACACGTCTGAACTCCAGTCA 12-D099_S7_L001_R1_001.fastq.gz
ATCGGAAGAGCACACGTCTGAACTCCAGTCACCTCTCGTCATCTCGTCTGCCGTCTTCTGCTTGAACAAACGATTCCAGAAGTACTGTCCTCACCTGCGTGCTTCTTGCTGCGGCGCCATTCTGCCCAGTGGGATCACGCACCACCGTAAC
ATCGGAAGAGCACACGTCTGAACTCCAGTCACCTCTCGTCATCCCGTATGCCGTCTTCTGCTTGAAAAAAATAGAGAAATGGTTAAAAAGATTAAGCGGGTTGGTGTTTGTGTTGTGGGAGTTGTGGCGGTGGAGGGAGGAGACCGATGAG
ATCGGAAGAGCACACGTCTGAACTCCAGTCACCTCTCGTCATCTCGTATGCCGTCTTCTGCTTGAAAAAATTGTATGGGGTTTGTTGTGGGTGGTATTTGTTTTGTTTGTTTTGTTGGGTGGGCGGTGGGGGTTGTTTGGGGTGTGGTTGG
ATCGGAAGAGCACACGTCTGAACTCCAGTCACCTCTCGTCATCTCGTATGCCGTCTTCTGCTTGAAAAAATGATTGGGGGGGGTTGTTGTGGTGGTGGTGTGGGTTTGTGGTCGGTTTTGGTTGGTGTTGTGGATGAGGTGGTGTGTGTGG
ATCGGAAGAGCACACGTCTGAACTCCAGTCACCTCTCGTCATCTCGTATGCCGTCTTCTGCTTGAAAAAACGCTACAGTCTGGAGGAAATCCTGTAGTGCAACGGACTCTGCAGTCTATCTTATCTCATTCTTGTGTTGGTGAATTTTGTG
ATCGGAAGAGCACACGTCTGAACTCCAGTCACCTCTCGTCATCTCGTATGCCGTCTTCTGCTTGAAAAAAAGATTTGGATTTTTGTGTTTTTGGGTTGCTTGGTGTTTGGGGGGTGTGGTCTGCGTTGGGTTGTGGCTTGCGTGTGTTTGT

zgrep ^ATCGGAAGAGCGTCGTGTAGGGAAAGAGTGT 12-D099_S7_L001_R2_001.fastq.gz
ATCGGAAGAGCGTCGTGTAGGGAAAGAGTGTAGGTTATAGTGTAGATCTCGGTGGTCGCCGTATCATTAAAAAAAAAAAATTTTTTTGGTTTCCCTAACAGAATTAAAAACAGAACAACGGTAAGTGAGTAAAAGATTGAGGTCATTTGAA
ATCGGAAGAGCGTCGTGTAGGGAAAGAGTGTAGGTTATAGTGTAGATCTCGGTGGTCGCCGTATCATTAAAAAAAATGGATAAGTAAGTGTTGGGGATCGTGTGGTGGTTGTGTTGGTGGAGAGAGGTGTGTTGAGTGGTGGTGTGGGTTG
ATCGGAAGAGCGTCGTGTAGGGAAAGAGTGTAGGTTATAGTGTAGATCTCGGTGGTCGCCGTATCATTAAAAAAGAGTGTGGTTTTTATGGTGTTTTTATTTTGCTTGGTTTTGTTGGTTTGGTCTGTTTTGTTTGGAGGTTGTAGGGGTT
ATCGGAAGAGCGTCGTGTAGGGAAAGAGTGTAGGTTATAGTGTAGATCTCGGTGGTCGCCGTATCATTAAAAAAAAAGTGATTGTTGGTGTCGTGAAGTGGATAGTAGGGGTGTGGAGGTATATGGAGATGGAAGGGCGGACGGAGGTAGT
ATCGGAAGAGCGTCGTGTAGGGAAAGAGTGTAGGTTATAGTGTAGATCTCGGTGGTCGCCGTATCATTAAAAAAATTGATGTTTTGGGGGGTTTTTTTTGTTGTAGTAGTTGGGTAGTTTGTGTTTTGTTGTGTGTGGTGATTGTTTTTTT
ATCGGAAGAGCGTCGTGTAGGGAAAGAGTGTAGGTTATAGTGTAGATCTCGGTGGTCGCCGTATCATTAAAAAAACAAGTGATGTGGTTGGGATGTTGTAGAATCAAGTGTGAGATGAGGTGATATGTTGTGTGGAGAGGATTTGTGAGGG
```

These sequences matched up really nicely with the i5 and i7 sequences described [here](https://support.illumina.com/content/dam/illumina-support/documents/documentation/chemistry_documentation/experiment-design/illumina-adapter-sequences-1000000002694-09.pdf) e.g.
```
# First read
[over-represented sequence]          ATCGGAAGAGCACACGTCTGAACTCCAGTCACCCGCGGTTATCTCGTATGCCGTCTTCTGCTTGAAAAA
[i7 adaptor]                        GATCGGAAGAGCACACGTCTGAACTCCAGTCA[i7index]ATCTCGTATGCCGTCTTCTGCTTG

# Second read
[over-represented sequence]           ATCGGAAGAGCGTCGTGTAGGGAAAGAGTGTCTAGCGCTGTGTAGATCTCGGTGGTCGCCGTATCATTAAAAA
[Reverse Complement of i5 adaptor]  AGATCGGAAGAGCGTCGTGTAGGGAAAGAGTGT[i5indx]GTGTAGATCTCGGTGGTCGCCGTATCATT
```

What was getting me stuck, however, is why the over-represented sequences didn't cover the first few bases of the adaptor sequence. This was probably the reason why the original cutadapt loop couldn't cull these reads out, because running cutadapt in default mode allows for only up to 10% errors when matching your adaptor sequence against your sequencing reads. In the 13 bp abbreviated adaptor sequence I fed cutadapt above (AGATCGGAAGAGC), this means up to 1.3 (probably rounded to 1) errors would be allowed. By missing the first 2 bp off this expected adaptor sequence, the over-represented sequences above had too many differences to the expected adaptor sequence to be culled.

But back to my original question - why were these bases missing? The following preprint was bloody useful in figuring this out!

Glenn, T. C., R. Nilsen, T. J. Kieran, J. W. Finger Jr., T. W. Pierson, K. E. Bentley, S. L. Hoffberg, S. Louha, F. J. Garcia-De Leon, M. A. D. R. Portilla, K. D. Reed, J. L. Anderson, J. K. Meece, S. E. Aggrey, R. Rekaya, M. Alabady, M. Belanger, K. Winker, and B. C. Faircloth. Adapterama I: Universal stubs and primers for thousands of dual-indexed Illumina libraries (iTru & iNext). Preprint at http://biorxiv.org/content/early/2016/06/15/049114

The i5 and i7 adaptors have a region of complementarity (Y-shaped adaptor). Coupled with the 3' A-tailing during [library prep](https://www.illumina.com/content/dam/illumina-marketing/documents/products/datasheets/datasheet_truseq_dna_pcr_free_sample_prep.pdf), this ensures the formation of library fragments with i5 on one side, and i7 on the other (i.e. exactly what we want for sequencing!).
```
# What these y-shaped adapters look like (yes, sorry, the DNA frag of interest is back-to-front, in comparison with the usual
# orientation for a diagram like this, but I didn't realise I had these in a non-intuitive direction until too late, and 
# couldn't be bothered remaking them! To get it in the usual 5' to 3' orientation flip it from left to right, and then top to 
# bottom in your brain
                                                                                      DNA frag of interest
[reverse i7]       GTTCGTCTTCTGCCGTATGCTCTA[7i]ACTGACCTCAAGTCTGCACA                  3'                   5'                     CAGCACATCCCTTTCTCACA[5i]CACATCTAGAGCCACCAGCGGCATAGCAA [Reverse i5]
                                                                   CGAGAAGGTCAG      A-xxxxxxxxxxxxxxxxxx           TCTAGCCTTCTCG
                                                                   GCTCTTCCGATCT       xxxxxxxxxxxxxxxxxx-A          GATCGGAAGAGC
[i5 adaptor]  AATGATACGGCGACCACCGAGATCTACAC[i5]ACACTCTTTCCCTACACGAC                  5'                   3'                     ACACGTCTGAACTCCAGTCA[i7]ATCTCGTATGCCGTCTTCTGCTTG  [i7 adaptor]
                                          
```

But... the dark side of this is adaptor-adaptor dimer, where the adaptors stick together instead of to our DNA fragments of interest. There are two base pairs at the very end of the adaptors that happen to be complementary to each other, and I think these are the bases sticking together e.g.
```
[reverse i7]     GTTCGTCTTCTGCCGTATGCTCTA[7i]ACTGACCTCAAGTCTGCACA\                  
                                                                  \CGAGAAGGTCAG
[i5 adaptor]  AATGATACGGCGACCACCGAGATCTACAC[i5]ACACTCTTTCCCTACACGACGCTCTTCCGATCT
                                                                              ||
                                                                              GATCGGAAGAGCACACGTCTGAACTCCAGTCA[i7index]ATCTCGTATGCCGTCTTCTGCTTG   [i7 adaptor] 
                                                                             TCTAGCCTTCTCG\
                                                                                           \CAGCACATCCCTTTCTCACA[5i]CACATCTAGAGCCACCAGCGGCATAGCAA [Reverse i5]
```
I'm not sure how this exactly would proceed in the ligation reaction, but let's say some exonuclease chomping going on on the overhanging ends to allow these fragments to mush together e.g.
```
# Putative adaptor-Adaptor dimer molecule
[reverse i7]     GTTCGTCTTCTGCCGTATGCTCTA[7i]ACTGACCTCAAGTCTGCACA\                  
                                                                  \CGAGAAGGTCAGATCGGAAGAGCACACGTCTGAACTCCAGTCA[i7index]ATCTCGTATGCCGTCTTCTGCTTG   [i7 adaptor] 
[i5 adaptor]  AATGATACGGCGACCACCGAGATCTACAC[i5]ACACTCTTTCCCTACACGACGCTCTTCCGATCTAGCCTTCTCG\
                                                                                           \CAGCACATCCCTTTCTCACA[5i]CACATCTAGAGCCACCAGCGGCATAGCAA [Reverse i5]
```
Then coupled with where the sequencing primer sits down, we get the following expected sequence from this fragment that exactly matches our over-represented sequence (e.g. identical to i7 with a base shaved off the beginning):                                                                                             
```
[reverse i7]     GTTCGTCTTCTGCCGTATGCTCTA[7i]ACTGACCTCAAGTCTGCACA\                  
                                                                  \CGAGAAGGTCAGATCGGAAGAGCACACGTCTGAACTCCAGTCA[i7index]ATCTCGTATGCCGTCTTCTGCTTG   [i7 adaptor] 
[i5 adaptor]  AATGATACGGCGACCACCGAGATCTACAC[i5]ACACTCTTTCCCTACACGACGCTCTTCCGATCTAGCCTTCTCG\
                                                                                           \CAGCACATCCCTTTCTCACA[5i]CACATCTAGAGCCACCAGCGGCATAGCAA [Reverse i5]
                                                R1 seq primer ->   GCTCTTCCGATC|
                                         [Read 1 over-represented sequence]    ATCGGAAGAGCACACGTCTGAACTCCAGTCACCCGCGGTTATCTCGTATGCCGTCTTCTGCTTGAAAAA
                       [in comparison to i7 adaptor (also see top strand)]    GATCGGAAGAGCACACGTCTGAACTCCAGTCA[i7index]ATCTCGTATGCCGTCTTCTGCTTG  

```
But what about Read 2's over-represented sequence? It has TWO base pairs shaved off the beginning. We start off with the "same" fragment, but flipped in the vertical direction because Read 2 is primed off the i7 sequencing primer.
```
# Sorry, going to make you do mental arithmetic again - I've reversed but not reverse-comped the following.
# Instead, I'll reverse-comp Read 2's over-represented sequence once I get to this demo below.
                                                                                                                                                             
[i5 adaptor]  AATGATACGGCGACCACCGAGATCTACAC[i5]ACACTCTTTCCCTACACGACGCTCTTCCGATCTAGCCTTCTCGCAGCACATCCCTTTCTCACA[xedn5i]CACATCTAGAGCCACCAGCGGCATAGCAA   [Reverse i5] 
                                                                  /CGAGAAGGTCAGATCGGAAGAGC\
[reverse i7]     GTTCGTCTTCTGCCGTATGCTCTA[7i]ACTGACCTCAAGTCTGCACA/                         \ACACGTCTGAACTCCAGTCA[i7index]ATCTCGTATGCCGTCTTCTGCTTG   [i7 adaptor]                                                    
```
Adding in the sequencing primer...
```
[i5 adaptor]  AATGATACGGCGACCACCGAGATCTACAC[i5]ACACTCTTTCCCTACACGACGCTCTTCCGATCTAGCCTTCTCGCAGCACATCCCTTTCTCACA[xedn5i]CACATCTAGAGCCACCAGCGGCATAGCAA   [Reverse i5] 
                                                                  /CGAGAAGGTCAGATCGGAAGAGC\
[reverse i7]     GTTCGTCTTCTGCCGTATGCTCTA[7i]ACTGACCTCAAGTCTGCACA/                         \ACACGTCTGAACTCCAGTCA[i7index]ATCTCGTATGCCGTCTTCTGCTTG   [i7 adaptor]                                                    
                                 R2 seq primer reverse comped ->   CGAGAAGGTCAG|  
                           [Read 2 over-represented sequence reverse comped]   TAGCCTTCTCGCAGCACATCCCTTTCTCACAGATCGCGACACATCTAGAGCCACCAGCGGCATAGTAATTTTT                                                                                                                              
           [in comparison to reversed i5 adaptor (also see top strand)] ]    TCTAGCCTTCTCGCAGCACATCCCTTTCTCACA[xedn5i]CACATCTAGAGCCACCAGCGGCATAGCAA 
                                                        
```
We can see that because of how the Read 2 sequencing primer sits down, that it shaves 2 bp off the i5 adaptor sequence, leading to Read 2's over-represented sequence.

### Pulled out cutadapt stats to record them (in R)
This step could be skipped after solving the adaptor sequence issue.
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

