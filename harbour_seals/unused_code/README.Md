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
After initial trimming of reads for quality/3’-adaptor contamination using the code above, FastQC found truseq/illumina sequencing primer  were overrepresented, even in the trimmed sequences. The presence of the Illumina adaptor/primers in the "overrepresented sequences" module of FastQC but not in the actual "adaptor sequences" module suggested this adaptor might not be at the 3' end of hte reads. A quick grep for the overrepresented sequences suggested this was very much the case e.g.
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
These sequences matched up really nicely with the i5 and i7 sequences described [here](https://support.illumina.com/content/dam/illumina-support/documents/documentation/chemistry_documentation/experiment-design/illumina-adapter-sequences-1000000002694-09.pdf)
```
#First read
[over-represented sequence]          ATCGGAAGAGCACACGTCTGAACTCCAGTCACCCGCGGTTATCTCGTATGCCGTCTTCTGCTTGAAAAA
[i7 adaptor]                        GATCGGAAGAGCACACGTCTGAACTCCAGTCA[i7index]ATCTCGTATGCCGTCTTCTGCTTG

#Second read
[over-represented sequence]           ATCGGAAGAGCGTCGTGTAGGGAAAGAGTGTCTAGCGCTGTGTAGATCTCGGTGGTCGCCGTATCATTAAAAA
[Reverse Complement of i5 adaptor]  AGATCGGAAGAGCGTCGTGTAGGGAAAGAGTGT[i5indx]GTGTAGATCTCGGTGGTCGCCGTATCATT
```
What was getting me stuck, however, is why the over-represented sequences didn't cover the first few bases of the adaptor sequence. This was probably the reason why the original cutadapt loop couldn't cut these guys out, because running cutadapt in default mode it only allows for up to 10% errors when matching your adaptor sequence against your sequencing reads. In the 13 bp abbreviated adaptor sequence I fed cutadapt above (AGATCGGAAGAGC), this means up to 1.3 (probably rounded to 1) errors would be allowed. By missing the first 2 bp off this expected adaptor sequence, the over-represented sequences above had too many differences to the expected adaptor sequence to be culled.


, but not adaptor sequences as it was present at the 5’ end of reads).


The sequence was very similar to the adapters, but missing the first two basepair (potentially as past of the ligation reaction).
