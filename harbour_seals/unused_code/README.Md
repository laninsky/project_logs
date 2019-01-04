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
