Based on Debbie's protocol, the basic steps (following amplification with the control region primers) are to denoise/demultiplex with dada2 in qiime, with no filtering other than to trim primers. Debbie then does a blast search and extracts the top 5 hits, and pulls the cetaceans from that, as well as the humans/pigs/cow/mouse to see what those are doing. This ends up giving a table of ASV counts, a fasta file of all ASVs, a blast output, and a cetacean-only top hit only blast summary. This is my adaptation of Debbie's protocol, greatly aided by the awesome tutorials at https://docs.qiime2.org/ 

These steps need to be completed every time you log in to NeSI:
```
# Need to load Python/3.10.5 otherwise QIIME2 errors
# because of numpy errors
module load Python/3.11.3-gimkl-2022a

# Loading QIIME2
module load QIIME2/2022.2

# Enabling tab completion
source tab-qiime

# Loading BLAST
module load BLAST/2.13.0-GCC-11.3.0
```

Upload files to the appropriate directory (fastq) using scp:
```
# QIIME expects data to be called "forward.fastq.gz" 
# and "reverse.fastq.gz", therefore files need to be renamed e.g.
mv Undetermined_S0_L001_R1_001.fastq.gz fastq/forward.fastq.gz
mv Undetermined_S0_L001_R2_001.fastq.gz fastq/reverse.fastq.gz

# Our files are from paired end sequencing, whether or not they 
# are sequenced in a way the reads overlap (e.g. for our frag, miseq 
# PE 250 is needed), so this is the initial pipeline followed for QCing
# the reads. fastq for each run is in its own parent folder 
# e.g. broad-single-end  narrow-single-end  paired-end-sequences 4Apr2023_post_size
```

Getting a barcodes file together (barcodes.tsv)
```
sampleid        forwardindex    reverseindex
BP_S_BD01       AACAAGCC        GAGCTTAC
BP_S_HD_021     GGAATGAG        GAGCTTAC
BP_N_HD_027     AACAAGCC        TTACCGCT
TIM_HD_043      GGAATGAG        TTACCGCT

# When duplicate samples are present with different indices give unique suffix to sampleid
sampleid        forwardindex    reverseindex
BP_S_HD01A      AACAAGCC        GAGCTTAC
BP_S_HD01B      GGAATGAG        GAGCTTAC
BP_N_HD02A      TTACCAGG        GAGCTTAC
BP_N_HD03B      CTGACCTT        GAGCTTAC
```

File directory
```
barcodes.tsv
broad-single-end/
   |--fastq/
      |--forward.fastq.gz
      |--forward.fastq.gz
narrow-single-end/
   |--fastq/
      |--forward.fastq.gz
      |--forward.fastq.gz
paired-end-sequences/
   |--fastq/
      |--forward.fastq.gz
      |--forward.fastq.gz
4Apr2023_post_size
   |--fastq/
      |--forward.fastq.gz
      |--forward.fastq.gz
```

Following commands are run from the top level directory (i.e. the one containing barcodes.tsv and the broad-single-end, narrow-single-end, and paired-end-sequences folders. First off, importing the data into QIIME:
```
qiime tools import \
  --type MultiplexedPairedEndBarcodeInSequence \
  --input-path broad-single-end/fastq \
  --output-path broad-single-end/multiplexed-seqs.qza

qiime tools import \
  --type MultiplexedPairedEndBarcodeInSequence \
  --input-path narrow-single-end/fastq \
  --output-path narrow-single-end/multiplexed-seqs.qza
  
 qiime tools import \
  --type MultiplexedPairedEndBarcodeInSequence \
  --input-path paired-end-sequences/fastq \
  --output-path paired-end-sequences/multiplexed-seqs.qza
  
 qiime tools import \
  --type MultiplexedPairedEndBarcodeInSequence \
  --input-path 4Apr2023_post_size/fastq \
  --output-path 4Apr2023_post_size/multiplexed-seqs.qza
```

Demultiplexing the data
```
qiime cutadapt demux-paired \
--i-seqs broad-single-end/multiplexed-seqs.qza \
--m-forward-barcodes-file barcodes.tsv \
--m-forward-barcodes-column forwardindex \
--m-reverse-barcodes-file barcodes.tsv \
--m-reverse-barcodes-column reverseindex \
--o-per-sample-sequences broad-single-end/demultiplexed-seqs.qza \
--o-untrimmed-sequences broad-single-end/untrimmed-seqs.qza

qiime cutadapt demux-paired \
--i-seqs narrow-single-end/multiplexed-seqs.qza \
--m-forward-barcodes-file barcodes.tsv \
--m-forward-barcodes-column forwardindex \
--m-reverse-barcodes-file barcodes.tsv \
--m-reverse-barcodes-column reverseindex \
--o-per-sample-sequences narrow-single-end/demultiplexed-seqs.qza \
--o-untrimmed-sequences narrow-single-end/untrimmed-seqs.qza

qiime cutadapt demux-paired \
--i-seqs paired-end-sequences/multiplexed-seqs.qza \
--m-forward-barcodes-file barcodes.tsv \
--m-forward-barcodes-column forwardindex \
--m-reverse-barcodes-file barcodes.tsv \
--m-reverse-barcodes-column reverseindex \
--o-per-sample-sequences paired-end-sequences/demultiplexed-seqs.qza \
--o-untrimmed-sequences paired-end-sequences/untrimmed-seqs.qza

qiime cutadapt demux-paired \
--i-seqs 4Apr2023_post_size/multiplexed-seqs.qza \
--m-forward-barcodes-file barcodes.tsv \
--m-forward-barcodes-column forwardindex \
--m-reverse-barcodes-file barcodes.tsv \
--m-reverse-barcodes-column reverseindex \
--o-per-sample-sequences 4Apr2023_post_size/demultiplexed-seqs.qza \
--o-untrimmed-sequences 4Apr2023_post_size/untrimmed-seqs.qza
```

Removing primers and discarding reads where no primer was found
```
qiime cutadapt trim-paired \
--i-demultiplexed-sequences broad-single-end/demultiplexed-seqs.qza \
--p-front-f ^TCACCCAAAGCTGRARTTCTA \
--p-front-r ^CGGGTTGCTGGTTTCACG \
--p-discard-untrimmed \
--o-trimmed-sequences broad-single-end/demultiplexed-seqs-trimmed.qza

qiime cutadapt trim-paired \
--i-demultiplexed-sequences narrow-single-end/demultiplexed-seqs.qza \
--p-front-f ^TCACCCAAAGCTGRARTTCTA \
--p-front-r ^CGGGTTGCTGGTTTCACG \
--p-discard-untrimmed \
--o-trimmed-sequences narrow-single-end/demultiplexed-seqs-trimmed.qza

qiime cutadapt trim-paired \
--i-demultiplexed-sequences paired-end-sequences/demultiplexed-seqs.qza \
--p-front-f ^TCACCCAAAGCTGRARTTCTA \
--p-front-r ^CGGGTTGCTGGTTTCACG \
--p-discard-untrimmed \
--o-trimmed-sequences paired-end-sequences/demultiplexed-seqs-trimmed.qza

qiime cutadapt trim-paired \
--i-demultiplexed-sequences 4Apr2023_post_size/demultiplexed-seqs.qza \
--p-front-f ^TCACCCAAAGCTGRARTTCTA \
--p-front-r ^CGGGTTGCTGGTTTCACG \
--p-discard-untrimmed \
--o-trimmed-sequences 4Apr2023_post_size/demultiplexed-seqs-trimmed.qza
```

First, we'll have a look at the quality of the data
```
qiime demux summarize \
  --i-data broad-single-end/demultiplexed-seqs-trimmed.qza  \
  --o-visualization broad-single-end/demux-summary.qzv

qiime demux summarize \
  --i-data narrow-single-end/demultiplexed-seqs-trimmed.qza  \
  --o-visualization narrow-single-end/demux-summary.qzv
  
qiime demux summarize \
  --i-data paired-end-sequences/demultiplexed-seqs-trimmed.qza  \
  --o-visualization paired-end-sequences/demux-summary.qzv

# If you get an "Aborted" error for the following, first run (without the hash):
# echo "backend: Agg" > ~/.config/matplotlib/matplotlibrc
# As mentioned here: https://github.com/spacocha/PreheimLab_16S_SOPs/issues/2

qiime demux summarize \
 --i-data 4Apr2023_post_size/demultiplexed-seqs-trimmed.qza  \
 --o-visualization 4Apr2023_post_size/demux-summary.qzv
```

This 'extracts' the visualisation so we can look at it
```
qiime tools export \
  --input-path broad-single-end/demux-summary.qzv \
  --output-path broad-single-end/demux-summary-figures

qiime tools export \
  --input-path narrow-single-end/demux-summary.qzv \
  --output-path narrow-single-end/demux-summary-figures
  
qiime tools export \
  --input-path paired-end-sequences/demux-summary.qzv \
  --output-path paired-end-sequences/demux-summary-figures
  
qiime tools export \
 --input-path 4Apr2023_post_size/demux-summary.qzv \
 --output-path 4Apr2023_post_size/demux-summary-figures
```

We then download it to our computer so we can look at the outputs (remember scp commands have to run from your computer i.e. push/pull from there - need to have the broad-single-end, narrow-single-end and  paired-end-sequences folders set up before downloading
```
scp -r mahuika:/nesi/nobackup/uoo02423/eDNA/broad-single-end/demux-summary-figures ./broad-single-end
scp -r mahuika:/nesi/nobackup/uoo02423/eDNA/narrow-single-end/demux-summary-figures ./narrow-single-end
scp -r mahuika:/nesi/nobackup/uoo02423/eDNA/paired-end-sequences/demux-summary-figures ./paired-end-sequences
scp -r mahuika:/nesi/nobackup/uoo02423/eDNA/4Apr2023_post_size/demux-summary-figures ./4Apr2023_post_size

```

After checking them out we keep going on our analyses
```
qiime dada2 denoise-paired \
--i-demultiplexed-seqs broad-single-end/demultiplexed-seqs-trimmed.qza \
--p-trunc-len-f 0 \
--p-trunc-len-r 0 \
--o-table broad-single-end/feature-data.qza \
--o-representative-sequences broad-single-end/representative-sequences.qza \
--o-denoising-stats broad-single-end/denoising-stats.qza

qiime dada2 denoise-paired \
--i-demultiplexed-seqs narrow-single-end/demultiplexed-seqs-trimmed.qza \
--p-trunc-len-f 0 \
--p-trunc-len-r 0 \
--o-table narrow-single-end/feature-data.qza \
--o-representative-sequences narrow-single-end/representative-sequences.qza \
--o-denoising-stats narrow-single-end/denoising-stats.qza

qiime dada2 denoise-paired \
--i-demultiplexed-seqs paired-end-sequences/demultiplexed-seqs-trimmed.qza \
--p-trunc-len-f 0 \
--p-trunc-len-r 0 \
--o-table paired-end-sequences/feature-data.qza \
--o-representative-sequences paired-end-sequences/representative-sequences.qza \
--o-denoising-stats paired-end-sequences/denoising-stats.qza

qiime dada2 denoise-paired \
--i-demultiplexed-seqs 4Apr2023_post_size/demultiplexed-seqs-trimmed.qza \
--p-trunc-len-f 0 \
--p-trunc-len-r 0 \
--o-table 4Apr2023_post_size/feature-data.qza \
--o-representative-sequences 4Apr2023_post_size/representative-sequences.qza \
--o-denoising-stats 4Apr2023_post_size/denoising-stats.qza
```

Summarizing the feaure table data
```
qiime feature-table summarize \
  --i-table broad-single-end/feature-data.qza \
  --o-visualization broad-single-end/feature-data-vis.qzv \
  --m-sample-metadata-file barcodes.tsv 

qiime feature-table summarize \
  --i-table narrow-single-end/feature-data.qza \
  --o-visualization narrow-single-end/feature-data-vis.qzv \
  --m-sample-metadata-file barcodes.tsv 
  
qiime feature-table summarize \
  --i-table paired-end-sequences/feature-data.qza \
  --o-visualization paired-end-sequences/feature-data-vis.qzv \
  --m-sample-metadata-file barcodes.tsv

qiime feature-table summarize \
  --i-table 4Apr2023_post_size/feature-data.qza \
  --o-visualization 4Apr2023_post_size/feature-data-vis.qzv \
  --m-sample-metadata-file barcodes.tsv  
```

Exporting all of this to have a look
```
qiime tools export \
  --input-path broad-single-end/feature-data-vis.qzv \
  --output-path broad-single-end/feature-data-vis

qiime tools export \
  --input-path narrow-single-end/feature-data-vis.qzv \
  --output-path narrow-single-end/feature-data-vis
  
qiime tools export \
  --input-path paired-end-sequences/feature-data-vis.qzv \
  --output-path paired-end-sequences/feature-data-vis
  
qiime tools export \
  --input-path 4Apr2023_post_size/feature-data-vis.qzv \
  --output-path 4Apr2023_post_size/feature-data-vis
```

We then download it to our computer so we can look at the outputs
```
scp -r mahuika:/nesi/nobackup/uoo02423/eDNA/broad-single-end/feature-data-vis ./broad-single-end
scp -r mahuika:/nesi/nobackup/uoo02423/eDNA/narrow-single-end/feature-data-vis ./narrow-single-end
scp -r mahuika:/nesi/nobackup/uoo02423/eDNA/paired-end-sequences/feature-data-vis ./paired-end-sequences
scp -r mahuika:/nesi/nobackup/uoo02423/eDNA/4Apr2023_post_size/feature-data-vis ./4Apr2023_post_size
```

Next step is to tabulate the sequences
```
qiime feature-table tabulate-seqs \
  --i-data broad-single-end/representative-sequences.qza \
  --o-visualization broad-single-end/representative-sequences.qzv
  
qiime feature-table tabulate-seqs \
  --i-data narrow-single-end/representative-sequences.qza \
  --o-visualization narrow-single-end/representative-sequences.qzv  
  
qiime feature-table tabulate-seqs \
  --i-data paired-end-sequences/representative-sequences.qza \
  --o-visualization paired-end-sequences/representative-sequences.qzv

qiime feature-table tabulate-seqs \
  --i-data 4Apr2023_post_size/representative-sequences.qza \
  --o-visualization 4Apr2023_post_size/representative-sequences.qzv
```

This gives us the fasta sequences
```
qiime tools export \
  --input-path broad-single-end/representative-sequences.qzv \
  --output-path broad-single-end/representative-sequences
  
qiime tools export \
  --input-path narrow-single-end/representative-sequences.qzv \
  --output-path narrow-single-end/representative-sequences

qiime tools export \
  --input-path paired-end-sequences/representative-sequences.qzv \
  --output-path paired-end-sequences/representative-sequences

qiime tools export \
  --input-path 4Apr2023_post_size/representative-sequences.qzv \
  --output-path 4Apr2023_post_size/representative-sequences
```

We then download it to our computer so we can look at the outputs
```
scp -r mahuika:/nesi/nobackup/uoo02423/eDNA/broad-single-end/representative-sequences ./broad-single-end
scp -r mahuika:/nesi/nobackup/uoo02423/eDNA/narrow-single-end/representative-sequences ./narrow-single-end
scp -r mahuika:/nesi/nobackup/uoo02423/eDNA/paired-end-sequences/representative-sequences ./paired-end-sequences
scp -r mahuika:/nesi/nobackup/uoo02423/eDNA/4Apr2023_post_size/representative-sequences ./4Apr2023_post_size
```

To get the counts of each ASV per sample, need to tabulate the feature 
```
qiime metadata tabulate \
  --m-input-file broad-single-end/feature-data.qza  \
  --o-visualization broad-single-end/tabulate-feature.qzv
  
qiime tools export \
  --input-path broad-single-end/tabulate-feature.qzv \
  --output-path broad-single-end/tabulate-feature

scp -r mahuika:/nesi/nobackup/uoo02423/eDNA/broad-single-end/tabulate-feature ./broad-single-end

qiime metadata tabulate \
  --m-input-file narrow-single-end/feature-data.qza  \
  --o-visualization narrow-single-end/tabulate-feature.qzv
  
qiime tools export \
  --input-path narrow-single-end/tabulate-feature.qzv \
  --output-path narrow-single-end/tabulate-feature

scp -r mahuika:/nesi/nobackup/uoo02423/eDNA/narrow-single-end/tabulate-feature ./narrow-single-end

qiime metadata tabulate \
  --m-input-file paired-end-sequences/feature-data.qza  \
  --o-visualization paired-end-sequences/tabulate-feature.qzv
  
qiime tools export \
  --input-path paired-end-sequences/tabulate-feature.qzv \
  --output-path paired-end-sequences/tabulate-feature

scp -r mahuika:/nesi/nobackup/uoo02423/eDNA/paired-end-sequences/tabulate-feature ./paired-end-sequences

qiime metadata tabulate \
  --m-input-file 4Apr2023_post_size/feature-data.qza  \
  --o-visualization 4Apr2023_post_size/tabulate-feature.qzv
  
qiime tools export \
  --input-path 4Apr2023_post_size/tabulate-feature.qzv \
  --output-path 4Apr2023_post_size/tabulate-feature

scp -r mahuika:/nesi/nobackup/uoo02423/eDNA/4Apr2023_post_size/tabulate-feature ./4Apr2023_post_size
```

Final step within qiime is to get some QC on the whole process
```
qiime metadata tabulate \
  --m-input-file broad-single-end/denoising-stats.qza \
  --o-visualization broad-single-end/denoising-stats.qzv
  
qiime metadata tabulate \
  --m-input-file narrow-single-end/denoising-stats.qza \
  --o-visualization narrow-single-end/denoising-stats.qzv
  
qiime metadata tabulate \
  --m-input-file paired-end-sequences/denoising-stats.qza \
  --o-visualization paired-end-sequences/denoising-stats.qzv
  
qiime metadata tabulate \
  --m-input-file 4Apr2023_post_size/denoising-stats.qza \
  --o-visualization 4Apr2023_post_size/denoising-stats.qzv

qiime tools export \
  --input-path broad-single-end/denoising-stats.qzv \
  --output-path broad-single-end/denoising-stats
  
qiime tools export \
  --input-path narrow-single-end/denoising-stats.qzv \
  --output-path narrow-single-end/denoising-stats

qiime tools export \
  --input-path paired-end-sequences/denoising-stats.qzv \
  --output-path paired-end-sequences/denoising-stats

qiime tools export \
  --input-path 4Apr2023_post_size/denoising-stats.qzv \
  --output-path 4Apr2023_post_size/denoising-stats

```

And then, you guessed it, downloading these outputs
```
scp -r mahuika:/nesi/nobackup/uoo02423/eDNA/broad-single-end/denoising-stats ./broad-single-end
scp -r mahuika:/nesi/nobackup/uoo02423/eDNA/narrow-single-end/denoising-stats ./narrow-single-end
scp -r mahuika:/nesi/nobackup/uoo02423/eDNA/paired-end-sequences/denoising-stats ./paired-end-sequences
scp -r mahuika:/nesi/nobackup/uoo02423/eDNA/4Apr2023_post_size/denoising-stats ./4Apr2023_post_size
```

Using BLAST, checking for similarity of sequences and cetacean/humans/pigs/cow/mouse. Downloaded all complete cetacean refseq mitogeomes from NCBI, and added to this the refseq for humans, pigs, cows, and mice. Uploaded these to mahuika and indexed them (only need to do this the first time)
```
scp -r cetacean_refseq_mitogenome.fasta mahuika:/nesi/nobackup/uoo02423/eDNA/
makeblastdb -in cetacean_refseq_mitogenome.fasta -dbtype nucl
```

After this, carrying out BLAST search for each of the sequences identified in sequences.fasta:
```
no_lines=`wc -l broad-single-end/representative-sequences/sequences.fasta | awk '{ print $1 }'`

for i in `seq 1 2 $no_lines`; 
   do j=$((i+1));
   seqname=`head -n $i broad-single-end/representative-sequences/sequences.fasta | tail -n 1`;
   head -n $j broad-single-end/representative-sequences/sequences.fasta | tail -n 1 > tempseq;
   blastn -task blastn -db cetacean_refseq_mitogenome.fasta -query tempseq -outfmt 6 -evalue 0.05 -word_size 11 -gapopen 5 -gapextend 2 -penalty -3 -reward 2 | sort -k 11g > tempblast;
   echo $seqname `head -n 1 tempblast` `head -n 2 tempblast | tail -n 1` >> broad-single-end/blast_results.txt;
   rm tempseq;
   rm tempblast;
done 

scp -r mahuika:/nesi/nobackup/uoo02423/eDNA/broad-single-end/blast_results.txt broad-single-end/blast_results.txt


no_lines=`wc -l narrow-single-end/representative-sequences/sequences.fasta | awk '{ print $1 }'`

for i in `seq 1 2 $no_lines`; 
   do j=$((i+1));
   seqname=`head -n $i narrow-single-end/representative-sequences/sequences.fasta | tail -n 1`;
   head -n $j narrow-single-end/representative-sequences/sequences.fasta | tail -n 1 > tempseq;
   blastn -task blastn -db cetacean_refseq_mitogenome.fasta -query tempseq -outfmt 6 -evalue 0.05 -word_size 11 -gapopen 5 -gapextend 2 -penalty -3 -reward 2 | sort -k 11g > tempblast;
   echo $seqname `head -n 1 tempblast` `head -n 2 tempblast | tail -n 1` >> narrow-single-end/blast_results.txt;
   rm tempseq;
   rm tempblast;
done 

scp -r mahuika:/nesi/nobackup/uoo02423/eDNA/narrow-single-end/blast_results.txt narrow-single-end/blast_results.txt


no_lines=`wc -l paired-end-sequences/representative-sequences/sequences.fasta | awk '{ print $1 }'`

for i in `seq 1 2 $no_lines`; 
   do j=$((i+1));
   seqname=`head -n $i paired-end-sequences/representative-sequences/sequences.fasta | tail -n 1`;
   head -n $j paired-end-sequences/representative-sequences/sequences.fasta | tail -n 1 > tempseq;
   blastn -task blastn -db cetacean_refseq_mitogenome.fasta -query tempseq -outfmt 6 -evalue 0.05 -word_size 11 -gapopen 5 -gapextend 2 -penalty -3 -reward 2 | sort -k 11g > tempblast;
   echo $seqname `head -n 1 tempblast` `head -n 2 tempblast | tail -n 1` >> paired-end-sequences/blast_results.txt;
   rm tempseq;
   rm tempblast;
done 

scp -r mahuika:/nesi/nobackup/uoo02423/eDNA/paired-end-sequences/blast_results.txt paired-end-sequences/blast_results.txt


no_lines=`wc -l 4Apr2023_post_size/representative-sequences/sequences.fasta | awk '{ print $1 }'`

for i in `seq 1 2 $no_lines`; 
   do j=$((i+1));
   seqname=`head -n $i 4Apr2023_post_size/representative-sequences/sequences.fasta | tail -n 1`;
   head -n $j 4Apr2023_post_size/representative-sequences/sequences.fasta | tail -n 1 > tempseq;
   blastn -task blastn -db cetacean_refseq_mitogenome.fasta -query tempseq -outfmt 6 -evalue 0.05 -word_size 11 -gapopen 5 -gapextend 2 -penalty -3 -reward 2 | sort -k 11g > tempblast;
   echo $seqname `head -n 1 tempblast` `head -n 2 tempblast | tail -n 1` >> 4Apr2023_post_size/blast_results.txt;
   rm tempseq;
   rm tempblast;
done 

scp -r mahuika:/nesi/nobackup/uoo02423/eDNA/4Apr2023_post_size/blast_results.txt 4Apr2023_post_size/blast_results.txt

```
Next step, pull into R and compare proportion of cetacean reads between different runs across the different samples and in total
```
# qseqid sseqid pident length mismatch gapopen qstart qend sstart send evalue bitscore
```
Because the iseq runs are not long enough to span the expected fragment, the results from the iseq are a little difficult to interpret, so going back and denoising based on single end
```
qiime dada2 denoise-single \
--i-demultiplexed-seqs broad-single-end/demultiplexed-seqs-trimmed.qza \
--p-trunc-len 0 \
--o-table broad-single-end/single-end-feature-data.qza \
--o-representative-sequences broad-single-end/single-end-representative-sequences.qza \
--o-denoising-stats broad-single-end/single-end-denoising-stats.qza

qiime dada2 denoise-single \
--i-demultiplexed-seqs narrow-single-end/demultiplexed-seqs-trimmed.qza \
--p-trunc-len 0 \
--o-table narrow-single-end/single-end-feature-data.qza \
--o-representative-sequences narrow-single-end/single-end-representative-sequences.qza \
--o-denoising-stats narrow-single-end/single-end-denoising-stats.qza

qiime dada2 denoise-single \
--i-demultiplexed-seqs 4Apr2023_post_size/demultiplexed-seqs-trimmed.qza \
--p-trunc-len 0 \
--o-table 4Apr2023_post_size/single-end-feature-data.qza \
--o-representative-sequences 4Apr2023_post_size/single-end-representative-sequences.qza \
--o-denoising-stats 4Apr2023_post_size/single-end-denoising-stats.qza

qiime feature-table tabulate-seqs \
  --i-data broad-single-end/single-end-representative-sequences.qza \
  --o-visualization broad-single-end/single-end-representative-sequences.qzv

qiime feature-table tabulate-seqs \
  --i-data narrow-single-end/single-end-representative-sequences.qza \
  --o-visualization narrow-single-end/single-end-representative-sequences.qzv  

qiime feature-table tabulate-seqs \
  --i-data 4Apr2023_post_size/single-end-representative-sequences.qza \
  --o-visualization 4Apr2023_post_size/single-end-representative-sequences.qzv  

qiime tools export \
  --input-path broad-single-end/single-end-representative-sequences.qzv \
  --output-path broad-single-end/single-end-representative-sequences

qiime tools export \
  --input-path narrow-single-end/single-end-representative-sequences.qzv \
  --output-path narrow-single-end/single-end-representative-sequences
  
qiime tools export \
  --input-path 4Apr2023_post_size/single-end-representative-sequences.qzv \
  --output-path 4Apr2023_post_size/single-end-representative-sequences

scp -r mahuika:/nesi/nobackup/uoo02423/eDNA/broad-single-end/single-end-representative-sequences ./broad-single-end
scp -r mahuika:/nesi/nobackup/uoo02423/eDNA/narrow-single-end/single-end-representative-sequences ./narrow-single-end
scp -r mahuika:/nesi/nobackup/uoo02423/eDNA/4Apr2023_post_size/single-end-representative-sequences ./4Apr2023_post_size

qiime metadata tabulate \
  --m-input-file broad-single-end/single-end-feature-data.qza  \
  --o-visualization broad-single-end/single-end-tabulate-feature.qzv
  
qiime tools export \
  --input-path broad-single-end/single-end-tabulate-feature.qzv \
  --output-path broad-single-end/single-end-tabulate-feature
  
scp -r mahuika:/nesi/nobackup/uoo02423/eDNA/broad-single-end/single-end-tabulate-feature ./broad-single-end
  
qiime metadata tabulate \
  --m-input-file narrow-single-end/single-end-feature-data.qza  \
  --o-visualization narrow-single-end/single-end-tabulate-feature.qzv

qiime tools export \
  --input-path narrow-single-end/single-end-tabulate-feature.qzv \
  --output-path narrow-single-end/single-end-tabulate-feature  
  
scp -r mahuika:/nesi/nobackup/uoo02423/eDNA/narrow-single-end/single-end-tabulate-feature ./narrow-single-end

qiime metadata tabulate \
  --m-input-file 4Apr2023_post_size/single-end-feature-data.qza  \
  --o-visualization 4Apr2023_post_size/single-end-tabulate-feature.qzv

qiime tools export \
  --input-path 4Apr2023_post_size/single-end-tabulate-feature.qzv \
  --output-path 4Apr2023_post_size/single-end-tabulate-feature  
  
scp -r mahuika:/nesi/nobackup/uoo02423/eDNA/4Apr2023_post_size/single-end-tabulate-feature ./4Apr2023_post_size

no_lines=`wc -l broad-single-end/single-end-representative-sequences/sequences.fasta | awk '{ print $1 }'`

for i in `seq 1 2 $no_lines`; 
   do j=$((i+1));
   seqname=`head -n $i broad-single-end/single-end-representative-sequences/sequences.fasta | tail -n 1`;
   head -n $j broad-single-end/single-end-representative-sequences/sequences.fasta | tail -n 1 > tempseq;
   blastn -task blastn -db cetacean_refseq_mitogenome.fasta -query tempseq -outfmt 6 -evalue 0.05 -word_size 11 -gapopen 5 -gapextend 2 -penalty -3 -reward 2 | sort -k 11g > tempblast;
   echo $seqname `head -n 1 tempblast` `head -n 2 tempblast | tail -n 1` >> broad-single-end/single-end-blast_results.txt;
   rm tempseq;
   rm tempblast;
done 

scp -r mahuika:/nesi/nobackup/uoo02423/eDNA/broad-single-end/single-end-blast_results.txt broad-single-end/single-end-blast_results.txt


no_lines=`wc -l narrow-single-end/single-end-representative-sequences/sequences.fasta | awk '{ print $1 }'`

for i in `seq 1 2 $no_lines`; 
   do j=$((i+1));
   seqname=`head -n $i narrow-single-end/single-end-representative-sequences/sequences.fasta | tail -n 1`;
   head -n $j narrow-single-end/single-end-representative-sequences/sequences.fasta | tail -n 1 > tempseq;
   blastn -task blastn -db cetacean_refseq_mitogenome.fasta -query tempseq -outfmt 6 -evalue 0.05 -word_size 11 -gapopen 5 -gapextend 2 -penalty -3 -reward 2 | sort -k 11g > tempblast;
   echo $seqname `head -n 1 tempblast` `head -n 2 tempblast | tail -n 1` >> narrow-single-end/single-end-blast_results.txt;
   rm tempseq;
   rm tempblast;
done 

scp -r mahuika:/nesi/nobackup/uoo02423/eDNA/narrow-single-end/single-end-blast_results.txt narrow-single-end/single-end-blast_results.txt


no_lines=`wc -l 4Apr2023_post_size/single-end-representative-sequences/sequences.fasta | awk '{ print $1 }'`

for i in `seq 1 2 $no_lines`; 
   do j=$((i+1));
   seqname=`head -n $i 4Apr2023_post_size/single-end-representative-sequences/sequences.fasta | tail -n 1`;
   head -n $j 4Apr2023_post_size/single-end-representative-sequences/sequences.fasta | tail -n 1 > tempseq;
   blastn -task blastn -db cetacean_refseq_mitogenome.fasta -query tempseq -outfmt 6 -evalue 0.05 -word_size 11 -gapopen 5 -gapextend 2 -penalty -3 -reward 2 | sort -k 11g > tempblast;
   echo $seqname `head -n 1 tempblast` `head -n 2 tempblast | tail -n 1` >> 4Apr2023_post_size/single-end-blast_results.txt;
   rm tempseq;
   rm tempblast;
done 

scp -r mahuika:/nesi/nobackup/uoo02423/eDNA/4Apr2023_post_size/single-end-blast_results.txt 4Apr2023_post_size/single-end-blast_results.txt

```

Once downloaded on to my local computer, the file directory looked like the following (this is not a comprehensive list of the files, just where the ones we need for the following R-code were sitting):
```
cetacean_refseq_mitogenome.fasta
broad-single-end/
   |--blast_results.txt
   |--tabulate-feature
      |--metadata.tsv
   |--representative-sequences
      |--sequences.fasta
narrow-single-end/
   |--blast_results.txt
   |--tabulate-feature
      |--metadata.tsv
   |--representative-sequences
      |--sequences.fasta
paired-end-sequences/
   |--blast_results.txt
   |--tabulate-feature
      |--metadata.tsv
   |--representative-sequences
      |--sequences.fasta
```

I then used the R-code in this folder to look at the proportion of reads assigned as cetacean instead of other gunk. Using the sequence identfiers, the cetacean sequences could be extracted and manually blasted through the Web to more accurately assign them to species (instead of the limited mitogenome dataset we utilised).
