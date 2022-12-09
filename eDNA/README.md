Based on Debbie's protocol, the basic steps (following amplification with the control region primers) are to denoise/demultiplex with dada2 in qiime, with no filtering other than to trim primers. Debbie then does a blast search and extracts the top 5 hits, and pulls the cetaceans from that, as well as the humans/pigs/cow/mouse to see what those are doing. This ends up giving a table of ASV counts, a fasta file of all ASVs, a blast output, and a cetacean-only top hit only blast summary.

```
# Need to load Python/3.10.5 otherwise QIIME2 errors
# because of numpy errors
module load Python/3.10.5-gimkl-2022a

# Loading QIIME2
module load QIIME2/2022.2

# Enabling tab completion
source tab-qiime

# QIIME expects data to be called "forward.fastq.gz" 
# and "reverse.fastq.gz", therefore files need to be renamed e.g.
mv Undetermined_S0_L001_R1_001.fastq.gz fastq/forward.fastq.gz
mv Undetermined_S0_L001_R2_001.fastq.gz fastq/reverse.fastq.gz

# Our files are from paired end sequencing, whether or not they 
# are sequenced in a way the reads overlap (e.g. for our frag, miseq 
# PE 250 is needed), so this is the initial pipeline followed for QCing
# the reads. fastq for each run is in its own parent folder 
# e.g. broad-single-end  narrow-single-end  paired-end-sequences

# Importing the data into QIIME
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

# Getting a barcodes file together (barcodes.tsv)
# (ditch comments at beginning)
#sampleid        forwardindex    reverseindex
#BP_S_BD01       AACAAGCC        GAGCTTAC
#BP_S_HD_021     GGAATGAG        GAGCTTAC
#BP_N_HD_027     AACAAGCC        TTACCGCT
#TIM_HD_043      GGAATGAG        TTACCGCT

# Demultiplexing the data
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

# Removing primers and discarding reads where no primer was found
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

# For data off the iseq, the reads will not overlap
# so we will go with single-end for the rest
# of the pipeline

# First, we'll have a look at the quality of the
# data
qiime demux summarize \
  --i-data broad-single-end/demultiplexed-seqs-trimmed.qza  \
  --o-visualization broad-single-end/demux-summary.qzv

qiime demux summarize \
  --i-data narrow-single-end/demultiplexed-seqs-trimmed.qza  \
  --o-visualization narrow-single-end/demux-summary.qzv

# This 'extracts' the visualisation so we can
# look at it
qiime tools export \
  --input-path broad-single-end/demux-summary.qzv \
  --output-path broad-single-end/demux-summary-figures

qiime tools export \
  --input-path narrow-single-end/demux-summary.qzv \
  --output-path narrow-single-end/demux-summary-figures

# We then download it to our computer so we can look
# at the outputs (remember scp commands have to run
# from your computer i.e. push/pull from there)
scp -r mahuika:/nesi/nobackup/uoo02423/eDNA/broad-single-end/demux-summary-figures ./broad-single-end
scp -r mahuika:/nesi/nobackup/uoo02423/eDNA/narrow-single-end/demux-summary-figures ./narrow-single-end


# After checking them out we keep going on our analyses
qiime dada2 denoise-paired \
--i-demultiplexed-seqs broad-single-end/demultiplexed-seqs-trimmed.qza \
--p-trunc-len-f 0 \
--p-trunc-len-r 0 \
--o-table feature-data.qza \
--o-representative-sequences broad-single-end/representative-sequences.qza \
--o-denoising-stats broad-single-end/denoising-stats.qza

qiime dada2 denoise-paired \
--i-demultiplexed-seqs narrow-single-end/demultiplexed-seqs-trimmed.qza \
--p-trunc-len-f 0 \
--p-trunc-len-r 0 \
--o-table feature-data.qza \
--o-representative-sequences narrow-single-end/representative-sequences.qza \
--o-denoising-stats narrow-single-end/denoising-stats.qza

```
