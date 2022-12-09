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
  
```
