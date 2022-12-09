Based on Debbie's protocol, the basic steps (following amplification with the control region primers) are to denoise/demultiplex with dada2 in qiime, with no filtering other than to trim primers. Debbie then does a blast search and extracts the top 5 hits, and pulls the cetaceans from that, as well as the humans/pigs/cow/mouse to see what those are doing. This ends up giving a table of ASV counts, a fasta file of all ASVs, a blast output, and a cetacean-only top hit only blast summary.

```
# Need to load Python/3.10.5 otherwise QIIME2 errors
# because of numpy errors
module load Python/3.10.5-gimkl-2022a

# Loading QIIME2
module load QIIME2/2022.2

# Enabling tab completion
source tab-qiime

```
