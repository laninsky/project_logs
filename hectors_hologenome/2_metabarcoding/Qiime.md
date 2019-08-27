### QIIME2
The second approach involved using Qiime2, following instructions at: https://otagoedna.github.io/getting_started_with_qiime2/

On NeSI there is a QIIME2 module (on boros you would need to `module load conda` and then `source activate qiime2-2019.7`)
```
module load QIIME2/2019.7 
```
When in interactive session you can run the following in order to allow tab complete of qiime commands:
```
source tab-qiime
```
The following commands are on 'toy data' - I'll need to adapt them to my own project.  

Importing the data into a qiime2 artifact
```
#!/bin/bash -e 
#SBATCH -A uoo02423
#SBATCH -J qiime 
#SBATCH -n 1
#SBATCH -c 1 
#SBATCH -t 1:00:00
#SBATCH --mem=3G
#SBATCH -D /nesi/nobackup/uoo02423/hectors/pilot_water_eDNA/qiime_workshop 
#SBATCH -N 1

module load QIIME2/2019.7 

qiime tools import \
  --type 'SampleData[SequencesWithQuality]' \
  --input-path run_2_manifest.txt \
  --output-path run_2_multiplex.qza \
  --input-format SingleEndFastqManifestPhred33V2
```

Summarize the output. Note: this step does not appear to be able to run on NeSI unless interactive mode.
```
qiime demux summarize \
  --i-data run_2_multiplex.qza \
  --o-visualization run_2_multiplex.qzv
```

Denoising step
```
#!/bin/bash -e 
#SBATCH -A uoo02423
#SBATCH -J qiime 
#SBATCH -n 1
#SBATCH -c 1 
#SBATCH -t 1:00:00
#SBATCH --mem=3G
#SBATCH -D /nesi/nobackup/uoo02423/hectors/pilot_water_eDNA/qiime_workshop 
#SBATCH -N 1

module load QIIME2/2019.7 

qiime dada2 denoise-single \
  --p-trim-left 13 \
  --p-trunc-len 150 \
  --i-demultiplexed-seqs run_2_multiplex.qza \
  --o-representative-sequences run_2_rep_seqs.qza \
  --o-table run_2_denoise_out.qza \
  --o-denoising-stats run_2_denoise.qza
```

Post denoising visualisation. This part works as part of a slurm script
```
qiime metadata tabulate \
  --m-input-file run_2_denoise.qza \
  --o-visualization run_2_denoise.qzv
```
These steps do not work as part of a slurm script - have to execute in interactive mode
```
qiime feature-table summarize \
  --i-table run_2_denoise_out.qza \
  --o-visualization run_2_denoise_out.qzv \
  --m-sample-metadata-file sample-metadata.tsv

qiime feature-table tabulate-seqs \
  --i-data run_2_rep_seqs.qza \
  --o-visualization run_2_rep_seq.qzv
```

Using Naive Bayes (machine learning) to classify taxonomy
```
#!/bin/bash -e 
#SBATCH -A uoo02423
#SBATCH -J qiime 
#SBATCH -n 1
#SBATCH -c 1 
#SBATCH -t 1:00:00
#SBATCH --mem=3G
#SBATCH -D /nesi/nobackup/uoo02423/hectors/pilot_water_eDNA/qiime_workshop 
#SBATCH -N 1

module load QIIME2/2019.7 

qiime feature-classifier classify-sklearn \
  --i-classifier references/gg-13-8-99-515-806-nb-classifier.qza \
  --i-reads run_2_rep_seqs.qza \
  --o-classification run_2_taxonomy.qza
```
Visualizing the taxonomic results - first tabulating
```
#!/bin/bash -e 
#SBATCH -A uoo02423
#SBATCH -J qiime 
#SBATCH -n 1
#SBATCH -c 1 
#SBATCH -t 1:00:00
#SBATCH --mem=3G
#SBATCH -D /nesi/nobackup/uoo02423/hectors/pilot_water_eDNA/qiime_workshop 
#SBATCH -N 1

module load QIIME2/2019.7 

qiime metadata tabulate \
  --m-input-file run_2_taxonomy.qza \
  --o-visualization run_2_taxonomy_viz.qzv
```

Then creating barplots
```
#!/bin/bash -e 
#SBATCH -A uoo02423
#SBATCH -J qiime 
#SBATCH -n 1
#SBATCH -c 1 
#SBATCH -t 1:00:00
#SBATCH --mem=3G
#SBATCH -D /nesi/nobackup/uoo02423/hectors/pilot_water_eDNA/qiime_workshop 
#SBATCH -N 1

module load QIIME2/2019.7 

qiime taxa barplot \
  --i-table run_2_denoise_out.qza \
  --i-taxonomy run_2_taxonomy.qza \
  --m-metadata-file sample-metadata.tsv \
  --o-visualization run_2_taxonomy_barplots.qzv
```
After this, redid all the steps for run_1 up to just before "Using Naive Bayes (machine learning) to classify taxonomy". Then merged run_1 and run_2 results
```
#!/bin/bash -e 
#SBATCH -A uoo02423
#SBATCH -J qiime 
#SBATCH -n 1
#SBATCH -c 1 
#SBATCH -t 1:00:00
#SBATCH --mem=3G
#SBATCH -D /nesi/nobackup/uoo02423/hectors/pilot_water_eDNA/qiime_workshop 
#SBATCH -N 1

qiime feature-table merge \
  --i-tables run_1_denoise_out.qza \
  --i-tables run_2_denoise_out.qza\
  --o-merged-table combined_denoise_out_table.qza

qiime feature-table merge-seqs \
  --i-data run_1_rep_seqs.qza \
  --i-data run_2_rep_seqs.qza \
  --o-merged-data combined_rep_seqs.qza
```

qiime feature-table summarize \
  --i-table combined_denoise_out_table.qza \
  --o-visualization {COMBINED-TABLE_VIZ}.qzv \
  --m-sample-metadata-file sample-metadata.tsv



For visualization of Qiime2 results, can drag and drop \*.vz files to:  
https://view.qiime2.org/  
In order to use these visualization results, need to add metadata to Sample Metadata (for anything we might want to display/sort by)
