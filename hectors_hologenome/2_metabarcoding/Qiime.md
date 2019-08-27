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

Summarize the output (make sure to use the `--output-dir` option if not running interactively)
```
qiime demux summarize \
  --i-data run_2_multiplex.qza \
  --output-dir run_2_multiplex.qzv
```





For visualization of Qiime2 results, can drag and drop \*.vz files to:  
https://view.qiime2.org/  
In order to use these visualization results, need to add metadata to Sample Metadata (for anything we might want to display/sort by)
