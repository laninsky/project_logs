## 1. Extracting the data
We got `*.bcl` data back from the sequencer, so first needed to generate the fastq files based off these. I did this separately for each lane after first unzipping (2hrs and 3GB of RAM was enough):
```
tar -zvxf 200702_A00488_0073_BHTL7YDMXX-lane1.tar.gz
tar -zvxf 200702_A00488_0073_BHTL7YDMXX-lane2.tar.gz
```
The "make fastq" scripts are [makefastq.sh](https://github.com/laninsky/project_logs/blob/master/hectors_hologenome/3_10x_genome_assembly/makefastq.sh) and [makefastq_L2.sh](https://github.com/laninsky/project_logs/blob/master/hectors_hologenome/3_10x_genome_assembly/makefastq_L2.sh), and the necessary csv files are [10x_samplesheet.csv](https://github.com/laninsky/project_logs/blob/master/hectors_hologenome/3_10x_genome_assembly/10x_samplesheet.csv) and [10x_samplesheet_L2.csv](https://github.com/laninsky/project_logs/blob/master/hectors_hologenome/3_10x_genome_assembly/10x_samplesheet_L2.csv)
