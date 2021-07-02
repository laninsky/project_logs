## 1. Extracting the data
We got `*.bcl` data back from the sequencer, so first needed to generate the fastq files based off these. I did this separately for each lane after first unzipping (2hrs and 3GB of RAM was enough):
```
tar -zvxf 200702_A00488_0073_BHTL7YDMXX-lane1.tar.gz
tar -zvxf 200702_A00488_0073_BHTL7YDMXX-lane2.tar.gz
```
The "make fastq" scripts are [makefastq.sh](makefastq.sh) and [makefastq_L2.sh](makefastq_L2.sh), and the necessary csv files are [10x_samplesheet.csv](10x_samplesheet.csv) and [10x_samplesheet_L2.csv](10x_samplesheet_L2.csv)

## 2. Running the supernova assembly
We ran our assemblies a few different ways, an initial `maui` and `hectors` run to gauge the depth effective run depth etc. Based on these, additional runs were done at 56x raw depth, and 42x effective depth. Following the initial supernova run, we extracted the two pseudohaploid genomes for each run using mkoutput files.
