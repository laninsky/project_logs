#!/bin/bash -e 
#SBATCH -A uoo02423
#SBATCH -J makefastq_L2
#SBATCH -n 1
#SBATCH -c 36 
#SBATCH -t 5:00:00
#SBATCH --mem=105G
#SBATCH -D /nesi/nobackup/uoo02423/10x_run 
#SBATCH -N 1
#SBATCH --hint=nomultithread

module load bcl2fastq2/2.20.0-gimkl-2018b
module load Supernova/2.1.1

supernova mkfastq --run=200702_A00488_0073_BHTL7YDMXX-lane2 --id=lane2_fastq --samplesheet=10x_samplesheet_L2.csv --qc --jobmode=local --localcores=36 --localmem=105 --ignore-dual-index 
