#!/bin/bash -e

#SBATCH -A uoo00105 
#SBATCH -J bgc
#SBATCH --ntasks 1
#SBATCH -c 1
#SBATCH -t 72:00:00
#SBATCH --mem=3G
#SBATCH -D /nesi/nobackup/uoo00105/chickadees/bgc 
#SBATCH --mail-type=ALL
#SBATCH --mail-user=alana.alexander@otago.ac.nz
#SBATCH -N 1
#SBATCH --hint=nomultithread
#SBATCH --partition=large

module load HDF5/1.10.5-gimkl-2018b
module load GSL/2.4-GCC-7.4.0
export PATH=/nesi/nobackup/uoo00105/chickadees/bin/bgcdist:$PATH

bgc -a black_capped.txt -b Carolina.txt -h admixed.txt -M genetic_map.txt -O 2 -x 50000 -n 25000 -t 5 -p 1 -q 1 -i 0 -N 1 -E 0.0001 -m 1 -d 1 -s 1 -o 0 -I 0 -D 1252.497 -T 0 -u 0.1 -g 0.08 -z 0.05 -e 0.2

