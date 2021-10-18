## Extracting and cleaning data

```
# Extracting the data
tar -zvxf 201126_FD09251656.tar.gz
```

## Using ORTHOSKIM to extract mtDNA contigs from the WGS data
### Installation following instructions at https://github.com/cpouchon/ORTHOSKIM
```
# Loading conda for the conda install
module load Miniconda3/4.10.3

# Cloning the ORTHOSKIM repo 
wget https://github.com/cpouchon/ORTHOSKIM/archive/master.zip

# Unzipping the ORTHOSKIM repo and navigating into this directory
unzip master.zip
cd ./ORTHOSKIM-master/

# Creating the ORTHOSKIM conda environment and activating it
conda env create --prefix /nesi/nobackup/uoo02423/bin/ORTHOSKIM --file orthoskim-env.yml
conda activate /nesi/nobackup/uoo02423/bin/ORTHOSKIM
```
### Getting input files together following instructions at https://github.com/cpouchon/ORTHOSKIM
Modified [../config_orthoskim.txt] following instructions

```
/nesi/nobackup/uoo02423/bin/ORTHOSKIM-master/orthoskim
```
