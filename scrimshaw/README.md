## Obtaining georeferenced data
Loading SRAtoolkit
```
# Load required module
module load sratoolkit/3.0.2
# Configure dump location to appropriate path
vdb-config.3.0.2 -i
```

Fetching data:
```
# From Violi et al. (2023)
prefetch -v SRP423249
# From Wang et al. (2021)
prefetch -v SRR13024481
# From Margaryan et al. (2021)
prefetch -v SRR11679510
# From Li et al. (2019)
prefetch -v SRR6192927 --max-size 100000000
prefetch -v SRR6192928 --max-size 100000000
prefetch -v SRR6192929 --max-size 100000000
prefetch -v SRR6192931 --max-size 100000000
# Fan et al. (2019)
prefetch -v SRR6192930 --max-size 100000000
prefetch -v SRR6192932 --max-size 100000000
prefetch -v SRR6192933 --max-size 100000000
prefetch -v SRR6117289 --max-size 100000000
prefetch -v SRR6117288 --max-size 100000000
prefetch -v SRR6117287 --max-size 100000000


```
