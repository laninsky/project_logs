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
prefetch -v SRP292185
# From Margaryan et al. (2021)
prefetch -v SRP254083
```
