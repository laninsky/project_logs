Originally, I used cutadapt in the following loop:
https://github.com/laninsky/project_logs/blob/master/harbour_seals/nuclear_genome.Md#trimming-raw-reads-and-assembling-them-against-the-northern-fur-seal-reference-example-for-sample_19-d135

However, there appeared to be adapter/adaptor dimer/concatamer, potentially involving the truseq process controls, so I switched the loop to using BBDuk instead, as this can search for artifacts like that.

```
for j in *R1*.fastq.gz;
  do trimname1=`echo $j | sed 's/.fastq/.trimmed.fastq/g'`;
  trimname2=`echo $trimname1 | sed 's/R1/R2/g'`;
  reverse=`echo $j | sed 's/R1/R2/g'`;
  cutadapt -j 16 -a AGATCGGAAGAGC -A AGATCGGAAGAGC -o $trimname1 -p $trimname2 $j $reverse -q 5,15 -m 25 >> ../$i.cutadapt.log;
done;
```
