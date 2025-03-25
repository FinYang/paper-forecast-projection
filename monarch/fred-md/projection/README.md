
# Projection using FRED-MD on MonARCH

This folder contains the code using projection on MonARCH cluster using `targets`. 

Run `targets` from a job to submit jobs.

Use code below to sync between local folder and server.
``` zsh
# local to remote
rsync -auv --chmod=ugo=rwX -e ssh monarch/fred-md/projection finy@monarch-dtn.erc.monash.edu:~/bw51/fred-md/ --exclude data-raw --exclude output --exclude _targets --exclude local -h -h
# remote to local
rsync -auv -e ssh finy@monarch-dtn.erc.monash.edu:~/bw51/fred-md/projection monarch/fred-md/ --exclude _targets --exclude .future --exclude registry --exclude .RData --exclude error.err --exclude output.out -h -h
```

``` zsh
smux new-session --partition=comp,short --time=1-00:00:00 --ntasks=1 --mem=10G -J i -o i.out -e i.err
R
```

To initialise on MonARCH, run

```r
setwd("~/bw51/fred-md/projection")
batchtools::makeRegistry()
```


Go to directory
```R
setwd("~/bw51/fred-md/projection")
```

Run
```
sbatch ~/bw51/fred-md/projection/job.sh
```
```R
source("~/bw51/fred-md/projection/run.R")
```

Save output
```R
source("~/bw51/fred-md/projection/output.R")
```


Clean up slurm error and output files

```
unlink(dir("bw51/fred-md/projection/slurm_output", full.name = TRUE))
unlink(dir("bw51/fred-md/projection/slurm_error", full.name = TRUE))
```


