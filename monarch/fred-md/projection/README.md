
# Projection using FRED-MD on MonARCH

This folder contains the code using projection on MonARCH cluster using `targets`. 

Run `targets` from a job to submit jobs.

Use code below to sync between local folder and server.
```
# local to remote
rsync -auv --chmod=ugo=rwX -e ssh monarch/fred-md/projection finy@monarch-dtn.erc.monash.edu:~/bw51/fred-md/ --exclude data-raw --exclude output --exclude _targets --exclude local -h -h
# remote to local
rsync -auv -e ssh finy@monarch-dtn.erc.monash.edu:~/bw51/fred-md/projection monarch/fred-md/ --exclude _targets --exclude .future --exclude registry --exclude .RData --exclude error.err --exclude output.out -h -h
```

To initialise on MonARCH, run

```r
setwd("~/bw51/fred-md/projection")
batchtools::makeRegistry()
```

The directory structure on MonARCH is planned to be:

```
bw51
  + fred-md/prjection
    + R (R functions)
    + output (folder containing the output data)
    + slurm_output (folder containing slurm output log files) 
    + slurm_error (folder containing slurm error files) 
    - _targets.R
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


