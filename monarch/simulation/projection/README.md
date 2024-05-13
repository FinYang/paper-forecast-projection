
# Simulation of using Projection on MonARCH

This folder contains the code using projection on MonARCH cluster using `targets`. 

Run `targets` from a job to submit jobs.

Use code below to sync between local folder and server.
```
# local to remote
rsync -auv --chmod=ugo=rwX -e ssh monarch/simulation/projection finy@monarch-dtn2.erc.monash.edu:~/bw51/simulation/ --exclude data-raw --exclude output --exclude _targets --exclude local -h -h
# remote to local
rsync -auv -e ssh finy@monarch-dtn2.erc.monash.edu:~/bw51/simulation/projection monarch/simulation/ --exclude _targets --exclude .future --exclude registry --exclude .RData --exclude error.err --exclude output.out -h -h
```

To initialise on MonARCH, run

```R
batchtools::makeRegistry()
```

The directory structure on MonARCH is planned to be:

```
bw51
  + simulation/prjection
    + data (folder containing data the script needs to run)
    + R (R functions)
    + output (folder containing the output data)
    + slurm_output (folder containing slurm output log files) 
    + slurm_error (folder containing slurm error files) 
    - _targets.R
```

Locally there are also files to prepare raw data and upload them to MonARCH

```
    + data-raw
```

Run
```
sbatch ~/bw51/simulation/projection/job.sh
```
```R
source("~/bw51/simulation/projection/run.R")
```

Save output
```R
source("~/bw51/simulation/projection/output.R")
```


Clean up slurm error and output files

```
unlink(dir("bw51/simulation/projection/slurm_output", full.name = TRUE))
unlink(dir("bw51/simulation/projection/slurm_error", full.name = TRUE))
```


