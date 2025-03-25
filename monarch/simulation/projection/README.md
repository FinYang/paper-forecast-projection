
# Simulation of using Projection on MonARCH

This folder contains the code using projection on MonARCH cluster using `targets`. 

Run `targets` from a job to submit jobs.

Use code below to sync between local folder and server.
``` zsh
# local to remote
rsync -auv --chmod=ugo=rwX -e ssh monarch/simulation/projection finy@monarch-dtn.erc.monash.edu:~/bw51/simulation/ --exclude data-raw --exclude output --exclude _targets --exclude local -h -h
# remote to local
rsync -auv -e ssh finy@monarch-dtn.erc.monash.edu:~/bw51/simulation/projection monarch/simulation/ --exclude _targets --exclude .future --exclude registry --exclude .RData --exclude error.err --exclude output.out -h -h
```

``` zsh
smux new-session --partition=comp,short --time=1-00:00:00 --ntasks=1 --mem=10G -J i -o i.out -e i.err
R
```

To initialise on MonARCH, run

```R
batchtools::makeRegistry()
```


Go to target deirectory

```R
setwd("~/bw51/simulation/projection")
```

Run
``` zsh
sbatch ~/bw51/simulation/projection/job.sh
```
```R
source("~/bw51/simulation/projection/run.R")
```

Save output
```R
source("~/bw51/simulation/projection/output.R")
```



