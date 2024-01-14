#!/bin/bash

# Submit the pipeline as a job with srun job.sh

# Modified from https://github.com/mschubert/clustermq/blob/master/inst/LSF.tmpl
# under the Apache 2.0 license:
#SBATCH --job-name=presecular_nettlefish
#SBATCH --output=bw51/simulation/projection/output.out
#SBATCH --error=bw51/simulation/projection/error.err
#SBATCH --mem-per-cpu=5G
#SBATCH --cpus-per-task=1
#SBATCH --partition=comp,short

# module load R # Comment out if R is not an environment module.
R CMD BATCH bw51/simulation/projection/run.R

# Removing .RData is recommended.
# rm -f .RData
