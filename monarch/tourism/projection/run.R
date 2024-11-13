#!/usr/bin/env Rscript

# This is a helper script to run the pipeline.
# Choose how to execute the pipeline below.
# See https://books.ropensci.org/targets/hpc.html
# to learn about your options.

# targets::tar_make()
# targets::tar_make_clustermq(workers = 2) # nolint
setwd("~/bw51/tourism/projection")
targets::tar_make_future(
  workers = 240,
  reporter = "timestamp_positives"
) # nolint
source("output.R")
