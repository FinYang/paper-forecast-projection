# Forecast Projection

The source code required to replicate the outcome is kept in the monarch directory, designed for execution on the MonARCH server.

Each data set and simulation has its own folder. In general, as described in their corresponding README file, the workflow is

1. Upload data, the [`targets`](https://books.ropensci.org/targets/) pipeline and functions onto the server;
1. Submit a job with the `job.sh` file, or run the pipeline in a interactive R session with `run.R`;
    - For the parallel computing of `targets` to work properly, it needs to be correctly configured and initialised. See https://books.ropensci.org/targets/hpc.html#future
    - The `run.R` or the job will also execute the `output.R` file, which saves the relevant results in the output folder.
1. Download the folder from the server, including the output.

The manuscript can be compiled by the `flap.qmd` file, which will also execute `flap.R` that reads the results in the output folder to create and insert plots to the manuscript.

The references of packages and R are stored in `references-pkg.bib` and other references are stored in `references-key.bib`. 

## Quarto Extensions

* The proofs are put into the appendix using the [`latex-environment`](https://github.com/quarto-ext/latex-environment) extension and the `apxproof` latex package.
* The template is the [Monash quarto working paper template](https://github.com/numbats/monash-quarto-workingpaper).
* The abstract is written as a normal section in the main text instead of in YAML using the [`abstract-section`](https://github.com/pandoc-ext/abstract-section) extension.
