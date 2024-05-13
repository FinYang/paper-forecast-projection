setwd("~/bw51/simulation/projection")
library(tidyverse)
library(targets)


gs <- function(p = last_plot(), ...)
  ggsave(
    file.path("output", ...),
    # plot = tar_read(plot_mse),
    plot = p,
    units = "px",
    width = 1920,
    height = 1080,
    scale = 2,
    limitsize = FALSE)

qs::qsave(tar_read(mse), "output/mse.qs")
qs::qsave(tar_read(mse_exp), "output/mse_exp.qs")

p <- tar_read(plot_mse_exp)
gs(p, "plot_mse_exp.png")
qs::qsave(p, "output/plot_mse_exp.qs")

names_byseries <- tar_objects(ends_with("_series"))
names_byseries <- names_byseries[!grepl("wls", names_byseries)]
names_byseries <- names_byseries[!grepl("iter", names_byseries)]
names_byseries <- names_byseries[!grepl("true", names_byseries)]
names_byseries <- names_byseries[!grepl("proj_var", names_byseries, fixed = TRUE)]
for(na in names_byseries) {
  qs::qsave(tar_read_raw(na), file.path("output", paste0(na, ".qs")))
}

