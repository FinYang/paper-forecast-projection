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

qs::qsave(tar_meta(), "output/tar_meta.qs")
qs::qsave(tar_read(mse), "output/mse.qs")
gs(tar_read(plot_mse), "plot_mse.png")

qs::qsave(tar_read(mse_exp), "output/mse_exp.qs")
p <- tar_read(plot_mse_exp)
gs(p, "plot_mse_exp.png")
qs::qsave(p, "output/plot_mse_exp.qs")



qs::qsave(tar_read(check_fc), "output/check_fc.qs")
gs(tar_read(plot_check_fc), "plot_check_fc.png")


names_byseries <- tar_objects(ends_with("_series"))
for(na in names_byseries) {
  qs::qsave(tar_read_raw(na), file.path("output", paste0(na, ".qs")))
}

names_bycv <- tar_objects(ends_with("_cv"))
for(na in names_bycv) {
  qs::qsave(tar_read_raw(na), file.path("output", paste0(na, ".qs")))
}

qs::qsave(tar_read(B_true), "output/B_true.qs")
