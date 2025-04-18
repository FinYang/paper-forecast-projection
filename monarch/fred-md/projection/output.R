setwd("~/bw51/fred-md/projection")
library(tidyverse)
library(targets)


gs <- function(p = last_plot(), ...) {
  ggsave(
    file.path("output", ...),
    # plot = tar_read(plot_mse),
    plot = p,
    units = "px",
    width = 1920,
    height = 1080,
    scale = 2,
    limitsize = FALSE
  )
}

# qs::qsave(tar_meta(), "output/tar_meta.qs")
qs::qsave(tar_read(mse), "output/mse.qs")
gs(tar_read(plot_mse), "plot_mse.png")
gs(tar_read(plot_mse_scale), "plot_mse_scale.png")
qs::qsave(tar_read(plot_mse), "output/plot_mse.qs")

names_byseries <- tar_objects(ends_with("_series"))
names_byseries <- names_byseries[!grepl("ets", names_byseries)]
names_byseries <- names_byseries[!grepl("pca_normal_switch_sd", names_byseries, fixed = TRUE)]
names_byseries <- names_byseries[!(grepl("dfm", names_byseries, fixed = TRUE) &
  grepl("switch", names_byseries, fixed = TRUE))]
for (na in names_byseries) {
  qs::qsave(tar_read_raw(na), file.path("output", paste0(na, ".qs")))
}
