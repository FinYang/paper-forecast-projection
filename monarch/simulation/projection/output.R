if (isTRUE(Sys.getenv("KUBERNETES_SERVICE_HOST") != "")) {
  # on DSRI
  setwd(here::here("monarch/simulation/projection"))
} else {
  # on Monarch
  setwd("~/bw51/simulation/projection")
}
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

qs2::qs_save(tar_read(mse), "output/mse.qs2")

p <- tar_read(plot_mse)
gs(p, "plot_mse.png")
qs2::qs_save(p, "output/plot_mse.qs2")

names_byseries <- tar_objects(ends_with("_series"))
names_byseries <- names_byseries[!grepl("wls", names_byseries)]
names_byseries <- names_byseries[!grepl("iter", names_byseries)]
names_byseries <- names_byseries[!grepl("true", names_byseries)]
names_byseries <- names_byseries[
  !grepl("proj_var", names_byseries, fixed = TRUE)
]
names_byseries <- names_byseries[
  !grepl("pca_normal_switch_sd", names_byseries, fixed = TRUE)
]
for (na in names_byseries) {
  qs2::qs_save(tar_read_raw(na), file.path("output", paste0(na, ".qs2")))
}
for (d in dir("output", full.names = TRUE)) {
  if (tools::file_ext(d) == "qs") unlink(d)
}
