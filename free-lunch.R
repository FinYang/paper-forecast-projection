## ---- path ----

current_path <- function(...){
  path <- file.path(...)
  normalizePath(path)
}
targets::tar_source(current_path("R"))

pa <- function(...) current_path("monarch", ...)

## ---- library ----
library(tidyverse)
library(knitr)
library(kableExtra)

cb_palette_grey <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
cb_palette_black <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

## ---- fred-md ----
pa_fredmd <- function(...) pa("fred-md", "projection", "output", ...)
m <- 122

## ---- fig-fred-md-line ----
mse <- qs::qread(pa_fredmd("mse.qs"))
plot_mse <- mse %>%
  filter(!model %in% "ets",
         !grepl("ets", Phi),
         h %in% c(1, 6, 12)) %>%
  ggplot(aes(x = p, y = value,
             colour = model,
             linetype = paste(proj, Phi, sep = "."))) +
  geom_vline(xintercept = m) +
  geom_line() +
  geom_hline(data = \(df) filter(df, !proj),
             aes(yintercept = value,
                 colour = model,
                 linetype = paste(proj, Phi, sep = "."))) +
  # facet_wrap("h", scales = "free", labeller = label_both) +
  facet_grid(rows = "h", scales = "free", labeller = label_both) +
  ylab("MSE") +
  scale_linetype_manual(
    name = "Component",
    values = c(
      "TRUE.PCA_normal" = "solid",
      "FALSE.NA" = "dashed",
      "TRUE.normal" = "longdash"
    ),
    labels = c(
      "TRUE.PCA_normal" = "PCA+Norm.",
      "FALSE.NA" = "No Proj.",
      "TRUE.normal" = "Norm."
    )) +
  scale_color_manual(
    name = "Model",
    values = cb_palette_grey[c(7, 6)],
    labels = c(
      "arima" = "ARIMA",
      "dfm" = "DFM"))
plot_mse
## ---- fig-fred-md-mcb-series ----

mse_arima_series <- qs::qread(pa_fredmd("mse_arima_series.qs"))
mse_dfm_series <- qs::qread(pa_fredmd("mse_dfm_series.qs"))
mse_proj_arima_normal_series <- qs::qread(pa_fredmd("mse_proj_arima_normal_series.qs"))
mse_proj_arima_pca_normal_series <- qs::qread(pa_fredmd("mse_proj_arima_pca_normal_series.qs"))
mse_proj_dfm_pca_normal_series <- qs::qread(pa_fredmd("mse_proj_dfm_pca_normal_series.qs"))

mse_proj_arima_pca_normal_series_1 <- mse_proj_arima_pca_normal_series[[1]]
mse_proj_arima_normal_series_1 <- mse_proj_arima_normal_series[[1]]
mse_proj_dfm_pca_normal_series_1 <- mse_proj_dfm_pca_normal_series[[1]]
mse_proj_arima_pca_normal_series_m <- mse_proj_arima_pca_normal_series[[m]]
mse_proj_arima_normal_series_m <- mse_proj_arima_normal_series[[m]]
mse_proj_dfm_pca_normal_series_m <- mse_proj_dfm_pca_normal_series[[m]]

name_vec <- c(
  mse_arima_series = "ARIMA-Base",
  mse_dfm_series = "DFM-Base",
  mse_proj_arima_pca_normal_series_1 = "ARIMA-PCA+Norm-1",
  mse_proj_arima_normal_series_1 = "ARIMA-Norm-1",
  mse_proj_dfm_pca_normal_series_1 = "DFM-PCA+Norm-1",
  mse_proj_arima_pca_normal_series_m = "ARIMA-PCA+Norm-m",
  mse_proj_arima_normal_series_m = "ARIMA-Norm-m",
  mse_proj_dfm_pca_normal_series_m = "DFM-PCA+Norm-m"
)


mse_series_mat_ls <- lst(!!!syms(names(name_vec))) %>%
  list2array() %>%
  aperm(c(1, 3, 2)) %>%
  array2list()
# tsutils::nemenyi(mse_series_mat_ls[[1]], plottype = "vmcb")


mcb_df_ls <-  mse_series_mat_ls %>%
  lapply(tsutils::nemenyi, plottype = "none") %>%
  lapply(\(x) as.data.frame(x) %>%
           mutate(name = name_vec[name]) %>%
           mutate(name = paste(name, format(round(value, 2), width = 5, nsmall = 2))) %>%
           mutate(col =
                    (u[[which.min(value)]] <=u &
                       u[[which.min(value)]] >=l) |
                    (u[[which.min(value)]] >=u &
                       l[[which.min(value)]] <=u)
           ))

plot_mcb_series <- mcb_df_ls %>%
  bind_rows(.id = "h") %>%
  mutate(h = as.integer(h)) %>%
  filter(h %in% c(1, 6, 12)) %>%
  mutate(name = factor(name,unique(name), ordered = TRUE)) %>%
  group_by(h) %>%
  plot_mcb("Base") +
  facet_wrap("h", scales = "free", labeller = label_both, ncol = 1,
             strip.position = "right")
plot_mcb_series
## ---- fig-fred-md-mcb-cv ----
# ?tsutils::nemenyi
# dir(pa_fredmd())

mse_arima_cv <- qs::qread(pa_fredmd("mse_arima_cv.qs"))
mse_dfm_cv <- qs::qread(pa_fredmd("mse_dfm_cv.qs"))
mse_proj_arima_normal_cv <- qs::qread(pa_fredmd("mse_proj_arima_normal_cv.qs"))
mse_proj_arima_pca_normal_cv <- qs::qread(pa_fredmd("mse_proj_arima_pca_normal_cv.qs"))
mse_proj_dfm_pca_normal_cv <- qs::qread(pa_fredmd("mse_proj_dfm_pca_normal_cv.qs"))

mse_proj_arima_pca_normal_cv_1 <- mse_proj_arima_pca_normal_cv[[1]]
mse_proj_arima_normal_cv_1 <- mse_proj_arima_normal_cv[[1]]
mse_proj_dfm_pca_normal_cv_1 <- mse_proj_dfm_pca_normal_cv[[1]]
mse_proj_arima_pca_normal_cv_m <- mse_proj_arima_pca_normal_cv[[m]]
mse_proj_arima_normal_cv_m <- mse_proj_arima_normal_cv[[m]]
mse_proj_dfm_pca_normal_cv_m <- mse_proj_dfm_pca_normal_cv[[m]]

name_vec <- c(
  mse_arima_cv = "ARIMA-Base",
  mse_dfm_cv = "DFM-Base",
  mse_proj_arima_pca_normal_cv_1 = "ARIMA-PCA+Norm-1",
  mse_proj_arima_normal_cv_1 = "ARIMA-Norm-1",
  mse_proj_dfm_pca_normal_cv_1 = "DFM-PCA+Norm-1",
  mse_proj_arima_pca_normal_cv_m = "ARIMA-PCA+Norm-m",
  mse_proj_arima_normal_cv_m = "ARIMA-Norm-m",
  mse_proj_dfm_pca_normal_cv_m = "DFM-PCA+Norm-m"
)


mse_cv_mat_ls <- lst(!!!syms(names(name_vec))) %>%
  list2array() %>%
  aperm(c(1, 3, 2)) %>%
  array2list()
# tsutils::nemenyi(mse_cv_mat_ls[[1]], plottype = "vmcb")

mcb_df_ls <-  mse_cv_mat_ls %>%
  lapply(tsutils::nemenyi, plottype = "none") %>%
  lapply(\(x) as.data.frame(x) %>%
           mutate(name = name_vec[name]) %>%
           mutate(name = paste(name, format(round(value, 2), width = 5, nsmall = 2))) %>%
           mutate(col =
                    (u[[which.min(value)]] <=u &
                       u[[which.min(value)]] >=l) |
                    (u[[which.min(value)]] >=u &
                       l[[which.min(value)]] <=u)
           ))

plot_mcb_cv <- mcb_df_ls %>%
  bind_rows(.id = "h") %>%
  mutate(h = as.integer(h)) %>%
  filter(h %in% c(1, 6, 12)) %>%
  mutate(name = factor(name,unique(name), ordered = TRUE)) %>%
  group_by(h) %>%
  plot_mcb("Base") +
  facet_wrap("h", scales = "free", labeller = label_both, ncol = 1,
             strip.position = "right")
plot_mcb_cv

## ---- visnights ----
pa_visnights <- function(...) pa("tourism", "projection", "output", ...)
m <- 77
p <- 200

## ---- fig-visnights-line ----
mse <- qs::qread(pa_visnights("mse.qs"))
plot_mse <- mse %>%
  filter(h %in% c(1, 6, 12)) %>%
  ggplot(aes(x = p, y = value,
             linetype = paste(proj, Phi, sep = "."))) +
  geom_vline(xintercept = m) +
  geom_line() +
  geom_hline(data = \(df) filter(df, !proj),
             aes(yintercept = value,
                 linetype = paste(proj, Phi, sep = "."))) +
  # facet_wrap("h", scales = "free", labeller = label_both) +
  facet_grid(rows = "h", scales = "free", labeller = label_both) +
  ylab("MSE") +
  scale_linetype_manual(
    name = "Component",
    values = c(
      "TRUE.PCA_normal" = "solid",
      "FALSE.NA" = "dashed",
      "TRUE.normal" = "longdash"
    ),
    labels = c(
      "TRUE.PCA_normal" = "PCA+Norm.",
      "FALSE.NA" = "No Proj.",
      "TRUE.normal" = "Norm."
    ))
plot_mse
## ---- fig-visnights-mcb-series ----
# ?tsutils::nemenyi
# dir(pa_visnights())

mse_ets_series <- qs::qread(pa_visnights("mse_ets_series.qs"))
mse_proj_ets_normal_series <- qs::qread(pa_visnights("mse_proj_ets_normal_series.qs"))
mse_proj_ets_pca_normal_series <- qs::qread(pa_visnights("mse_proj_ets_pca_normal_series.qs"))
mse_proj_ets_pca_normal_series_1 <- mse_proj_ets_pca_normal_series[[1]]
mse_proj_ets_normal_series_1 <- mse_proj_ets_normal_series[[1]]
mse_proj_ets_pca_normal_series_m <- mse_proj_ets_pca_normal_series[[m]]
mse_proj_ets_normal_series_m <- mse_proj_ets_normal_series[[m]]
mse_proj_ets_pca_normal_series_p <- mse_proj_ets_pca_normal_series[[p]]
mse_proj_ets_normal_series_p <- mse_proj_ets_normal_series[[p]]

name_vec <- c(
  mse_ets_series = "ETS-Base",
  mse_proj_ets_pca_normal_series_1 = "ETS-PCA+Norm-1",
  mse_proj_ets_normal_series_1 = "ETS-Norm-1",
  mse_proj_ets_pca_normal_series_m = "ETS-PCA+Norm-m",
  mse_proj_ets_normal_series_m = "ETS-Norm-m",
  mse_proj_ets_pca_normal_series_p = "ETS-PCA+Norm-200",
  mse_proj_ets_normal_series_p = "ETS-Norm-200"
)


mse_series_mat_ls <- lst(!!!syms(names(name_vec))) %>%
  list2array() %>%
  aperm(c(1, 3, 2)) %>%
  array2list()
# tsutils::nemenyi(mse_series_mat_ls[[1]], plottype = "vmcb")


mcb_df_ls <-  mse_series_mat_ls %>%
  lapply(tsutils::nemenyi, plottype = "none") %>%
  lapply(\(x) as.data.frame(x) %>%
           mutate(name = name_vec[name]) %>%
           mutate(name = paste(name, format(round(value, 2), width = 5, nsmall = 2))) %>%
           mutate(col =
                    (u[[which.min(value)]] <=u &
                       u[[which.min(value)]] >=l) |
                    (u[[which.min(value)]] >=u &
                       l[[which.min(value)]] <=u)
           ))


plot_mcb_series <- mcb_df_ls %>%
  bind_rows(.id = "h") %>%
  mutate(h = as.integer(h)) %>%
  filter(h %in% c(1, 6, 12)) %>%
  mutate(name = factor(name,unique(name), ordered = TRUE)) %>%
  group_by(h) %>%
  plot_mcb("Base") +
  facet_wrap("h", scales = "free", labeller = label_both, ncol = 1,
             strip.position = "right")
plot_mcb_series
## ---- fig-visnights-mcb-cv ----
# ?tsutils::nemenyi
# dir(pa_visnights())

mse_ets_cv <- qs::qread(pa_visnights("mse_ets_cv.qs"))
mse_proj_ets_normal_cv <- qs::qread(pa_visnights("mse_proj_ets_normal_cv.qs"))
mse_proj_ets_pca_normal_cv <- qs::qread(pa_visnights("mse_proj_ets_pca_normal_cv.qs"))
mse_proj_ets_pca_normal_cv_1 <- mse_proj_ets_pca_normal_cv[[1]]
mse_proj_ets_normal_cv_1 <- mse_proj_ets_normal_cv[[1]]
mse_proj_ets_pca_normal_cv_m <- mse_proj_ets_pca_normal_cv[[m]]
mse_proj_ets_normal_cv_m <- mse_proj_ets_normal_cv[[m]]
mse_proj_ets_pca_normal_cv_p <- mse_proj_ets_pca_normal_cv[[p]]
mse_proj_ets_normal_cv_p <- mse_proj_ets_normal_cv[[p]]

name_vec <- c(
  mse_ets_cv = "ETS-Base",
  mse_proj_ets_pca_normal_cv_1 = "ETS-PCA+Norm-1",
  mse_proj_ets_normal_cv_1 = "ETS-Norm-1",
  mse_proj_ets_pca_normal_cv_m = "ETS-PCA+Norm-m",
  mse_proj_ets_normal_cv_m = "ETS-Norm-m",
  mse_proj_ets_pca_normal_cv_p = "ETS-PCA+Norm-200",
  mse_proj_ets_normal_cv_p = "ETS-Norm-200"
)


mse_cv_mat_ls <- lst(!!!syms(names(name_vec))) %>%
  list2array() %>%
  aperm(c(1, 3, 2)) %>%
  array2list()
# tsutils::nemenyi(mse_cv_mat_ls[[1]], plottype = "vmcb")


mcb_df_ls <-  mse_cv_mat_ls %>%
  lapply(tsutils::nemenyi, plottype = "none") %>%
  lapply(\(x) as.data.frame(x) %>%
           mutate(name = name_vec[name]) %>%
           mutate(name = paste(name, format(round(value, 2), width = 5, nsmall = 2))) %>%
           mutate(col =
                    (u[[which.min(value)]] <=u &
                       u[[which.min(value)]] >=l) |
                    (u[[which.min(value)]] >=u &
                       l[[which.min(value)]] <=u)
           ))


plot_mcb_cv <- mcb_df_ls %>%
  bind_rows(.id = "h") %>%
  mutate(h = as.integer(h)) %>%
  filter(h %in% c(1, 6, 12)) %>%
  mutate(name = factor(name,unique(name), ordered = TRUE)) %>%
  group_by(h) %>%
  plot_mcb("Base") +
  facet_wrap("h", scales = "free", labeller = label_both, ncol = 1,
             strip.position = "right")
plot_mcb_cv

## ---- simulation ----
pa_simulation <- function(...) pa("simulation", "projection", "output", ...)
m <- 70
p <- 300

## ---- fig-simulation-line ----
mse <- bind_rows(
  qs::qread(pa_simulation("mse.qs")),
  qs::qread(pa_simulation("mse_exp.qs"))
)
plot_mse <- mse %>%
  filter(model %in% c("arima", "dfm", "var", "true"),
         Phi %in% c("PCA_normal", "normal") | is.na(Phi),
         h %in% c(1, 6, 12)) %>%
  # {print(distinct(., model, Phi))}
  ggplot(aes(x = p, y = value,
             colour = model,
             linetype = paste(proj, Phi, sep = ".")))+
  geom_vline(xintercept = m) +
  geom_line() +
  geom_hline(data = \(df) filter(df, !proj),
             aes(yintercept = value,
                 colour = model,
                 linetype = paste(proj, Phi, sep = "."))) +
  # facet_wrap("h", scales = "free", labeller = label_both) +
  facet_grid(rows = "h", scales = "free", labeller = label_both) +
  ylab("MSE") +
  scale_linetype_manual(
    name = "Component",
    values = c(
      "TRUE.PCA_normal" = "solid",
      "FALSE.NA" = "dashed",
      "TRUE.normal" = "longdash"
    ),
    labels = c(
      "TRUE.PCA_normal" = "PCA+Norm.",
      "FALSE.NA" = "No Proj.",
      "TRUE.normal" = "Norm."
    )) +
  scale_color_manual(
    name = "Model",
    values = cb_palette_grey[c(7, 6, 4, 2)],
    labels = c(
      "arima" = "ARIMA",
      "dfm" = "DFM",
      "true" = "VAR - DGP",
      "var" = "VAR - Est."))
plot_mse
## ---- fig-simulation-mcb-series ----
# ?tsutils::nemenyi
# dir(pa_simulation())

mse_arima_series <- qs::qread(pa_simulation("mse_arima_series.qs"))
mse_dfm_series <- qs::qread(pa_simulation("mse_dfm_series.qs"))
mse_var_series <- qs::qread(pa_simulation("mse_var_series.qs"))
mse_proj_arima_normal_series <- qs::qread(pa_simulation("mse_proj_arima_normal_series.qs"))
mse_proj_arima_ortho_normal_series <- qs::qread(pa_simulation("mse_proj_arima_ortho_normal_series.qs"))
mse_proj_arima_series <- qs::qread(pa_simulation("mse_proj_arima_series.qs"))
mse_proj_arima_pca_uniform_series <- qs::qread(pa_simulation("mse_proj_arima_pca_uniform_series.qs"))
mse_proj_arima_uniform_series <- qs::qread(pa_simulation("mse_proj_arima_uniform_series.qs"))
mse_proj_dfm_series <- qs::qread(pa_simulation("mse_proj_dfm_series.qs"))

mse_proj_arima_normal_series_m <- mse_proj_arima_normal_series[[m]]
mse_proj_arima_ortho_normal_series_m <- mse_proj_arima_ortho_normal_series[[m]]
mse_proj_arima_series_m <- mse_proj_arima_series[[m]]
mse_proj_arima_pca_uniform_series_m <- mse_proj_arima_pca_uniform_series[[m]]
mse_proj_arima_uniform_series_m <- mse_proj_arima_uniform_series[[m]]
mse_proj_dfm_series_m <- mse_proj_dfm_series[[m]]
mse_proj_arima_normal_series_p <- mse_proj_arima_normal_series[[p]]
mse_proj_arima_ortho_normal_series_p <- mse_proj_arima_ortho_normal_series[[p]]
mse_proj_arima_series_p <- mse_proj_arima_series[[p]]
mse_proj_arima_pca_uniform_series_p <- mse_proj_arima_pca_uniform_series[[p]]
mse_proj_arima_uniform_series_p <- mse_proj_arima_uniform_series[[p]]
mse_proj_dfm_series_p <- mse_proj_dfm_series[[p]]

name_vec <- c(
  mse_arima_series = "ARIMA-Base",
  mse_dfm_series = "DFM-Base",
  # mse_var_series = "VAR-Base",
  mse_proj_arima_normal_series_m = "ARIMA-Norm-m",
  mse_proj_arima_ortho_normal_series_m = "ARIMA-Ortho+Norm-m",
  mse_proj_arima_series_m = "ARIMA-PCA+Norm-m",
  mse_proj_arima_pca_uniform_series_m = "ARIMA-PCA+Unif-m",
  mse_proj_arima_uniform_series_m = "ARIMA-Unif-m",
  mse_proj_dfm_series_m = "DFM-PCA+Norm-m",
  mse_proj_arima_normal_series_p = "ARIMA-Norm-300",
  mse_proj_arima_ortho_normal_series_p = "ARIMA-Ortho+Norm-300",
  mse_proj_arima_series_p = "ARIMA-PCA+Norm-300",
  mse_proj_arima_pca_uniform_series_p = "ARIMA-PCA+Unif-300",
  mse_proj_arima_uniform_series_p = "ARIMA-Unif-300",
  mse_proj_dfm_series_p = "DFM-PCA+Norm-300"
)


mse_series_mat_ls <- lst(!!!syms(names(name_vec))) %>%
  list2array() %>%
  aperm(c(1, 3, 2)) %>%
  array2list()
# tsutils::nemenyi(mse_series_mat_ls[[1]], plottype = "vmcb")


mcb_df_ls <-  mse_series_mat_ls %>%
  lapply(tsutils::nemenyi, plottype = "none") %>%
  lapply(\(x) as.data.frame(x) %>%
           mutate(name = name_vec[name]) %>%
           mutate(name = paste(name, format(round(value, 2), width = 5, nsmall = 2))) %>%
           mutate(col =
                    (u[[which.min(value)]] <=u &
                       u[[which.min(value)]] >=l) |
                    (u[[which.min(value)]] >=u &
                       l[[which.min(value)]] <=u)
           ))

plot_mcb_series <- mcb_df_ls %>%
  bind_rows(.id = "h") %>%
  mutate(h = as.integer(h)) %>%
  filter(h %in% c(1, 6, 12)) %>%
  mutate(name = paste0(sprintf("%002d", h), name)) %>%
  mutate(name = factor(name,unique(name), ordered = TRUE)) %>%
  group_by(h) %>%
  plot_mcb("Base") +
  facet_wrap("h", scales = "free", labeller = label_both, ncol = 1,
             strip.position = "right") +
  scale_y_discrete(labels = \(x) substr(x, 3, 1e4))
plot_mcb_series
## ---- fig-simulation-mcb-cv ----
# ?tsutils::nemenyi
# dir(pa_simulation())

mse_arima_cv <- qs::qread(pa_simulation("mse_arima_cv.qs"))
mse_dfm_cv <- qs::qread(pa_simulation("mse_dfm_cv.qs"))
mse_var_cv <- qs::qread(pa_simulation("mse_var_cv.qs"))
mse_proj_arima_normal_cv <- qs::qread(pa_simulation("mse_proj_arima_normal_cv.qs"))
mse_proj_arima_ortho_normal_cv <- qs::qread(pa_simulation("mse_proj_arima_ortho_normal_cv.qs"))
mse_proj_arima_cv <- qs::qread(pa_simulation("mse_proj_arima_cv.qs"))
mse_proj_arima_pca_uniform_cv <- qs::qread(pa_simulation("mse_proj_arima_pca_uniform_cv.qs"))
mse_proj_arima_uniform_cv <- qs::qread(pa_simulation("mse_proj_arima_uniform_cv.qs"))
mse_proj_dfm_cv <- qs::qread(pa_simulation("mse_proj_dfm_cv.qs"))

mse_proj_arima_normal_cv_m <- mse_proj_arima_normal_cv[[m]]
mse_proj_arima_ortho_normal_cv_m <- mse_proj_arima_ortho_normal_cv[[m]]
mse_proj_arima_cv_m <- mse_proj_arima_cv[[m]]
mse_proj_arima_pca_uniform_cv_m <- mse_proj_arima_pca_uniform_cv[[m]]
mse_proj_arima_uniform_cv_m <- mse_proj_arima_uniform_cv[[m]]
mse_proj_dfm_cv_m <- mse_proj_dfm_cv[[m]]
mse_proj_arima_normal_cv_p <- mse_proj_arima_normal_cv[[p]]
mse_proj_arima_ortho_normal_cv_p <- mse_proj_arima_ortho_normal_cv[[p]]
mse_proj_arima_cv_p <- mse_proj_arima_cv[[p]]
mse_proj_arima_pca_uniform_cv_p <- mse_proj_arima_pca_uniform_cv[[p]]
mse_proj_arima_uniform_cv_p <- mse_proj_arima_uniform_cv[[p]]
mse_proj_dfm_cv_p <- mse_proj_dfm_cv[[p]]

name_vec <- c(
  mse_arima_cv = "ARIMA-Base",
  mse_dfm_cv = "DFM-Base",
  # mse_var_cv = "VAR-Base",
  mse_proj_arima_normal_cv_m = "ARIMA-Norm-m",
  mse_proj_arima_ortho_normal_cv_m = "ARIMA-Ortho+Norm-m",
  mse_proj_arima_cv_m = "ARIMA-PCA+Norm-m",
  mse_proj_arima_pca_uniform_cv_m = "ARIMA-PCA+Unif-m",
  mse_proj_arima_uniform_cv_m = "ARIMA-Unif-m",
  mse_proj_dfm_cv_m = "DFM-PCA+Norm-m",
  mse_proj_arima_normal_cv_p = "ARIMA-Norm-300",
  mse_proj_arima_ortho_normal_cv_p = "ARIMA-Ortho+Norm-300",
  mse_proj_arima_cv_p = "ARIMA-PCA+Norm-300",
  mse_proj_arima_pca_uniform_cv_p = "ARIMA-PCA+Unif-300",
  mse_proj_arima_uniform_cv_p = "ARIMA-Unif-300",
  mse_proj_dfm_cv_p = "DFM-PCA+Norm-300"
)


mse_cv_mat_ls <- lst(!!!syms(names(name_vec))) %>%
  list2array() %>%
  aperm(c(1, 3, 2)) %>%
  array2list()
# tsutils::nemenyi(mse_cv_mat_ls[[1]], plottype = "vmcb")


mcb_df_ls <-  mse_cv_mat_ls %>%
  lapply(tsutils::nemenyi, plottype = "none") %>%
  lapply(\(x) as.data.frame(x) %>%
           mutate(name = name_vec[name]) %>%
           mutate(name = paste(name, format(round(value, 2), width = 5, nsmall = 2))) %>%
           mutate(col =
                    (u[[which.min(value)]] <=u &
                       u[[which.min(value)]] >=l) |
                    (u[[which.min(value)]] >=u &
                       l[[which.min(value)]] <=u)
           ))

plot_mcb_cv <- mcb_df_ls %>%
  bind_rows(.id = "h") %>%
  mutate(h = as.integer(h)) %>%
  filter(h %in% c(1, 6, 12)) %>%
  mutate(name = paste0(sprintf("%002d", h), name)) %>%
  mutate(name = factor(name,unique(name), ordered = TRUE)) %>%
  group_by(h) %>%
  plot_mcb("Base") +
  facet_wrap("h", scales = "free", labeller = label_both, ncol = 1,
             strip.position = "right") +
  scale_y_discrete(labels = \(x) substr(x, 3, 1e4))
plot_mcb_cv

## ---- tbl-simulation-var-coef ----

B_true <- qs::qread(pa_simulation("B_true.qs"))
rownames(B_true) <- paste0("V",1:70)
colnames(B_true)[2:211] <- paste(rep(paste0("V",1:70), 3), rep(-1:-3, each=70))
B_true %>%
  round(3) %>%
  kable("latex", longtable = TRUE, booktabs = TRUE) %>%
  kable_styling(latex_options = c("repeat_header"), font_size = 7) %>%
  landscape()

## ---- end ----
