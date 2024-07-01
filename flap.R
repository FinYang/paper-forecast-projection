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
library(rlang)
library(glue)

cb_palette_grey <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
cb_palette_black <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

# Singular component or PCA
SC <- FALSE

if(SC) {
  pca_name <- "PCA_normal"
  comp <- "SC"
} else {
  pca_name <- "PCAcentred_normal"
  comp <- "PCA"
}

c2 <- function(...) {
  dots <- list2(...)
  num <- unlist(dots)
  set_names(num, names(dots))
}
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
  geom_hline(data = \(df) filter(df, !proj),
             aes(yintercept = value,
                 colour = model,
                 linetype = paste(proj, Phi, sep = "."))) +
  geom_line() +
  # facet_wrap("h", scales = "free", labeller = label_both) +
  facet_grid(rows = "h", scales = "free", labeller = label_both) +
  ylab("MSE") +
  scale_linetype_manual(
    name = "Component",
    values = c2(
      !!sym(paste0("TRUE.", pca_name)) := "dashed",
      "FALSE.NA" = "solid",
      "TRUE.normal" = "longdash"
    ),
    labels = c2(
      !!sym(paste0("TRUE.", pca_name)) := paste0(comp, "+Norm."),
      "FALSE.NA" = "No Proj.",
      "TRUE.normal" = "Norm."
    )) +
  scale_color_manual(
    name = "Model",
    values = cb_palette_grey[c(7, 6)],
    labels = c(
      "arima" = "ARIMA",
      "dfm" = "DFM")) +
  theme(plot.background = element_blank(),
        legend.background = element_blank()) +
  scale_x_continuous(expand = expansion(mult = 0))
plot_mse
## ---- fig-fred-md-mcb-series ----

mse_arima_series <- qs::qread(pa_fredmd("mse_arima_series.qs"))
mse_dfm_series <- qs::qread(pa_fredmd("mse_dfm_series.qs"))
mse_proj_arima_normal_series <- qs::qread(pa_fredmd("mse_proj_arima_normal_series.qs"))
mse_proj_arima_pca_normal_series <- qs::qread(pa_fredmd(glue("mse_proj_arima_{tolower(pca_name)}_series.qs")))
mse_proj_dfm_pca_normal_series <- qs::qread(pa_fredmd(glue("mse_proj_dfm_{tolower(pca_name)}_series.qs")))

mse_proj_arima_pca_normal_series_1 <- mse_proj_arima_pca_normal_series[[1]]
mse_proj_arima_normal_series_1 <- mse_proj_arima_normal_series[[1]]
mse_proj_dfm_pca_normal_series_1 <- mse_proj_dfm_pca_normal_series[[1]]
mse_proj_arima_pca_normal_series_2 <- mse_proj_arima_pca_normal_series[[2]]
mse_proj_arima_normal_series_2 <- mse_proj_arima_normal_series[[2]]
mse_proj_dfm_pca_normal_series_2 <- mse_proj_dfm_pca_normal_series[[2]]
mse_proj_arima_pca_normal_series_m <- mse_proj_arima_pca_normal_series[[m]]
mse_proj_arima_normal_series_m <- mse_proj_arima_normal_series[[m]]
mse_proj_dfm_pca_normal_series_m <- mse_proj_dfm_pca_normal_series[[m]]

name_vec <- c(
  mse_arima_series = "ARIMA-Benchmark",
  mse_dfm_series = "DFM-Benchmark",
  mse_proj_arima_pca_normal_series_1 = glue("ARIMA-{comp}-1"),
  mse_proj_arima_normal_series_1 = "ARIMA-Norm-1",
  mse_proj_dfm_pca_normal_series_1 = glue("DFM-{comp}-1"),
  # mse_proj_arima_pca_normal_series_2 = "ARIMA-PCA-2",
  # mse_proj_arima_normal_series_2 = "ARIMA-Norm-2",
  # mse_proj_dfm_pca_normal_series_2 = "DFM-PCA-2",
  mse_proj_arima_pca_normal_series_m = glue("ARIMA-{comp}-m"),
  mse_proj_arima_normal_series_m = "ARIMA-Norm-m",
  mse_proj_dfm_pca_normal_series_m = glue("DFM-{comp}-m")
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
  plot_mcb("Benchmark") +
  facet_wrap("h", scales = "free", labeller = label_both, ncol = 1,
             strip.position = "right")
plot_mcb_series

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
  geom_hline(data = \(df) filter(df, !proj),
             aes(yintercept = value,
                 linetype = paste(proj, Phi, sep = "."))) +
  geom_line() +
  # facet_wrap("h", scales = "free", labeller = label_both) +
  facet_grid(rows = "h", scales = "free", labeller = label_both) +
  ylab("MSE") +
  scale_linetype_manual(
    name = "Component",
    values = c2(
      !!sym(paste0("TRUE.", pca_name)) := "dashed",
      "FALSE.NA" = "solid",
      "TRUE.normal" = "longdash"
    ),
    labels = c2(
      !!sym(paste0("TRUE.", pca_name)) := paste0(comp, "+Norm."),
      "FALSE.NA" = "No Proj.",
      "TRUE.normal" = "Norm."
    )) +
  theme(plot.background = element_blank(),
        legend.background = element_blank()) +
  scale_x_continuous(expand = expansion(mult = 0))
plot_mse
## ---- fig-visnights-mcb-series ----
# ?tsutils::nemenyi
# dir(pa_visnights())
mse_ets_series <- qs::qread(pa_visnights("mse_ets_series.qs"))
mse_proj_ets_normal_series <- qs::qread(pa_visnights("mse_proj_ets_normal_series.qs"))
mse_proj_ets_pca_normal_series <- qs::qread(pa_visnights(glue("mse_proj_ets_{tolower(pca_name)}_series.qs")))

mse_proj_ets_pca_normal_series_1 <- mse_proj_ets_pca_normal_series[[1]]
mse_proj_ets_normal_series_1 <- mse_proj_ets_normal_series[[1]]
mse_proj_ets_pca_normal_series_2 <- mse_proj_ets_pca_normal_series[[2]]
mse_proj_ets_normal_series_2 <- mse_proj_ets_normal_series[[2]]
mse_proj_ets_pca_normal_series_m <- mse_proj_ets_pca_normal_series[[m]]
mse_proj_ets_normal_series_m <- mse_proj_ets_normal_series[[m]]
mse_proj_ets_pca_normal_series_p <- mse_proj_ets_pca_normal_series[[p]]
mse_proj_ets_normal_series_p <- mse_proj_ets_normal_series[[p]]

name_vec <- c(
  mse_ets_series = "ETS-Benchmark",
  mse_proj_ets_pca_normal_series_1 = glue("ETS-{comp}-1"),
  mse_proj_ets_normal_series_1 = "ETS-Norm-1",
  mse_proj_ets_pca_normal_series_2 = glue("ETS-{comp}-2"),
  mse_proj_ets_normal_series_2 = "ETS-Norm-2",
  mse_proj_ets_pca_normal_series_m = glue("ETS-{comp}-m"),
  mse_proj_ets_normal_series_m = "ETS-Norm-m",
  mse_proj_ets_pca_normal_series_p = glue("ETS-{comp}+Norm-200"),
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
  imap(\(df, i) mutate(df, name_unique = paste(name, i))) %>%
  bind_rows(.id = "h") %>%
  mutate(h = as.integer(h)) %>%
  filter(h %in% c(1, 6, 12)) %>%
  mutate(name = factor(name_unique, unique(name_unique), ordered = TRUE)) %>%
  group_by(h) %>%
  plot_mcb("Benchmark") +
  facet_wrap("h", scales = "free", labeller = label_both, ncol = 1,
             strip.position = "right") +
  scale_y_discrete(labels = \(x) gsub("[[:space:]][[:digit:]]{1,2}$", "", x))
plot_mcb_series

## ---- simulation ----
pa_simulation <- function(...) pa("simulation", "projection", "output", ...)
m <- 70
p <- 300

## ---- fig-simulation-line ----
mse <- qs::qread(pa_simulation("mse.qs"))
plot_mse <- mse %>%
  filter(model %in% c("arima", "dfm", "var", "true"),
         h %in% c(1, 6, 12)) %>%
  # {print(distinct(., model, Phi))}
  ggplot(aes(x = p, y = value,
             colour = model,
             linetype = paste(proj, Phi, sep = ".")))+
  geom_vline(xintercept = m) +
  geom_hline(data = \(df) filter(df, !proj),
             aes(yintercept = value,
                 colour = model,
                 linetype = paste(proj, Phi, sep = "."))) +
  geom_line() +
  # facet_wrap("h", scales = "free", labeller = label_both) +
  facet_grid(rows = "h", labeller = label_both) +
  ylab("MSE") +
  scale_linetype_manual(
    name = "Component",
    values = c2(
      !!sym(paste0("TRUE.", pca_name)) := "dashed",
      "FALSE.NA" = "solid",
      "TRUE.normal" = "longdash"
    ),
    labels = c2(
      !!sym(paste0("TRUE.", pca_name)) := paste0(comp, "+Norm."),
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
      "var" = "VAR - Est.")) +
  theme(plot.background = element_blank(),
        legend.background = element_blank()) +
  scale_x_continuous(expand = expansion(mult = 0))
plot_mse
## ---- fig-simulation-mcb-series ----
# ?tsutils::nemenyi
# dir(pa_simulation())

mse_arima_series <- qs::qread(pa_simulation("mse_arima_series.qs"))
mse_dfm_series <- qs::qread(pa_simulation("mse_dfm_series.qs"))
mse_var_series <- qs::qread(pa_simulation("mse_var_series.qs"))
mse_proj_arima_normal_series <- qs::qread(pa_simulation("mse_proj_arima_normal_series.qs"))
mse_proj_arima_ortho_normal_series <- qs::qread(pa_simulation("mse_proj_arima_ortho_normal_series.qs"))
mse_proj_arima_uniform_series <- qs::qread(pa_simulation("mse_proj_arima_uniform_series.qs"))
if(SC) {
  mse_proj_arima_series <- qs::qread(pa_simulation("mse_proj_arima_series.qs"))
  mse_proj_dfm_series <- qs::qread(pa_simulation("mse_proj_dfm_series.qs"))
  mse_proj_arima_pca_uniform_series <- qs::qread(pa_simulation("mse_proj_arima_pca_uniform_series.qs"))
} else {
  mse_proj_arima_series <- qs::qread(pa_simulation("mse_proj_arima_pcacentred_normal_series.qs"))
  mse_proj_dfm_series <- qs::qread(pa_simulation("mse_proj_dfm_pcacentred_normal_series.qs"))
  mse_proj_arima_pca_uniform_series <- qs::qread(pa_simulation("mse_proj_arima_pcacentred_uniform_series.qs"))
}
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
  mse_arima_series = "ARIMA-Benchmark",
  mse_dfm_series = "DFM-Benchmark",
  # mse_var_series = "VAR-Base",
  mse_proj_arima_normal_series_m = "ARIMA-Norm-m",
  mse_proj_arima_ortho_normal_series_m = "ARIMA-Ortho-m",
  mse_proj_arima_series_m = glue("ARIMA-{comp}-m"),
  # mse_proj_arima_pca_uniform_series_m = "ARIMA-{comp}+Unif-m",
  mse_proj_arima_uniform_series_m = "ARIMA-Unif-m",
  mse_proj_dfm_series_m = glue("DFM-{comp}-m"),
  mse_proj_arima_normal_series_p = "ARIMA-Norm-300",
  mse_proj_arima_ortho_normal_series_p = "ARIMA-Ortho+Norm-300",
  mse_proj_arima_series_p = glue("ARIMA-{comp}+Norm-300"),
  mse_proj_arima_pca_uniform_series_p = glue("ARIMA-{comp}+Unif-300"),
  mse_proj_arima_uniform_series_p = "ARIMA-Unif-300",
  mse_proj_dfm_series_p = glue("DFM-{comp}+Norm-300")
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
  plot_mcb("Benchmark") +
  facet_wrap("h", scales = "free", labeller = label_both, ncol = 1,
             strip.position = "right") +
  scale_y_discrete(labels = \(x) substr(x, 3, 1e4))
plot_mcb_series


## ---- end ----
