# Created by use_targets().
# Follow the comments below to fill in this target script.
# Then follow the manual to check and run the pipeline:
#   https://books.ropensci.org/targets/walkthrough.html#inspect-the-pipeline # nolint

# Load packages required to define the pipeline:
library(targets)
library(tarchetypes)
library(future)
library(future.batchtools)
library(rlang)
library(progressr)
library(tidyverse)
# Set target options:
tar_option_set(
  packages = c(
    "tidyverse",
    "forecast",
    "fbi",
    "tsibble"
  ), # packages that your targets need to run
  format = "qs", # default storage format
  deployment= "worker",
  storage = "worker",
  retrieval = "worker",
  seed = 0,
  error = "abridge",
  resources = tar_resources(
    future = tar_resources_future(
      plan = future::tweak(
        batchtools_slurm,
        template = "future.tmpl",
        resources = list(
          ntasks = 1,
          ncpus = 1,
          walltime = 60*60*24, #seconds
          memory = 1024*1 #megabytes
        )
      )
    )
  )
)

# tar_make_clustermq() configuration (okay to leave alone):
options(clustermq.scheduler = "slurm")
options(clustermq.template = "clustermq.tmpl")

options(future.batchtools.workers = 240)

# tar_make_future() configuration (okay to leave alone):
future::plan(future.batchtools::batchtools_slurm, template = "future.tmpl")


future_ram <- function(G = 1) {
  tar_resources(
    future = tar_resources_future(
      plan = future::tweak(
        batchtools_slurm,
        template = "future.tmpl",
        resources = list(
          ncpus = 1,
          memory = 1024*G
        )
      )
    )
  )
}

# Run the R scripts in the R/ folder with your custom functions:
tar_source()
# source("other_functions.R") # Source other scripts as needed. # nolint

# Global variables
# .init <- ifelse(Sys.info()['sysname'] == "Linux", 300, 765)
# .n_id <- 777 - .init
# .n_lag <- 3
.forecast_h <- 12
p <- 300
# FRED-MD
# .frequency <- 12
# m <- 122


# Replace the target list below with your own:
list(
  # tar_target(error_var, rWishart(1, m, diag(m))[,,1]),
  tar_target(fred_month, "2023-10", deployment = "main"),
  tar_target(fred_link,
             sprintf("https://files.stlouisfed.org/files/htdocs/fred-md/monthly/%s.csv",
                     fred_month),
             deployment = "main"),
  tar_target(fred_raw, fbi::fredmd(fred_link),
             deployment = "main"),
  tar_target(fred_nooutlier, as_tibble(fbi::rm_outliers.fredmd(fred_raw)),
             deployment = "main"),
  tar_target(fred_lessna, dplyr::select(fred_nooutlier, where(\(x) mean(is.na(x))<0.05)),
             deployment = "main"),
  # tar_target(fred, fred_lessna %>%
  #              mutate(date = yearmonth(date)) %>% # fred-md
  #              em_fill_df(),
  tar_target(fred, fred_lessna %>%
               mutate(date = yearmonth(date)) %>% # fred-md
               em_fill_df() %>%
               mutate(across(-date, \(x) as.numeric(scale(x)))),
             deployment = "main"),
  tar_target(fred_sd, fred_lessna %>%
               mutate(date = yearmonth(date)) %>% # fred-md
               em_fill_df() %>%
               select(-date) %>%
               as.matrix() %>%
               apply(2, sd),
             deployment = "main"),
  tar_target(n_init, ifelse(Sys.info()['sysname'] == "Linux", 300, nrow(fred) - .forecast_h),
             deployment = "main"),
  tar_target(fred_tsb, as_tsibble(fred, index = date), deployment = "main"),
  tar_target(fred_mat, as.ts(fred_tsb), deployment = "main"),
  tar_target(m, ncol(fred_mat), deployment = "main"),
  tar_target(fred_tsp, tsp(fred_mat), deployment = "main"),
  tar_target(fred_start, fred_tsp[[1]], deployment = "main"),
  tar_target(fred_freq, fred_tsp[[3]], deployment = "main"),
  tar_target(sam_idx, seq(n_init, nrow(fred) - .forecast_h),
             deployment = "main"),
  tar_target(sam_in, fred_mat[seq_len(sam_idx), ],
             iteration = "list", pattern = map(sam_idx),
             deployment = "main"),
  tar_target(sam_out, fred_mat[seq(sam_idx+1, length.out = .forecast_h), ],
             iteration = "list", pattern = map(sam_idx),
             deployment = "main"),

  tar_target(out_arima,
             arima_mat(sam_in, .h = .forecast_h,
                       start = fred_start, frequency = fred_freq),
             iteration = "list",
             pattern = map(sam_in),
             resources = future_ram(2)),
  tar_target(fc_arima, get_fc(out_arima),
             iteration = "list", pattern = map(out_arima),
             deployment = "main"),
  tar_target(res_arima, get_res(out_arima),
             iteration = "list", pattern = map(out_arima),
             deployment = "main"),
  tar_target(out_ets,
             ets_mat(sam_in, .h = .forecast_h,
                     start = fred_start, frequency = fred_freq),
             iteration = "list",
             pattern = map(sam_in),
             resources = future_ram(2)),
  tar_target(fc_ets, get_fc(out_ets),
             iteration = "list", pattern = map(out_ets),
             deployment = "main"),
  tar_target(res_ets, get_res(out_ets),
             iteration = "list", pattern = map(out_ets),
             deployment = "main"),

  tar_target(pca_normal, component(sam_in, "PCA", p = p),
             iteration = "list",
             pattern = map(sam_in),
             # cue = tar_cue(command = FALSE),
             deployment = "main"),
  tar_target(out_arima_pca_normal,
             arima_mat(pca_normal$x, .h = .forecast_h,
                       start = fred_start, frequency = fred_freq),
             iteration = "list", pattern = map(pca_normal),
             resources = future_ram(3)),
  tar_target(fc_arima_pca_normal, get_fc(out_arima_pca_normal),
             iteration = "list", pattern = map(out_arima_pca_normal),
             deployment = "main"),
  tar_target(res_arima_pca_normal, get_res(out_arima_pca_normal),
             iteration = "list", pattern = map(out_arima_pca_normal),
             deployment = "main"),
  tar_target(out_ets_pca_normal,
             ets_mat(pca_normal$x, .h = .forecast_h,
                     start = fred_start, frequency = fred_freq),
             iteration = "list", pattern = map(pca_normal),
             resources = future_ram(3)),
  tar_target(fc_ets_pca_normal, get_fc(out_ets_pca_normal),
             iteration = "list", pattern = map(out_ets_pca_normal),
             deployment = "main"),
  tar_target(res_ets_pca_normal, get_res(out_ets_pca_normal),
             iteration = "list", pattern = map(out_ets_pca_normal),
             deployment = "main"),

  tar_target(pcacentred_normal, component(sam_in, "PCAcentred_normal", p = p),
             iteration = "list",
             pattern = map(sam_in),
             deployment = "main"),
  tar_target(out_arima_pcacentred_normal,
             arima_mat(pcacentred_normal$x, .h = .forecast_h,
                       start = fred_start, frequency = fred_freq),
             iteration = "list", pattern = map(pcacentred_normal),
             resources = future_ram(3)),
  tar_target(fc_arima_pcacentred_normal, get_fc(out_arima_pcacentred_normal),
             iteration = "list", pattern = map(out_arima_pcacentred_normal),
             deployment = "main"),
  tar_target(res_arima_pcacentred_normal, get_res(out_arima_pcacentred_normal),
             iteration = "list", pattern = map(out_arima_pcacentred_normal),
             deployment = "main"),

  tar_target(normal, component(sam_in, "normal", p = p),
             iteration = "list",
             pattern = map(sam_in),
             deployment = "main"),
  tar_target(out_arima_normal,
             arima_mat(normal$x, .h = .forecast_h,
                       start = fred_start, frequency = fred_freq),
             iteration = "list", pattern = map(normal),
             resources = future_ram(3)),
  tar_target(fc_arima_normal, get_fc(out_arima_normal),
             iteration = "list", pattern = map(out_arima_normal),
             deployment = "main"),
  tar_target(res_arima_normal, get_res(out_arima_normal),
             iteration = "list", pattern = map(out_arima_normal),
             deployment = "main"),

  tar_target(out_dfm, {
    lapply(seq_len(.forecast_h), \(.h)
           auto_dfm(sam_in, n_factor = 8, f_lag = 3, y_lag = 6, h = .h))
  }, iteration = "list", pattern = map(sam_in)),
  tar_target(fc_dfm,
             do.call(rbind, lapply(out_dfm, getElement, "fc")),
             iteration = "list", pattern = map(out_dfm),
             deployment = "main"),
  tar_target(res_dfm,
             lapply(out_dfm, getElement, "res"),
             iteration = "list", pattern = map(out_dfm),
             deployment = "main"),


  tar_target(W_arima_pca_normal, get_W(res_arima, res_arima_pca_normal),
             iteration = "list",
             pattern = map(res_arima, res_arima_pca_normal),
             resources = future_ram(4)),
  tar_target(W_arima_pcacentred_normal, get_W(res_arima, res_arima_pcacentred_normal),
             iteration = "list",
             pattern = map(res_arima, res_arima_pcacentred_normal),
             resources = future_ram(4)),
  tar_target(W_ets_pca_normal, get_W(res_ets, res_ets_pca_normal),
             iteration = "list",
             pattern = map(res_ets, res_ets_pca_normal),
             resources = future_ram(4)),
  tar_target(W_dfm_pca_normal, get_W(res_dfm[[1]], res_arima_pca_normal),
             iteration = "list",
             pattern = map(res_dfm, res_arima_pca_normal),
             resources = future_ram(4)),
  tar_target(W_dfm_pcacentred_normal, get_W(res_dfm[[1]], res_arima_pcacentred_normal),
             iteration = "list",
             pattern = map(res_dfm, res_arima_pcacentred_normal),
             resources = future_ram(4)),
  tar_target(W_dfm_ets_pca_normal, get_W(res_dfm[[1]], res_ets_pca_normal),
             iteration = "list",
             pattern = map(res_dfm, res_ets_pca_normal),
             resources = future_ram(4)),
  tar_target(W_arima_normal, get_W(res_arima, res_arima_normal),
             iteration = "list",
             pattern = map(res_arima, res_arima_normal),
             resources = future_ram(4)),

  tar_target(proj_arima_pca_normal,
             project(
               cbind(fc_arima, fc_arima_pca_normal),
               W = W_arima_pca_normal,
               Phi = pca_normal$Phi),
             iteration = "list",
             pattern = map(fc_arima, fc_arima_pca_normal, W_arima_pca_normal, pca_normal),
             resources = future_ram(4)),
  tar_target(proj_arima_pcacentred_normal,
             project(
               cbind(fc_arima, fc_arima_pcacentred_normal),
               W = W_arima_pcacentred_normal,
               Phi = pcacentred_normal$Phi),
             iteration = "list",
             pattern = map(fc_arima, fc_arima_pcacentred_normal, W_arima_pcacentred_normal, pcacentred_normal),
             resources = future_ram(4)),
  tar_target(proj_ets_pca_normal,
             project(
               cbind(fc_ets, fc_ets_pca_normal),
               W = W_ets_pca_normal,
               Phi = pca_normal$Phi),
             iteration = "list",
             pattern = map(fc_ets, fc_ets_pca_normal, W_ets_pca_normal, pca_normal),
             resources = future_ram(4)),
  tar_target(proj_dfm_pca_normal,
             project(
               cbind(fc_dfm, fc_arima_pca_normal),
               W = W_dfm_pca_normal,
               Phi = pca_normal$Phi),
             iteration = "list",
             pattern = map(fc_dfm, fc_arima_pca_normal, W_dfm_pca_normal, pca_normal),
             resources = future_ram(4)),
  tar_target(proj_dfm_pcacentred_normal,
             project(
               cbind(fc_dfm, fc_arima_pcacentred_normal),
               W = W_dfm_pcacentred_normal,
               Phi = pcacentred_normal$Phi),
             iteration = "list",
             pattern = map(fc_dfm, fc_arima_pcacentred_normal, W_dfm_pcacentred_normal, pcacentred_normal),
             resources = future_ram(4)),
  tar_target(proj_dfm_ets_pca_normal,
             project(
               cbind(fc_dfm, fc_ets_pca_normal),
               W = W_dfm_ets_pca_normal,
               Phi = pca_normal$Phi),
             iteration = "list",
             pattern = map(fc_dfm, fc_ets_pca_normal, W_dfm_ets_pca_normal, pca_normal),
             resources = future_ram(4)),
  tar_target(proj_arima_normal,
             project(
               cbind(fc_arima, fc_arima_normal),
               W = W_arima_normal,
               Phi = normal$Phi),
             iteration = "list",
             pattern = map(fc_arima, fc_arima_normal, W_arima_normal, normal),
             resources = future_ram(4)),


  tar_target(pca_normal_switch_from_sd,
             which(apply(pca_normal$x, 2, sd) < mean(apply(normal$x[,1:100], 2, sd)))[[1]],
             pattern = map(pca_normal, normal),
             deployment = "main"),
  tar_target(proj_arima_pca_normal_switch_sd,
             project_switch(
               fc = fc_arima,
               fc_comp1 = fc_arima_pca_normal,
               fc_comp2 = fc_arima_normal,
               Phi1 = pca_normal$Phi,
               Phi2 = normal$Phi,
               res = res_arima,
               res_comp1 = res_arima_pca_normal,
               res_comp2 = res_arima_normal,
               switch_from = pca_normal_switch_from_sd),
             pattern = map(fc_arima, fc_arima_pca_normal, fc_arima_normal,
                           pca_normal, normal,
                           res_arima, res_arima_pca_normal, res_arima_normal,
                           pca_normal_switch_from_sd),
             resources = future_ram(3)),
  tar_target(proj_dfm_pca_normal_switch_sd,
             project_switch(
               fc = fc_dfm,
               fc_comp1 = fc_arima_pca_normal,
               fc_comp2 = fc_arima_normal,
               Phi1 = pca_normal$Phi,
               Phi2 = normal$Phi,
               res = res_dfm[[1]],
               res_comp1 = res_arima_pca_normal,
               res_comp2 = res_arima_normal,
               switch_from = pca_normal_switch_from_sd),
             pattern = map(fc_dfm, fc_arima_pca_normal, fc_arima_normal,
                           pca_normal, normal,
                           res_dfm, res_arima_pca_normal, res_arima_normal,
                           pca_normal_switch_from_sd),
             resources = future_ram(3)),

  tar_target(pca_normal_switch_from_rms,
             which(sqrt(colMeans(pca_normal$x^2)) < mean(sqrt(colMeans(normal$x[,1:100]^2))))[[1]],
             pattern = map(pca_normal, normal),
             deployment = "main"),
  tar_target(proj_arima_pca_normal_switch_rms,
             project_switch(
               fc = fc_arima,
               fc_comp1 = fc_arima_pca_normal,
               fc_comp2 = fc_arima_normal,
               Phi1 = pca_normal$Phi,
               Phi2 = normal$Phi,
               res = res_arima,
               res_comp1 = res_arima_pca_normal,
               res_comp2 = res_arima_normal,
               switch_from = pca_normal_switch_from_rms),
             pattern = map(fc_arima, fc_arima_pca_normal, fc_arima_normal,
                           pca_normal, normal,
                           res_arima, res_arima_pca_normal, res_arima_normal,
                           pca_normal_switch_from_rms),
             resources = future_ram(3)),
  tar_target(proj_dfm_pca_normal_switch_rms,
             project_switch(
               fc = fc_dfm,
               fc_comp1 = fc_arima_pca_normal,
               fc_comp2 = fc_arima_normal,
               Phi1 = pca_normal$Phi,
               Phi2 = normal$Phi,
               res = res_dfm[[1]],
               res_comp1 = res_arima_pca_normal,
               res_comp2 = res_arima_normal,
               switch_from = pca_normal_switch_from_rms),
             pattern = map(fc_dfm, fc_arima_pca_normal, fc_arima_normal,
                           pca_normal, normal,
                           res_dfm, res_arima_pca_normal, res_arima_normal,
                           pca_normal_switch_from_rms),
             resources = future_ram(3)),

  tar_target(pcacentred_normal_switch_from_sd,
             which(apply(pcacentred_normal$x, 2, sd) < mean(apply(normal$x[,1:100], 2, sd)))[[1]],
             pattern = map(pcacentred_normal, normal),
             deployment = "main"),
  tar_target(proj_arima_pcacentred_normal_switch_sd,
             project_switch(
               fc = fc_arima,
               fc_comp1 = fc_arima_pcacentred_normal,
               fc_comp2 = fc_arima_normal,
               Phi1 = pcacentred_normal$Phi,
               Phi2 = normal$Phi,
               res = res_arima,
               res_comp1 = res_arima_pcacentred_normal,
               res_comp2 = res_arima_normal,
               switch_from = pcacentred_normal_switch_from_sd),
             pattern = map(fc_arima, fc_arima_pcacentred_normal, fc_arima_normal,
                           pcacentred_normal, normal,
                           res_arima, res_arima_pcacentred_normal, res_arima_normal,
                           pcacentred_normal_switch_from_sd),
             resources = future_ram(3)),
  tar_target(proj_dfm_pcacentred_normal_switch_sd,
             project_switch(
               fc = fc_dfm,
               fc_comp1 = fc_arima_pcacentred_normal,
               fc_comp2 = fc_arima_normal,
               Phi1 = pcacentred_normal$Phi,
               Phi2 = normal$Phi,
               res = res_dfm[[1]],
               res_comp1 = res_arima_pcacentred_normal,
               res_comp2 = res_arima_normal,
               switch_from = pcacentred_normal_switch_from_sd),
             pattern = map(fc_dfm, fc_arima_pcacentred_normal, fc_arima_normal,
                           pcacentred_normal, normal,
                           res_dfm, res_arima_pcacentred_normal, res_arima_normal,
                           pcacentred_normal_switch_from_sd),
             resources = future_ram(3)),

  tar_target(se_arima, (sam_out - fc_arima)^2,
             iteration = "list",
             pattern = map(sam_out, fc_arima),
             deployment = "main"),
  tar_target(se_ets, (sam_out - fc_ets)^2,
             iteration = "list",
             pattern = map(sam_out, fc_ets),
             deployment = "main"),
  tar_target(se_dfm, (sam_out - fc_dfm)^2,
             iteration = "list",
             pattern = map(sam_out, fc_dfm),
             deployment = "main"),
  tar_target(se_proj_arima_pca_normal, lapply(proj_arima_pca_normal, \(pa) (sam_out - pa)^2),
             iteration = "list",
             pattern = map(sam_out, proj_arima_pca_normal)),
  tar_target(se_proj_arima_pcacentred_normal, lapply(proj_arima_pcacentred_normal, \(pa) (sam_out - pa)^2),
             iteration = "list",
             pattern = map(sam_out, proj_arima_pcacentred_normal)),
  tar_target(se_proj_ets_pca_normal, lapply(proj_ets_pca_normal, \(pa) (sam_out - pa)^2),
             iteration = "list",
             pattern = map(sam_out, proj_ets_pca_normal)),
  tar_target(se_proj_dfm_pca_normal, lapply(proj_dfm_pca_normal, \(pa) (sam_out - pa)^2),
             iteration = "list",
             pattern = map(sam_out, proj_dfm_pca_normal)),
  tar_target(se_proj_dfm_pcacentred_normal, lapply(proj_dfm_pcacentred_normal, \(pa) (sam_out - pa)^2),
             iteration = "list",
             pattern = map(sam_out, proj_dfm_pcacentred_normal)),
  tar_target(se_proj_dfm_ets_pca_normal, lapply(proj_dfm_ets_pca_normal, \(pa) (sam_out - pa)^2),
             iteration = "list",
             pattern = map(sam_out, proj_dfm_ets_pca_normal)),
  tar_target(se_proj_arima_normal, lapply(proj_arima_normal, \(pa) (sam_out - pa)^2),
             iteration = "list",
             pattern = map(sam_out, proj_arima_normal)),

  tar_target(se_proj_arima_pca_normal_switch_sd, lapply(proj_arima_pca_normal_switch_sd, \(pa) (sam_out - pa)^2),
             iteration = "list",
             pattern = map(sam_out, proj_arima_pca_normal_switch_sd),
             deployment = "main"),
  tar_target(se_proj_arima_pca_normal_switch_rms, lapply(proj_arima_pca_normal_switch_rms, \(pa) (sam_out - pa)^2),
             iteration = "list",
             pattern = map(sam_out, proj_arima_pca_normal_switch_rms),
             deployment = "main"),
  tar_target(se_proj_arima_pcacentred_normal_switch_sd, lapply(proj_arima_pcacentred_normal_switch_sd, \(pa) (sam_out - pa)^2),
             iteration = "list",
             pattern = map(sam_out, proj_arima_pcacentred_normal_switch_sd),
             deployment = "main"),
  tar_target(se_proj_dfm_pca_normal_switch_sd, lapply(proj_dfm_pca_normal_switch_sd, \(pa) (sam_out - pa)^2),
             iteration = "list",
             pattern = map(sam_out, proj_dfm_pca_normal_switch_sd),
             deployment = "main"),
  tar_target(se_proj_dfm_pca_normal_switch_rms, lapply(proj_dfm_pca_normal_switch_rms, \(pa) (sam_out - pa)^2),
             iteration = "list",
             pattern = map(sam_out, proj_dfm_pca_normal_switch_rms),
             deployment = "main"),
  tar_target(se_proj_dfm_pcacentred_normal_switch_sd, lapply(proj_dfm_pcacentred_normal_switch_sd, \(pa) (sam_out - pa)^2),
             iteration = "list",
             pattern = map(sam_out, proj_dfm_pcacentred_normal_switch_sd),
             deployment = "main"),

  tar_target(mse_arima, get_mse(se_arima)),
  tar_target(mse_ets, get_mse(se_ets)),
  tar_target(mse_dfm, get_mse(se_dfm)),
  tar_target(mse_arima_scale, get_mse_scale(se_arima, sd = fred_sd)),
  tar_target(mse_ets_scale, get_mse_scale(se_ets, sd = fred_sd)),
  tar_target(mse_dfm_scale, get_mse_scale(se_dfm, sd = fred_sd)),

  tar_target(mse_proj_arima_pca_normal, get_mse_proj(se_proj_arima_pca_normal),
             resources = future_ram(5)),
  tar_target(mse_proj_arima_pcacentred_normal, get_mse_proj(se_proj_arima_pcacentred_normal),
             resources = future_ram(5)),
  tar_target(mse_proj_ets_pca_normal, get_mse_proj(se_proj_ets_pca_normal),
             resources = future_ram(5)),
  tar_target(mse_proj_dfm_pca_normal, get_mse_proj(se_proj_dfm_pca_normal),
             resources = future_ram(5)),
  tar_target(mse_proj_dfm_pcacentred_normal, get_mse_proj(se_proj_dfm_pcacentred_normal),
             resources = future_ram(5)),
  tar_target(mse_proj_dfm_ets_pca_normal, get_mse_proj(se_proj_dfm_ets_pca_normal),
             resources = future_ram(5)),
  tar_target(mse_proj_arima_normal, get_mse_proj(se_proj_arima_normal),
             resources = future_ram(5)),
  tar_target(mse_proj_arima_pca_normal_scale, get_mse_proj_scale(se_proj_arima_pca_normal, sd = fred_sd),
             resources = future_ram(5)),
  # tar_target(mse_proj_arima_pcacentred_normal_scale, get_mse_proj_scale(se_proj_arima_pcacentred_normal, sd = fred_sd),
  #            resources = future_ram(5)),
  tar_target(mse_proj_ets_pca_normal_scale, get_mse_proj_scale(se_proj_ets_pca_normal, sd = fred_sd),
             resources = future_ram(5)),
  tar_target(mse_proj_dfm_pca_normal_scale, get_mse_proj_scale(se_proj_dfm_pca_normal, sd = fred_sd),
             resources = future_ram(5)),
  # tar_target(mse_proj_dfm_pcacentred_normal_scale, get_mse_proj_scale(se_proj_dfm_pcacentred_normal, sd = fred_sd),
  #            resources = future_ram(5)),
  tar_target(mse_proj_dfm_ets_pca_normal_scale, get_mse_proj_scale(se_proj_dfm_ets_pca_normal, sd = fred_sd),
             resources = future_ram(5)),
  tar_target(mse_proj_arima_normal_scale, get_mse_proj_scale(se_proj_arima_normal, sd = fred_sd),
             resources = future_ram(5)),

  tar_target(mse_proj_arima_pca_normal_switch_sd, get_mse_proj(se_proj_arima_pca_normal_switch_sd),
             resources = future_ram(5)),
  tar_target(mse_proj_arima_pca_normal_switch_rms, get_mse_proj(se_proj_arima_pca_normal_switch_rms),
             resources = future_ram(5)),
  tar_target(mse_proj_arima_pcacentred_normal_switch_sd, get_mse_proj(se_proj_arima_pcacentred_normal_switch_sd),
             resources = future_ram(5)),
  tar_target(mse_proj_dfm_pca_normal_switch_sd, get_mse_proj(se_proj_dfm_pca_normal_switch_sd),
             resources = future_ram(5)),
  tar_target(mse_proj_dfm_pca_normal_switch_rms, get_mse_proj(se_proj_dfm_pca_normal_switch_rms),
             resources = future_ram(5)),
  tar_target(mse_proj_dfm_pcacentred_normal_switch_sd, get_mse_proj(se_proj_dfm_pcacentred_normal_switch_sd),
             resources = future_ram(5)),


  tar_target(mse_arima_series, get_mse_series(se_arima)),
  tar_target(mse_ets_series, get_mse_series(se_ets)),
  tar_target(mse_dfm_series, get_mse_series(se_dfm)),
  tar_target(mse_proj_arima_pca_normal_series, get_mse_proj_series(se_proj_arima_pca_normal),
             resources = future_ram(5)),
  tar_target(mse_proj_dfm_pca_normal_series, get_mse_proj_series(se_proj_dfm_pca_normal),
             resources = future_ram(5)),
  tar_target(mse_proj_arima_pcacentred_normal_series, get_mse_proj_series(se_proj_arima_pcacentred_normal),
             resources = future_ram(5)),
  tar_target(mse_proj_dfm_pcacentred_normal_series, get_mse_proj_series(se_proj_dfm_pcacentred_normal),
             resources = future_ram(5)),
  tar_target(mse_proj_arima_normal_series, get_mse_proj_series(se_proj_arima_normal),
             resources = future_ram(5)),

  tar_target(mse_proj_arima_pca_normal_switch_sd_series, get_mse_proj_series(se_proj_arima_pca_normal_switch_sd),
             resources = future_ram(5)),
  tar_target(mse_proj_arima_pca_normal_switch_rms_series, get_mse_proj_series(se_proj_arima_pca_normal_switch_rms),
             resources = future_ram(5)),
  tar_target(mse_proj_arima_pcacentred_normal_switch_sd_series, get_mse_proj_series(se_proj_arima_pcacentred_normal_switch_sd),
             resources = future_ram(5)),
  tar_target(mse_proj_dfm_pca_normal_switch_sd_series, get_mse_proj_series(se_proj_dfm_pca_normal_switch_sd),
             resources = future_ram(5)),
  tar_target(mse_proj_dfm_pca_normal_switch_rms_series, get_mse_proj_series(se_proj_dfm_pca_normal_switch_rms),
             resources = future_ram(5)),
  tar_target(mse_proj_dfm_pcacentred_normal_switch_sd_series, get_mse_proj_series(se_proj_dfm_pcacentred_normal_switch_sd),
             resources = future_ram(5)),

  tar_target(mse_arima_cv, get_mse_cv(se_arima)),
  # tar_target(mse_ets_cv, get_mse_cv(se_ets)),
  tar_target(mse_dfm_cv, get_mse_cv(se_dfm)),
  tar_target(mse_proj_arima_pca_normal_cv, get_mse_proj_cv(se_proj_arima_pca_normal),
             resources = future_ram(5)),
  tar_target(mse_proj_dfm_pca_normal_cv, get_mse_proj_cv(se_proj_dfm_pca_normal),
             resources = future_ram(5)),
  tar_target(mse_proj_arima_normal_cv, get_mse_proj_cv(se_proj_arima_normal),
             resources = future_ram(5)),

  tar_target(mse,
             bind_rows(
               tibble(mse_arima, mse_ets, mse_dfm) %>%
                 mutate(h = row_number(),
                        p = 0,
                        proj = FALSE) %>%
                 pivot_longer(!c(h, p, proj), names_to = "model") %>%
                 mutate(model = gsub("mse_", "", model, fixed = TRUE),
                        Phi = "NA"),
               get_df_mse_proj(mse_proj_arima_pca_normal) %>%
                 mutate(model = "arima", proj = TRUE, Phi = "PCA_normal"),
               get_df_mse_proj(mse_proj_arima_pcacentred_normal) %>%
                 mutate(model = "arima", proj = TRUE, Phi = "PCAcentred_normal"),
               get_df_mse_proj(mse_proj_ets_pca_normal) %>%
                 mutate(model = "ets", proj = TRUE, Phi = "PCA_normal"),
               get_df_mse_proj(mse_proj_dfm_pca_normal) %>%
                 mutate(model = "dfm", proj = TRUE, Phi = "PCA_normal"),
               get_df_mse_proj(mse_proj_dfm_pcacentred_normal) %>%
                 mutate(model = "dfm", proj = TRUE, Phi = "PCAcentred_normal"),
               get_df_mse_proj(mse_proj_dfm_ets_pca_normal) %>%
                 mutate(model = "dfm", proj = TRUE, Phi = "PCA_normal-ets"),
               get_df_mse_proj(mse_proj_arima_normal) %>%
                 mutate(model = "arima", proj = TRUE, Phi = "normal"),
               get_df_mse_proj(mse_proj_arima_pca_normal_switch_sd) %>%
                 mutate(model = "arima", proj = TRUE, Phi = "PCA_normal_switch_sd"),
               get_df_mse_proj(mse_proj_arima_pca_normal_switch_rms) %>%
                 mutate(model = "arima", proj = TRUE, Phi = "PCA_normal_switch_rms"),
               get_df_mse_proj(mse_proj_arima_pcacentred_normal_switch_sd) %>%
                 mutate(model = "arima", proj = TRUE, Phi = "PCAcentred_normal_switch_sd"),
               get_df_mse_proj(mse_proj_dfm_pca_normal_switch_sd) %>%
                 mutate(model = "dfm", proj = TRUE, Phi = "PCA_normal_switch_sd"),
               get_df_mse_proj(mse_proj_dfm_pca_normal_switch_rms) %>%
                 mutate(model = "dfm", proj = TRUE, Phi = "PCA_normal_switch_rms"),
               get_df_mse_proj(mse_proj_dfm_pcacentred_normal_switch_sd) %>%
                 mutate(model = "dfm", proj = TRUE, Phi = "PCAcentred_normal_switch_sd"),
             ),
             resources = future_ram(4)
  ),

  tar_target(plot_mse,
             ggplot(mse, aes(x = p, y = value,
                             colour = model,
                             linetype = paste(proj, Phi, sep = "."))) +
               geom_line() +
               geom_hline(data = \(df) filter(df, !proj),
                          aes(yintercept = value,
                              colour = model,
                              linetype = paste(proj, Phi, sep = "."))) +
               facet_wrap("h", scales = "free", labeller = label_both) +
               ylab("MSE") +
               scale_linetype_discrete(
                 name = "Constraint",
                 labels = c(
                   "TRUE.PCA_normal" = "PCA+Norm.",
                   "TRUE.PCAcentred_normal" = "PCAcentred+Norm.",
                   "TRUE.PCA_normal-ets" = "ETS PCA+Norm.",
                   "TRUE.PCA_normal_switch_sd" = "PCA->Norm. by sd",
                   "TRUE.PCA_normal_switch_rms" = "PCA->Norm. by rms",
                   "TRUE.PCAcentred_normal_switch_sd" = "PCAcentred->Norm. by sd",
                   "FALSE.NA" = "No Proj.",
                   "TRUE.normal" = "Norm."
                 ))+
               geom_vline(xintercept = m)),

  tar_target(mse_scale,
             bind_rows(
               tibble(mse_arima = mse_arima_scale,
                      mse_ets = mse_ets_scale,
                      mse_dfm = mse_dfm_scale) %>%
                 mutate(h = row_number(),
                        p = 0,
                        proj = FALSE) %>%
                 pivot_longer(!c(h, p, proj), names_to = "model") %>%
                 mutate(model = gsub("mse_", "", model, fixed = TRUE),
                        Phi = "NA"),
               get_df_mse_proj(mse_proj_arima_pca_normal_scale) %>%
                 mutate(model = "arima", proj = TRUE, Phi = "PCA_normal"),
               get_df_mse_proj(mse_proj_ets_pca_normal_scale) %>%
                 mutate(model = "ets", proj = TRUE, Phi = "PCA_normal"),
               get_df_mse_proj(mse_proj_dfm_pca_normal_scale) %>%
                 mutate(model = "dfm", proj = TRUE, Phi = "PCA_normal"),
               get_df_mse_proj(mse_proj_dfm_ets_pca_normal_scale) %>%
                 mutate(model = "dfm", proj = TRUE, Phi = "PCA_normal-ets"),
               get_df_mse_proj(mse_proj_arima_normal_scale) %>%
                 mutate(model = "arima", proj = TRUE, Phi = "normal")
             ),
             resources = future_ram(4)
  ),

  tar_target(plot_mse_scale,
             ggplot(mse_scale, aes(x = p, y = value,
                                   colour = model,
                                   linetype = paste(proj, Phi, sep = "."))) +
               geom_line() +
               geom_hline(data = \(df) filter(df, !proj),
                          aes(yintercept = value,
                              colour = model,
                              linetype = paste(proj, Phi, sep = "."))) +
               facet_wrap("h", scales = "free", labeller = label_both) +
               ylab("MSE") +
               scale_linetype_manual(
                 name = "Constraint",
                 values = c(
                   "TRUE.PCA_normal" = "solid",
                   "TRUE.PCA_normal-ets" = "dotdash",
                   "FALSE.NA" = "dotted",
                   "TRUE.normal" = "longdash"
                 ),
                 labels = c(
                   "TRUE.PCA_normal" = "PCA+Norm.",
                   "TRUE.PCA_normal-ets" = "ETS PCA+Norm.",
                   "FALSE.NA" = "No Proj.",
                   "TRUE.normal" = "Norm."
                 ))+
               geom_vline(xintercept = m))

)
