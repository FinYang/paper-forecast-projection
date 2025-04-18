# Created by use_targets().
# Follow the comments below to fill in this target script.
# Then follow the manual to check and run the pipeline:
#   https://books.ropensci.org/targets/walkthrough.html#inspect-the-pipeline # nolint

# Load packages required to define the pipeline:
library(targets)
library(tarchetypes)
library(future)
library(future.batchtools)
# library(tarchetypes) # Load other packages as needed. # nolint
library(rlang)
library(dtplyr)
library(tsfeatures)
library(progressr)
# library(mgcv)
library(tidyverse)
# Set target options:
tar_option_set(
  packages = c(
    "tidyverse",
    "forecast",
    "tsDyn"
  ), # packages that your targets need to run
  format = "qs", # default storage format
  deployment = "worker",
  storage = "worker",
  retrieval = "worker",
  seed = 0,
  error = "abridge",
  iteration = "list",
  memory = "transient",
  resources = tar_resources(
    future = tar_resources_future(
      plan = future::tweak(
        batchtools_slurm,
        template = "future.tmpl",
        resources = list(
          ntasks = 1,
          ncpus = 1,
          walltime = 60 * 60 * 24, # seconds
          memory = 1024 * 1 # megabytes
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
          memory = 1024 * G
        )
      )
    )
  )
}

# Run the R scripts in the R/ folder with your custom functions:
tar_source()
# source("other_functions.R") # Source other scripts as needed. # nolint

# Global variables
n <- 400
.n_sim <- ifelse(Sys.info()["sysname"] == "Linux", 220, 1)
.n_lag <- 3
.forecast_h <- 12
p <- 300

m <- 70

# Replace the target list below with your own:
list(
  # tar_target(error_var, rWishart(1, m, diag(m))[,,1]),
  tar_target(visnights_mat_file, "data/visnights_mat.qs",
    format = "file",
    deployment = "main"
  ),
  tar_target(visnights_mat, qs::qread(visnights_mat_file), deployment = "main"),
  tar_target(visnights_mat_sta, apply(visnights_mat, 2, \(x) x %>%
    # forecast::BoxCox(lambda = forecast::BoxCox.lambda(.)) %>%
    diff(lag = 12) %>%
    scale()),
  deployment = "main"
  ),
  tar_target(model_true, lineVar(visnights_mat_sta[, 1:70], lag = .n_lag),
    deployment = "main"
  ),
  tar_target(B_true, coef(model_true), deployment = "main"),
  tar_target(sim_ls, replicate(.n_sim, VAR.sim(B = B_true, lag = .n_lag, n = n + .forecast_h), simplify = FALSE),
    deployment = "main"
  ),
  tar_target(Wz_true, diag(m), deployment = "main"),
  tar_target(sim_in, sim_ls[seq_len(n), ],
    pattern = map(sim_ls),
    deployment = "main"
  ),
  tar_target(sim_out, sim_ls[seq(n + 1, n + .forecast_h), ],
    pattern = map(sim_ls),
    deployment = "main"
  ),
  tar_target(fc_true,
    {
      sam <- matrix(nrow = .n_lag + .forecast_h, ncol = ncol(sim_in))
      sam[seq_len(.n_lag), ] <- sim_in[seq(nrow(sim_in) - .n_lag + 1, nrow(sim_in)), ]
      fc <- matrix(nrow = .forecast_h, ncol = ncol(sim_in))
      for (i in seq_len(.forecast_h)) {
        fc[i, ] <- sam[i + .n_lag, ] <- t(B_true[, -1] %*% c(t(sam[seq(i + .n_lag - 1, i, by = -1), ])) + B_true[, 1])
      }
      fc
    },
    pattern = map(sim_in)
  ),
  tar_target(res_true,
    {
      res <- matrix(nrow = n, ncol = ncol(sim_in))
      sam <- rbind(
        matrix(0, nrow = .n_lag, ncol = ncol(sim_in)),
        sim_in
      )
      for (i in seq_len(n)) {
        res[i, ] <- sam[i + .n_lag, ] -
          t(B_true[, -1] %*% c(t(sam[seq(i + .n_lag - 1, i, by = -1), ])) + B_true[, 1])
      }
      res
    },
    pattern = map(sim_in)
  ),
  tar_target(out_arima, arima2(sim_in, .forecast_h),
    pattern = map(sim_in)
  ),
  tar_target(fc_arima, get_fc(out_arima),
    pattern = map(out_arima),
    deployment = "main"
  ),
  tar_target(res_arima, get_res(out_arima),
    pattern = map(out_arima),
    deployment = "main"
  ),
  tar_target(fitted_arima, get_fitted(out_arima),
    pattern = map(out_arima),
    deployment = "main"
  ),
  tar_target(out_var, var_est(sim_in, .n_lag, .forecast_h),
    pattern = map(sim_in)
  ),
  tar_target(fc_var, out_var$fc,
    pattern = map(out_var),
    deployment = "main"
  ),
  tar_target(res_var, out_var$res,
    pattern = map(out_var),
    deployment = "main"
  ),
  tar_target(pca, component(sim_in, "PCA", p = p),
    pattern = map(sim_in),
    cue = tar_cue(command = FALSE),
    deployment = "main"
  ),
  tar_target(out_arima_pca, arima2(pca$x, .forecast_h),
    pattern = map(pca)
  ),
  tar_target(fc_arima_pca, get_fc(out_arima_pca),
    pattern = map(out_arima_pca),
    deployment = "main"
  ),
  tar_target(res_arima_pca, get_res(out_arima_pca),
    pattern = map(out_arima_pca)
  ),
  tar_target(fitted_arima_pca, get_fitted(out_arima_pca),
    pattern = map(out_arima_pca)
  ),
  tar_target(pca_uniform, component(sim_in, "PCA_uniform", p = p),
    pattern = map(sim_in),
    cue = tar_cue(command = FALSE),
    deployment = "main"
  ),
  tar_target(out_arima_pca_uniform, arima(pca_uniform$x, .forecast_h),
    pattern = map(pca_uniform)
  ),
  tar_target(fc_arima_pca_uniform, get_fc(out_arima_pca_uniform),
    pattern = map(out_arima_pca_uniform),
    deployment = "main"
  ),
  tar_target(res_arima_pca_uniform, get_res(out_arima_pca_uniform),
    pattern = map(out_arima_pca_uniform)
  ),
  tar_target(pcacentred_normal, component(sim_in, "PCAcentred_normal", p = p),
    pattern = map(sim_in),
    cue = tar_cue(command = FALSE),
    deployment = "main"
  ),
  tar_target(out_arima_pcacentred_normal, arima2(pcacentred_normal$x, .forecast_h),
    pattern = map(pcacentred_normal)
  ),
  tar_target(fc_arima_pcacentred_normal, get_fc(out_arima_pcacentred_normal),
    pattern = map(out_arima_pcacentred_normal),
    deployment = "main"
  ),
  tar_target(res_arima_pcacentred_normal, get_res(out_arima_pcacentred_normal),
    pattern = map(out_arima_pcacentred_normal)
  ),
  tar_target(fitted_arima_pcacentred_normal, get_fitted(out_arima_pcacentred_normal),
    pattern = map(out_arima_pcacentred_normal)
  ),
  tar_target(pcacentred_uniform, component(sim_in, "PCAcentred_uniform", p = p),
    pattern = map(sim_in),
    cue = tar_cue(command = FALSE),
    deployment = "main"
  ),
  tar_target(out_arima_pcacentred_uniform, arima(pcacentred_uniform$x, .forecast_h),
    pattern = map(pcacentred_uniform)
  ),
  tar_target(fc_arima_pcacentred_uniform, get_fc(out_arima_pcacentred_uniform),
    pattern = map(out_arima_pcacentred_uniform),
    deployment = "main"
  ),
  tar_target(res_arima_pcacentred_uniform, get_res(out_arima_pcacentred_uniform),
    pattern = map(out_arima_pcacentred_uniform)
  ),
  tar_target(ortho_normal, component(sim_in, "ortho_normal", p = p),
    pattern = map(sim_in),
    cue = tar_cue(command = FALSE),
    deployment = "main"
  ),
  tar_target(out_arima_ortho_normal, arima(ortho_normal$x, .forecast_h),
    pattern = map(ortho_normal)
  ),
  tar_target(fc_arima_ortho_normal, get_fc(out_arima_ortho_normal),
    pattern = map(out_arima_ortho_normal),
    deployment = "main"
  ),
  tar_target(res_arima_ortho_normal, get_res(out_arima_ortho_normal),
    pattern = map(out_arima_ortho_normal)
  ),
  tar_target(normal, component(sim_in, "normal", p = p),
    pattern = map(sim_in),
    deployment = "main"
  ),
  tar_target(out_arima_normal, arima(normal$x, .forecast_h),
    pattern = map(normal)
  ),
  tar_target(fc_arima_normal, get_fc(out_arima_normal),
    pattern = map(out_arima_normal),
    deployment = "main"
  ),
  tar_target(res_arima_normal, get_res(out_arima_normal),
    pattern = map(out_arima_normal)
  ),
  tar_target(uniform, component(sim_in, "uniform", p = p),
    pattern = map(sim_in),
    cue = tar_cue(command = FALSE),
    deployment = "main"
  ),
  tar_target(out_arima_uniform, arima(uniform$x, .forecast_h),
    pattern = map(uniform)
  ),
  tar_target(fc_arima_uniform, get_fc(out_arima_uniform),
    pattern = map(out_arima_uniform),
    deployment = "main"
  ),
  tar_target(res_arima_uniform, get_res(out_arima_uniform),
    pattern = map(out_arima_uniform)
  ),
  tar_target(out_dfm,
    {
      lapply(seq_len(.forecast_h), \(.h)
      # predict_dfm(sim_in, pca$x[,seq_len(5)], y_lag = 3, h = .h))
      auto_dfm(sim_in, n_factor = 6, y_lag = 3, f_lag = 3, h = .h))
    },
    pattern = map(sim_in)
  ),
  tar_target(fc_dfm,
    do.call(rbind, lapply(out_dfm, getElement, "fc")),
    pattern = map(out_dfm),
    deployment = "main"
  ),
  tar_target(res_dfm,
    lapply(out_dfm, getElement, "res"),
    pattern = map(out_dfm),
    deployment = "main"
  ),
  tar_target(W_arima, get_W(res_arima, res_arima_pca),
    pattern = map(res_arima, res_arima_pca),
    resources = future_ram(6)
  ),
  tar_target(W_arima_pcacentred_normal, get_W(res_arima, res_arima_pcacentred_normal),
    pattern = map(res_arima, res_arima_pcacentred_normal),
    resources = future_ram(6)
  ),
  tar_target(W_var, get_W(res_var, res_arima_pca),
    pattern = map(res_var, res_arima_pca),
    resources = future_ram(6)
  ),
  tar_target(W_var_pcacentred_normal, get_W(res_var, res_arima_pcacentred_normal),
    pattern = map(res_var, res_arima_pcacentred_normal),
    resources = future_ram(6)
  ),
  tar_target(W_arima_wls,
    map2(
      res_arima, res_arima_pca,
      \(a, b){
        vars <- apply(cbind(a, b), 2, var, na.rm = TRUE)
        lapply(
          seq_len(p),
          \(pp) diag(vars[seq_len(m + pp)])
        )
      }
    ),
    pattern = map(res_arima, res_arima_pca),
    resources = future_ram(6)
  ),
  tar_target(W_dfm, get_W(res_dfm, res_arima_pca),
    pattern = map(res_dfm, res_arima_pca),
    resources = future_ram(6)
  ),
  tar_target(W_dfm_pcacentred_normal, get_W(res_dfm, res_arima_pcacentred_normal),
    pattern = map(res_dfm, res_arima_pcacentred_normal),
    resources = future_ram(6)
  ),
  tar_target(W_true,
    {
      res <- cbind(res_true, res_arima_pca[[1]])
      lapply(
        seq_len(p),
        \(pp) corpcor::cov.shrink(res[, seq_len(m + pp)], verbose = FALSE)
      )
    },
    pattern = map(res_true, res_arima_pca)
  ),
  tar_target(W_true_pcacentred_normal,
    {
      res <- cbind(res_true, res_arima_pcacentred_normal[[1]])
      lapply(
        seq_len(p),
        \(pp) corpcor::cov.shrink(res[, seq_len(m + pp)], verbose = FALSE)
      )
    },
    pattern = map(res_true, res_arima_pcacentred_normal)
  ),
  tar_target(W_arima_normal, get_W(res_arima, res_arima_normal),
    pattern = map(res_arima, res_arima_normal),
    resources = future_ram(6)
  ),
  tar_target(W_arima_uniform, get_W(res_arima, res_arima_uniform),
    pattern = map(res_arima, res_arima_uniform),
    resources = future_ram(6)
  ),
  tar_target(W_arima_pca_uniform, get_W(res_arima, res_arima_pca_uniform),
    pattern = map(res_arima, res_arima_pca_uniform),
    resources = future_ram(6)
  ),
  tar_target(W_arima_pcacentred_uniform, get_W(res_arima, res_arima_pcacentred_uniform),
    pattern = map(res_arima, res_arima_pcacentred_uniform),
    resources = future_ram(6)
  ),
  tar_target(W_arima_ortho_normal, get_W(res_arima, res_arima_ortho_normal),
    pattern = map(res_arima, res_arima_ortho_normal),
    resources = future_ram(6)
  ),
  tar_target(proj_iter_arima,
    project_iter(sim_in, fitted_arima, fitted_arima_pca,
      res_arima_pca,
      fc_arima, fc_arima_pca,
      Phi = pca$Phi
    ),
    pattern = map(
      sim_in, fitted_arima, fitted_arima_pca,
      res_arima_pca,
      fc_arima, fc_arima_pca, pca
    ),
    resources = future_ram(6)
  ),
  tar_target(proj_arima,
    project(
      cbind(fc_arima, fc_arima_pca),
      W = W_arima,
      Phi = pca$Phi
    ),
    pattern = map(fc_arima, fc_arima_pca, W_arima, pca),
    resources = future_ram(4)
  ),
  tar_target(proj_arima_pcacentred_normal,
    project(
      cbind(fc_arima, fc_arima_pcacentred_normal),
      W = W_arima_pcacentred_normal,
      Phi = pcacentred_normal$Phi
    ),
    pattern = map(fc_arima, fc_arima_pcacentred_normal, W_arima_pcacentred_normal, pcacentred_normal),
    resources = future_ram(4)
  ),

  # using W (in turn res) for h=1 only for fc for all h
  tar_target(proj_arima_pcacentred_normal_h1,
    project_1(
      cbind(fc_arima, fc_arima_pcacentred_normal),
      W = W_arima_pcacentred_normal[[1]],
      Phi = pcacentred_normal$Phi
    ),
    pattern = map(fc_arima, fc_arima_pcacentred_normal, W_arima_pcacentred_normal, pcacentred_normal),
    resources = future_ram(4)
  ),
  tar_target(proj_var,
    project(
      cbind(fc_var, fc_arima_pca),
      W = W_var,
      Phi = pca$Phi
    ),
    pattern = map(fc_var, fc_arima_pca, W_var, pca),
    resources = future_ram(4)
  ),
  tar_target(proj_var_pcacentred_normal,
    project(
      cbind(fc_var, fc_arima_pcacentred_normal),
      W = W_var_pcacentred_normal,
      Phi = pcacentred_normal$Phi
    ),
    pattern = map(fc_var, fc_arima_pcacentred_normal, W_var_pcacentred_normal, pcacentred_normal),
    resources = future_ram(4)
  ),
  tar_target(proj_arima_wls,
    project(
      cbind(fc_arima, fc_arima_pca),
      W = W_arima_wls,
      Phi = pca$Phi
    ),
    pattern = map(fc_arima, fc_arima_pca, W_arima_wls, pca),
    resources = future_ram(4)
  ),
  tar_target(proj_dfm,
    project(
      cbind(fc_dfm, fc_arima_pca),
      W = W_dfm,
      Phi = pca$Phi
    ),
    pattern = map(fc_dfm, fc_arima_pca, W_dfm, pca),
    resources = future_ram(4)
  ),
  tar_target(proj_dfm_pcacentred_normal,
    project(
      cbind(fc_dfm, fc_arima_pcacentred_normal),
      W = W_dfm_pcacentred_normal,
      Phi = pcacentred_normal$Phi
    ),
    pattern = map(fc_dfm, fc_arima_pcacentred_normal, W_dfm_pcacentred_normal, pcacentred_normal),
    resources = future_ram(4)
  ),
  # using W (in turn res) for h=1 only for fc for all h
  tar_target(proj_dfm_pcacentred_normal_h1,
    project_1(
      cbind(fc_dfm, fc_arima_pcacentred_normal),
      W = W_dfm_pcacentred_normal[[1]],
      Phi = pcacentred_normal$Phi
    ),
    pattern = map(fc_dfm, fc_arima_pcacentred_normal, W_dfm_pcacentred_normal, pcacentred_normal),
    resources = future_ram(4)
  ),
  tar_target(proj_true,
    project_1(
      cbind(fc_true, fc_arima_pca),
      W = W_true,
      Phi = pca$Phi
    ),
    pattern = map(fc_true, fc_arima_pca, W_true, pca),
    resources = future_ram(1)
  ),
  tar_target(proj_true_pcacentred_normal,
    project_1(
      cbind(fc_true, fc_arima_pcacentred_normal),
      W = W_true_pcacentred_normal,
      Phi = pcacentred_normal$Phi
    ),
    pattern = map(fc_true, fc_arima_pcacentred_normal, W_true_pcacentred_normal, pcacentred_normal),
    resources = future_ram(1)
  ),
  tar_target(proj_arima_normal,
    project(
      cbind(fc_arima, fc_arima_normal),
      W = W_arima_normal,
      Phi = normal$Phi
    ),
    pattern = map(fc_arima, fc_arima_normal, W_arima_normal, normal),
    resources = future_ram(4)
  ),
  # using W (in turn res) for h=1 only for fc for all h
  tar_target(proj_arima_normal_h1,
    project_1(
      cbind(fc_arima, fc_arima_normal),
      W = W_arima_normal[[1]],
      Phi = normal$Phi
    ),
    pattern = map(fc_arima, fc_arima_normal, W_arima_normal, normal),
    resources = future_ram(4)
  ),
  tar_target(proj_arima_uniform,
    project(
      cbind(fc_arima, fc_arima_uniform),
      W = W_arima_uniform,
      Phi = uniform$Phi
    ),
    pattern = map(fc_arima, fc_arima_uniform, W_arima_uniform, uniform),
    resources = future_ram(4)
  ),
  tar_target(proj_arima_pca_uniform,
    project(
      cbind(fc_arima, fc_arima_pca_uniform),
      W = W_arima_pca_uniform,
      Phi = pca_uniform$Phi
    ),
    pattern = map(fc_arima, fc_arima_pca_uniform, W_arima_pca_uniform, pca_uniform),
    resources = future_ram(4)
  ),
  tar_target(proj_arima_pcacentred_uniform,
    project(
      cbind(fc_arima, fc_arima_pcacentred_uniform),
      W = W_arima_pcacentred_uniform,
      Phi = pcacentred_uniform$Phi
    ),
    pattern = map(fc_arima, fc_arima_pcacentred_uniform, W_arima_pcacentred_uniform, pcacentred_uniform),
    resources = future_ram(4)
  ),
  tar_target(proj_arima_ortho_normal,
    project(
      cbind(fc_arima, fc_arima_ortho_normal),
      W = W_arima_ortho_normal,
      Phi = ortho_normal$Phi
    ),
    pattern = map(fc_arima, fc_arima_ortho_normal, W_arima_ortho_normal, ortho_normal),
    resources = future_ram(4)
  ),
  tar_target(pca_normal_switch_from_sd,
    which(apply(pca$x, 2, sd) < mean(apply(normal$x[, 1:100], 2, sd)))[[1]],
    pattern = map(pca, normal),
    deployment = "main"
  ),
  tar_target(proj_arima_pca_normal_switch_sd,
    project_switch(
      fc = fc_arima,
      fc_comp1 = fc_arima_pca,
      fc_comp2 = fc_arima_normal,
      Phi1 = pca$Phi,
      Phi2 = normal$Phi,
      res = res_arima,
      res_comp1 = res_arima_pca,
      res_comp2 = res_arima_normal,
      switch_from = pca_normal_switch_from_sd
    ),
    pattern = map(
      fc_arima, fc_arima_pca, fc_arima_normal,
      pca, normal,
      res_arima, res_arima_pca, res_arima_normal,
      pca_normal_switch_from_sd
    ),
    resources = future_ram(10)
  ),
  tar_target(pca_normal_switch_from_rms,
    which(sqrt(colMeans(pca$x^2)) < mean(sqrt(colMeans(normal$x[, 1:100]^2))))[[1]],
    pattern = map(pca, normal),
    deployment = "main"
  ),
  tar_target(proj_arima_pca_normal_switch_rms,
    project_switch(
      fc = fc_arima,
      fc_comp1 = fc_arima_pca,
      fc_comp2 = fc_arima_normal,
      Phi1 = pca$Phi,
      Phi2 = normal$Phi,
      res = res_arima,
      res_comp1 = res_arima_pca,
      res_comp2 = res_arima_normal,
      switch_from = pca_normal_switch_from_rms
    ),
    pattern = map(
      fc_arima, fc_arima_pca, fc_arima_normal,
      pca, normal,
      res_arima, res_arima_pca, res_arima_normal,
      pca_normal_switch_from_rms
    ),
    resources = future_ram(10)
  ),
  tar_target(pcacentred_normal_switch_from_sd,
    which(apply(pcacentred_normal$x, 2, sd) < mean(apply(normal$x[, 1:100], 2, sd)))[[1]],
    pattern = map(pcacentred_normal, normal),
    deployment = "main"
  ),
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
      switch_from = pcacentred_normal_switch_from_sd
    ),
    pattern = map(
      fc_arima, fc_arima_pcacentred_normal, fc_arima_normal,
      pcacentred_normal, normal,
      res_arima, res_arima_pcacentred_normal, res_arima_normal,
      pcacentred_normal_switch_from_sd
    ),
    resources = future_ram(10)
  ),
  tar_target(check_fc,
    {
      example_size <- 6
      example_sample_size <- .forecast_h * 3

      list(
        sim_in,
        sim_out,
        fc_true,
        fc_arima,
        fc_dfm,
        proj_arima[[1]][p],
        proj_true[[1]][p],
        proj_dfm[[1]][p]
      ) %>%
        lapply(\(x) tail(x[[1]][, seq_len(example_size)], example_sample_size)) %>%
        do.call(rbind, .) %>%
        `colnames<-`(paste0("V", seq_len(example_size))) %>%
        as_tibble() %>%
        mutate(
          index = c(
            seq(to = n, length.out = example_sample_size),
            rep(n + seq_len(.forecast_h), 7)
          ),
          type = c(
            rep("actual", example_sample_size + .forecast_h),
            rep("fc", .forecast_h * 6)
          ),
          model = c(
            rep(NA_character_, example_sample_size + .forecast_h),
            rep("true", .forecast_h),
            rep("arima", .forecast_h),
            rep("dfm", .forecast_h),
            rep("arima", .forecast_h),
            rep("true", .forecast_h),
            rep("dfm", .forecast_h)
          ),
          proj = c(
            rep(FALSE, example_sample_size + .forecast_h * 4),
            rep(TRUE, .forecast_h * 3)
          )
        ) %>%
        pivot_longer(starts_with("V"))
    },
    resources = future_ram(4)
  ),
  tar_target(
    plot_check_fc,
    check_fc %>%
      ggplot(aes(x = index, y = value)) +
      facet_wrap("name", scales = "free") +
      geom_line(aes(linetype = type, colour = interaction(model, proj)))
  ),
  tar_target(se_true, (sim_out - fc_true)^2,
    pattern = map(sim_out, fc_true),
    deployment = "main"
  ),
  tar_target(se_arima, (sim_out - fc_arima)^2,
    pattern = map(sim_out, fc_arima),
    deployment = "main"
  ),
  tar_target(se_var, (sim_out - fc_var)^2,
    pattern = map(sim_out, fc_var),
    deployment = "main"
  ),
  tar_target(se_dfm, (sim_out - fc_dfm)^2,
    pattern = map(sim_out, fc_dfm),
    deployment = "main"
  ),
  tar_target(se_proj_iter_arima, lapply(proj_iter_arima, \(pa) (sim_out - pa)^2),
    pattern = map(sim_out, proj_iter_arima),
    deployment = "main"
  ),
  tar_target(se_proj_arima, lapply(proj_arima, \(pa) (sim_out - pa)^2),
    pattern = map(sim_out, proj_arima),
    deployment = "main"
  ),
  tar_target(se_proj_arima_pcacentred_normal, lapply(proj_arima_pcacentred_normal, \(pa) (sim_out - pa)^2),
    pattern = map(sim_out, proj_arima_pcacentred_normal),
    deployment = "main"
  ),
  tar_target(se_proj_arima_pcacentred_normal_h1, lapply(proj_arima_pcacentred_normal_h1, \(pa) (sim_out - pa)^2),
    pattern = map(sim_out, proj_arima_pcacentred_normal_h1),
    deployment = "main"
  ),
  tar_target(se_proj_var, lapply(proj_var, \(pa) (sim_out - pa)^2),
    pattern = map(sim_out, proj_var),
    deployment = "main"
  ),
  tar_target(se_proj_var_pcacentred_normal, lapply(proj_var_pcacentred_normal, \(pa) (sim_out - pa)^2),
    pattern = map(sim_out, proj_var_pcacentred_normal),
    deployment = "main"
  ),
  tar_target(se_proj_arima_wls, lapply(proj_arima_wls, \(pa) (sim_out - pa)^2),
    pattern = map(sim_out, proj_arima_wls),
    deployment = "main"
  ),
  tar_target(se_proj_dfm, lapply(proj_dfm, \(pa) (sim_out - pa)^2),
    pattern = map(sim_out, proj_dfm),
    deployment = "main"
  ),
  tar_target(se_proj_dfm_pcacentred_normal, lapply(proj_dfm_pcacentred_normal, \(pa) (sim_out - pa)^2),
    pattern = map(sim_out, proj_dfm_pcacentred_normal),
    deployment = "main"
  ),
  tar_target(se_proj_dfm_pcacentred_normal_h1, lapply(proj_dfm_pcacentred_normal_h1, \(pa) (sim_out - pa)^2),
    pattern = map(sim_out, proj_dfm_pcacentred_normal_h1),
    deployment = "main"
  ),
  tar_target(se_proj_true, lapply(proj_true, \(pa) (sim_out - pa)^2),
    pattern = map(sim_out, proj_true),
    deployment = "main"
  ),
  tar_target(se_proj_true_pcacentred_normal, lapply(proj_true_pcacentred_normal, \(pa) (sim_out - pa)^2),
    pattern = map(sim_out, proj_true_pcacentred_normal),
    deployment = "main"
  ),
  tar_target(se_proj_arima_normal, lapply(proj_arima_normal, \(pa) (sim_out - pa)^2),
    pattern = map(sim_out, proj_arima_normal),
    deployment = "main"
  ),
  tar_target(se_proj_arima_normal_h1, lapply(proj_arima_normal_h1, \(pa) (sim_out - pa)^2),
    pattern = map(sim_out, proj_arima_normal_h1),
    deployment = "main"
  ),
  tar_target(se_proj_arima_uniform, lapply(proj_arima_uniform, \(pa) (sim_out - pa)^2),
    pattern = map(sim_out, proj_arima_uniform),
    deployment = "main"
  ),
  tar_target(se_proj_arima_pca_uniform, lapply(proj_arima_pca_uniform, \(pa) (sim_out - pa)^2),
    pattern = map(sim_out, proj_arima_pca_uniform),
    deployment = "main"
  ),
  tar_target(se_proj_arima_pcacentred_uniform, lapply(proj_arima_pcacentred_uniform, \(pa) (sim_out - pa)^2),
    pattern = map(sim_out, proj_arima_pcacentred_uniform),
    deployment = "main"
  ),
  tar_target(se_proj_arima_ortho_normal, lapply(proj_arima_ortho_normal, \(pa) (sim_out - pa)^2),
    pattern = map(sim_out, proj_arima_ortho_normal),
    deployment = "main"
  ),
  tar_target(se_proj_arima_pca_normal_switch_sd, lapply(proj_arima_pca_normal_switch_sd, \(pa) (sim_out - pa)^2),
    pattern = map(sim_out, proj_arima_pca_normal_switch_sd),
    deployment = "main"
  ),
  tar_target(se_proj_arima_pca_normal_switch_rms, lapply(proj_arima_pca_normal_switch_rms, \(pa) (sim_out - pa)^2),
    pattern = map(sim_out, proj_arima_pca_normal_switch_rms),
    deployment = "main"
  ),
  tar_target(se_proj_arima_pcacentred_normal_switch_sd, lapply(proj_arima_pcacentred_normal_switch_sd, \(pa) (sim_out - pa)^2),
    pattern = map(sim_out, proj_arima_pcacentred_normal_switch_sd),
    deployment = "main"
  ),
  # tar_target(se,
  #            list(se_true, se_arima) %>%
  #              map2(c("true", "arima"), \(se, model)
  #                   lapply(se, \(x) {
  #                     colnames(x) <- seq_len(ncol(x))
  #                     as_tibble(x) %>%
  #                       mutate(h = row_number()) %>%
  #                       pivot_longer(-h, names_to = "name")
  #                   }) %>%
  #                     unname() %>%
  #                     imap(\(x, i) mutate(x, .id = i)) %>%
  #                     bind_rows() %>%
  #                     mutate(model = model,
  #                            proj = FALSE,
  #                            p=0)
  #              ) %>%
  #              bind_rows() %>%
  #              bind_rows(
  #                se_proj_arima %>%
  #                  unname() %>%
  #                  imap(\(se, i){
  #                    imap(se, \(x, p){
  #                      colnames(x) <- seq_len(ncol(x))
  #                      as_tibble(x) %>%
  #                        mutate(h = row_number(), p=p) %>%
  #                        pivot_longer(!c(h, p), names_to = "name")
  #                    }) %>%
  #                      bind_rows() %>%
  #                      mutate(.id = i)
  #                  }) %>%
  #                  bind_rows() %>%
  #                  mutate(model = "arima", proj = TRUE)
  #              )
  #
  # ),
  # tar_target(plot_se,
  #            se %>%
  #              group_by(h, .id, model, proj, p) %>%
  #              summarise(value = mean(value), .groups = "drop") %>%
  #            ggplot(aes(x = p, y = value,
  #                           colour = model,
  #                           linetype = proj)) +
  #              geom_smooth() +
  #              # geom_point() +
  #              # geom_hline(data = \(df) filter(df, !proj) %>%
  #              #              group_by(model) %>%
  #              #              summarise(p, h, value = mean(value), proj,
  #              #                        .groups = "drop"),
  #              #            aes(yintercept = value,
  #              #                colour = model,
  #              #                linetype = proj)) +
  #              geom_smooth(data = \(df)
  #                          lapply(seq_len(p), \(pp, df)
  #                             mutate(df, p=pp), df=
  #                               filter(df, !proj)
  #                             ) %>%
  #                            bind_rows()) +
  #              # geom_point(data = \(df)
  #              #             lapply(seq_len(p), \(pp, df)
  #              #                mutate(df, p=pp), df=
  #              #                  filter(df, !proj)
  #              #                  ) %>%
  #              #               bind_rows()) +
  #              facet_wrap("h", scales = "free", labeller = label_both) +
  #              ylab("MSE") +
  #              scale_linetype_manual(values = c("TRUE" = "solid", "FALSE" = "dashed"))+
  #              geom_vline(xintercept = m)),

  tar_target(mse_true, get_mse(se_true)),
  tar_target(mse_arima, get_mse(se_arima)),
  tar_target(mse_var, get_mse(se_var)),
  tar_target(mse_dfm, get_mse(se_dfm)),
  tar_target(mse_proj_iter_arima, get_mse_proj(se_proj_iter_arima),
    resources = future_ram(3)
  ),
  tar_target(mse_proj_arima, get_mse_proj(se_proj_arima),
    resources = future_ram(3)
  ),
  tar_target(mse_proj_arima_pcacentred_normal, get_mse_proj(se_proj_arima_pcacentred_normal),
    resources = future_ram(3)
  ),
  tar_target(mse_proj_arima_pcacentred_normal_h1, get_mse_proj(se_proj_arima_pcacentred_normal_h1),
    resources = future_ram(3)
  ),
  tar_target(mse_proj_var, get_mse_proj(se_proj_var),
    resources = future_ram(3)
  ),
  tar_target(mse_proj_var_pcacentred_normal, get_mse_proj(se_proj_var_pcacentred_normal),
    resources = future_ram(3)
  ),
  tar_target(mse_proj_arima_wls, get_mse_proj(se_proj_arima_wls),
    resources = future_ram(3)
  ),
  tar_target(mse_proj_dfm, get_mse_proj(se_proj_dfm),
    resources = future_ram(3)
  ),
  tar_target(mse_proj_dfm_pcacentred_normal, get_mse_proj(se_proj_dfm_pcacentred_normal),
    resources = future_ram(3)
  ),
  tar_target(mse_proj_dfm_pcacentred_normal_h1, get_mse_proj(se_proj_dfm_pcacentred_normal_h1),
    resources = future_ram(3)
  ),
  tar_target(mse_proj_true, get_mse_proj(se_proj_true),
    resources = future_ram(3)
  ),
  tar_target(mse_proj_true_pcacentred_normal, get_mse_proj(se_proj_true_pcacentred_normal),
    resources = future_ram(3)
  ),
  tar_target(mse_proj_arima_normal, get_mse_proj(se_proj_arima_normal),
    resources = future_ram(3)
  ),
  tar_target(mse_proj_arima_normal_h1, get_mse_proj(se_proj_arima_normal_h1),
    resources = future_ram(3)
  ),
  tar_target(mse_proj_arima_uniform, get_mse_proj(se_proj_arima_uniform),
    resources = future_ram(3)
  ),
  tar_target(mse_proj_arima_pca_uniform, get_mse_proj(se_proj_arima_pca_uniform),
    resources = future_ram(3)
  ),
  tar_target(mse_proj_arima_pcacentred_uniform, get_mse_proj(se_proj_arima_pcacentred_uniform),
    resources = future_ram(3)
  ),
  tar_target(mse_proj_arima_ortho_normal, get_mse_proj(se_proj_arima_ortho_normal),
    resources = future_ram(3)
  ),
  tar_target(mse_proj_arima_pca_normal_switch_sd, get_mse_proj(se_proj_arima_pca_normal_switch_sd),
    resources = future_ram(5)
  ),
  tar_target(mse_proj_arima_pca_normal_switch_rms, get_mse_proj(se_proj_arima_pca_normal_switch_rms),
    resources = future_ram(5)
  ),
  tar_target(mse_proj_arima_pcacentred_normal_switch_sd, get_mse_proj(se_proj_arima_pcacentred_normal_switch_sd),
    resources = future_ram(5)
  ),
  # by series
  tar_target(mse_true_series, get_mse_series(se_true)),
  tar_target(mse_arima_series, get_mse_series(se_arima)),
  tar_target(mse_var_series, get_mse_series(se_var)),
  tar_target(mse_dfm_series, get_mse_series(se_dfm)),
  tar_target(mse_proj_iter_arima_series, get_mse_proj_series(se_proj_iter_arima),
    resources = future_ram(3)
  ),
  tar_target(mse_proj_arima_series, get_mse_proj_series(se_proj_arima),
    resources = future_ram(3)
  ),
  tar_target(mse_proj_arima_pcacentred_normal_series, get_mse_proj_series(se_proj_arima_pcacentred_normal),
    resources = future_ram(3)
  ),
  tar_target(mse_proj_arima_pcacentred_normal_series_h1, get_mse_proj_series(se_proj_arima_pcacentred_normal_h1),
    resources = future_ram(3)
  ),
  tar_target(mse_proj_var_series, get_mse_proj_series(se_proj_var),
    resources = future_ram(3)
  ),
  tar_target(mse_proj_var_pcacentred_normal_series, get_mse_proj_series(se_proj_var_pcacentred_normal),
    resources = future_ram(3)
  ),
  tar_target(mse_proj_arima_wls_series, get_mse_proj_series(se_proj_arima_wls),
    resources = future_ram(3)
  ),
  tar_target(mse_proj_dfm_series, get_mse_proj_series(se_proj_dfm),
    resources = future_ram(3)
  ),
  tar_target(mse_proj_dfm_pcacentred_normal_series, get_mse_proj_series(se_proj_dfm_pcacentred_normal),
    resources = future_ram(3)
  ),
  tar_target(mse_proj_dfm_pcacentred_normal_series_h1, get_mse_proj_series(se_proj_dfm_pcacentred_normal_h1),
    resources = future_ram(3)
  ),
  tar_target(mse_proj_true_series, get_mse_proj_series(se_proj_true),
    resources = future_ram(3)
  ),
  tar_target(mse_proj_true_pcacentred_normal_series, get_mse_proj_series(se_proj_true_pcacentred_normal),
    resources = future_ram(3)
  ),
  tar_target(mse_proj_arima_normal_series, get_mse_proj_series(se_proj_arima_normal),
    resources = future_ram(3)
  ),
  tar_target(mse_proj_arima_normal_series_h1, get_mse_proj_series(se_proj_arima_normal_h1),
    resources = future_ram(3)
  ),
  tar_target(mse_proj_arima_uniform_series, get_mse_proj_series(se_proj_arima_uniform),
    resources = future_ram(3)
  ),
  tar_target(mse_proj_arima_pca_uniform_series, get_mse_proj_series(se_proj_arima_pca_uniform),
    resources = future_ram(3)
  ),
  tar_target(mse_proj_arima_pcacentred_uniform_series, get_mse_proj_series(se_proj_arima_pcacentred_uniform),
    resources = future_ram(3)
  ),
  tar_target(mse_proj_arima_ortho_normal_series, get_mse_proj_series(se_proj_arima_ortho_normal),
    resources = future_ram(3)
  ),
  tar_target(mse_proj_arima_pca_normal_switch_sd_series, get_mse_proj_series(se_proj_arima_pca_normal_switch_sd),
    resources = future_ram(5)
  ),
  tar_target(mse_proj_arima_pca_normal_switch_rms_series, get_mse_proj_series(se_proj_arima_pca_normal_switch_rms),
    resources = future_ram(5)
  ),
  tar_target(mse_proj_arima_pcacentred_normal_switch_sd_series, get_mse_proj_series(se_proj_arima_pcacentred_normal_switch_sd),
    resources = future_ram(5)
  ),
  # by cv
  tar_target(mse_true_cv, get_mse_cv(se_true)),
  tar_target(mse_arima_cv, get_mse_cv(se_arima)),
  tar_target(mse_var_cv, get_mse_cv(se_var)),
  tar_target(mse_dfm_cv, get_mse_cv(se_dfm)),
  tar_target(mse_proj_iter_arima_cv, get_mse_proj_cv(se_proj_iter_arima),
    resources = future_ram(3)
  ),
  tar_target(mse_proj_arima_cv, get_mse_proj_cv(se_proj_arima),
    resources = future_ram(3)
  ),
  tar_target(mse_proj_var_cv, get_mse_proj_cv(se_proj_var),
    resources = future_ram(3)
  ),
  tar_target(mse_proj_arima_wls_cv, get_mse_proj_cv(se_proj_arima_wls),
    resources = future_ram(3)
  ),
  tar_target(mse_proj_dfm_cv, get_mse_proj_cv(se_proj_dfm),
    resources = future_ram(3)
  ),
  tar_target(mse_proj_true_cv, get_mse_proj_cv(se_proj_true),
    resources = future_ram(3)
  ),
  tar_target(mse_proj_arima_normal_cv, get_mse_proj_cv(se_proj_arima_normal),
    resources = future_ram(3)
  ),
  tar_target(mse_proj_arima_uniform_cv, get_mse_proj_cv(se_proj_arima_uniform),
    resources = future_ram(3)
  ),
  tar_target(mse_proj_arima_pca_uniform_cv, get_mse_proj_cv(se_proj_arima_pca_uniform),
    resources = future_ram(3)
  ),
  tar_target(mse_proj_arima_ortho_normal_cv, get_mse_proj_cv(se_proj_arima_ortho_normal),
    resources = future_ram(3)
  ),
  tar_target(mse,
    bind_rows(
      tibble(mse_true, mse_arima, mse_dfm, mse_var) %>%
        mutate(h = row_number(), p = 0, proj = FALSE) %>%
        pivot_longer(!c(h, p, proj), names_to = "model") %>%
        mutate(model = gsub("mse_", "", model, fixed = TRUE), Phi = NA),
      get_df_mse_proj(mse_proj_arima) %>%
        mutate(model = "arima", proj = TRUE, Phi = "PCA_normal"),
      get_df_mse_proj(mse_proj_arima_pcacentred_normal) %>%
        mutate(model = "arima", proj = TRUE, Phi = "PCAcentred_normal"),
      get_df_mse_proj(mse_proj_arima_pcacentred_normal_h1) %>%
        mutate(model = "arima_h1", proj = TRUE, Phi = "PCAcentred_normal"),
      get_df_mse_proj(mse_proj_dfm) %>%
        mutate(model = "dfm", proj = TRUE, Phi = "PCA_normal"),
      get_df_mse_proj(mse_proj_dfm_pcacentred_normal) %>%
        mutate(model = "dfm", proj = TRUE, Phi = "PCAcentred_normal"),
      get_df_mse_proj(mse_proj_dfm_pcacentred_normal_h1) %>%
        mutate(model = "dfm_h1", proj = TRUE, Phi = "PCAcentred_normal"),
      get_df_mse_proj(mse_proj_true) %>%
        mutate(model = "true", proj = TRUE, Phi = "PCA_normal"),
      get_df_mse_proj(mse_proj_true_pcacentred_normal) %>%
        mutate(model = "true", proj = TRUE, Phi = "PCAcentred_normal"),
      get_df_mse_proj(mse_proj_var) %>%
        mutate(model = "var", proj = TRUE, Phi = "PCA_normal"),
      get_df_mse_proj(mse_proj_var_pcacentred_normal) %>%
        mutate(model = "var", proj = TRUE, Phi = "PCAcentred_normal"),
      get_df_mse_proj(mse_proj_arima_wls) %>%
        mutate(model = "arima_wls", proj = TRUE, Phi = "PCA_normal"),
      get_df_mse_proj(mse_proj_arima_normal) %>%
        mutate(model = "arima", proj = TRUE, Phi = "normal"),
      get_df_mse_proj(mse_proj_arima_normal_h1) %>%
        mutate(model = "arima_h1", proj = TRUE, Phi = "normal"),
      get_df_mse_proj(mse_proj_arima_uniform) %>%
        mutate(model = "arima", proj = TRUE, Phi = "uniform"),
      get_df_mse_proj(mse_proj_arima_pca_uniform) %>%
        mutate(model = "arima", proj = TRUE, Phi = "PCA_uniform"),
      get_df_mse_proj(mse_proj_arima_pcacentred_uniform) %>%
        mutate(model = "arima", proj = TRUE, Phi = "PCAcentred_uniform"),
      get_df_mse_proj(mse_proj_arima_ortho_normal) %>%
        mutate(model = "arima", proj = TRUE, Phi = "ortho_normal"),
      get_df_mse_proj(mse_proj_iter_arima) %>%
        mutate(model = "arima_iter", proj = TRUE, Phi = "PCA_normal"),
      get_df_mse_proj(mse_proj_arima_pca_normal_switch_sd) %>%
        mutate(model = "arima", proj = TRUE, Phi = "PCA_normal_switch_sd"),
      get_df_mse_proj(mse_proj_arima_pca_normal_switch_rms) %>%
        mutate(model = "arima", proj = TRUE, Phi = "PCA_normal_switch_rms"),
      get_df_mse_proj(mse_proj_arima_pcacentred_normal_switch_sd) %>%
        mutate(model = "arima", proj = TRUE, Phi = "PCAcentred_normal_switch_sd"),
    ),
    resources = future_ram(6)
  ),
  tar_target(
    plot_mse,
    ggplot(mse, aes(
      x = p, y = value,
      colour = model,
      linetype = paste(proj, Phi, sep = ".")
    )) +
      geom_line() +
      geom_hline(
        data = \(df) filter(df, !proj),
        aes(
          yintercept = value,
          colour = model,
          linetype = paste(proj, Phi, sep = ".")
        )
      ) +
      facet_wrap("h", scales = "free", labeller = label_both) +
      ylab("MSE") +
      scale_linetype_discrete(
        name = "Constraint",
        labels = c(
          "TRUE.PCA_normal" = "PCA+Norm.",
          "TRUE.PCAcentred_normal" = "PCAcentred+Norm.",
          "FALSE.NA" = "No Proj.",
          "TRUE.normal" = "Norm.",
          "TRUE.uniform" = "Unif.",
          "TRUE.PCA_uniform" = "PCA+Unif.",
          "TRUE.PCAcentred_uniform" = "PCAcentred+Unif.",
          "TRUE.ortho_normal" = "Ortho.+Norm.",
          "PCA_normal_switch_sd" = "PCA->Norm by sd",
          "PCA_normal_switch_rms" = "PCA->Norm by rms",
          "PCAcentred_normal_switch_rms" = "PCAcentred->Norm by sd"
        )
      ) +
      geom_vline(xintercept = m)
  )
)
