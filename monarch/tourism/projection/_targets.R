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
    "tsibble"
  ), # packages that your targets need to run
  format = "qs", # default storage format
  deployment = "worker",
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
.forecast_h <- 12
p <- 200

# Replace the target list below with your own:
list(
  tar_target(visnights_tsb,
    read_csv("data/visnights_monthly.csv") %>%
      group_by(Month, Region) %>%
      summarise(Nights = sum(Nights), .groups = "drop") %>%
      mutate(Month = yearmonth(Month)) %>%
      as_tsibble(index = Month, key = Region) %>%
      mutate(Region = make.names(Region)) %>%
      rename(key = Region, date = Month) %>%
      pivot_wider(names_from = key, values_from = Nights),
    deployment = "main"
  ),
  tar_target(n_init, ifelse(Sys.info()["sysname"] == "Linux",
    84, nrow(visnights_tsb) - .forecast_h
  ),
  deployment = "main"
  ),
  tar_target(visnights_mat, as.ts(visnights_tsb), deployment = "main"),
  tar_target(m, ncol(visnights_mat), deployment = "main"),
  tar_target(visnights_tsp, tsp(visnights_mat), deployment = "main"),
  tar_target(visnights_start, visnights_tsp[[1]], deployment = "main"),
  tar_target(visnights_freq, visnights_tsp[[3]], deployment = "main"),
  tar_target(sam_idx, seq(n_init, nrow(visnights_tsb) - .forecast_h),
    deployment = "main"
  ),
  tar_target(sam_in, visnights_mat[seq_len(sam_idx), ],
    iteration = "list", pattern = map(sam_idx),
    deployment = "main"
  ),
  tar_target(sam_out, visnights_mat[seq(sam_idx + 1, length.out = .forecast_h), ],
    iteration = "list", pattern = map(sam_idx),
    deployment = "main"
  ),
  #
  tar_target(out_ets,
    ets_mat(sam_in,
      .h = .forecast_h,
      start = visnights_start, frequency = visnights_freq
    ),
    iteration = "list",
    pattern = map(sam_in),
    resources = future_ram(2)
  ),
  tar_target(fc_ets, get_fc(out_ets),
    iteration = "list", pattern = map(out_ets),
    deployment = "main"
  ),
  tar_target(res_ets, get_res(out_ets),
    iteration = "list", pattern = map(out_ets),
    deployment = "main"
  ),
  tar_target(pca_normal, component(sam_in, "PCA", p = p),
    iteration = "list",
    pattern = map(sam_in),
    # cue = tar_cue(command = FALSE),
    deployment = "main"
  ),
  tar_target(out_ets_pca_normal,
    ets_mat(pca_normal$x,
      .h = .forecast_h,
      start = visnights_start, frequency = visnights_freq
    ),
    iteration = "list", pattern = map(pca_normal),
    resources = future_ram(3)
  ),
  tar_target(fc_ets_pca_normal, get_fc(out_ets_pca_normal),
    iteration = "list", pattern = map(out_ets_pca_normal),
    deployment = "main"
  ),
  tar_target(res_ets_pca_normal, get_res(out_ets_pca_normal),
    iteration = "list", pattern = map(out_ets_pca_normal),
    deployment = "main"
  ),
  tar_target(pcacentred_normal, component(sam_in, "PCAcentred_normal", p = p),
    iteration = "list",
    pattern = map(sam_in),
    # cue = tar_cue(command = FALSE),
    deployment = "main"
  ),
  tar_target(out_ets_pcacentred_normal,
    ets_mat(pcacentred_normal$x,
      .h = .forecast_h,
      start = visnights_start, frequency = visnights_freq
    ),
    iteration = "list", pattern = map(pcacentred_normal),
    resources = future_ram(3)
  ),
  tar_target(fc_ets_pcacentred_normal, get_fc(out_ets_pcacentred_normal),
    iteration = "list", pattern = map(out_ets_pcacentred_normal),
    deployment = "main"
  ),
  tar_target(res_ets_pcacentred_normal, get_res(out_ets_pcacentred_normal),
    iteration = "list", pattern = map(out_ets_pcacentred_normal),
    deployment = "main"
  ),
  tar_target(normal, component(sam_in, "normal", p = p),
    iteration = "list",
    pattern = map(sam_in),
    deployment = "main"
  ),
  tar_target(out_ets_normal,
    ets_mat(normal$x,
      .h = .forecast_h,
      start = visnights_start, frequency = visnights_freq
    ),
    iteration = "list", pattern = map(normal),
    resources = future_ram(3)
  ),
  tar_target(fc_ets_normal, get_fc(out_ets_normal),
    iteration = "list", pattern = map(out_ets_normal),
    deployment = "main"
  ),
  tar_target(res_ets_normal, get_res(out_ets_normal),
    iteration = "list", pattern = map(out_ets_normal),
    deployment = "main"
  ),
  tar_target(uniform, component(sam_in, "uniform", p = p),
    iteration = "list",
    pattern = map(sam_in),
    deployment = "main"
  ),
  tar_target(out_ets_uniform,
    ets_mat(uniform$x,
      .h = .forecast_h,
      start = visnights_start, frequency = visnights_freq
    ),
    iteration = "list", pattern = map(uniform),
    resources = future_ram(3)
  ),
  tar_target(fc_ets_uniform, get_fc(out_ets_uniform),
    iteration = "list", pattern = map(out_ets_uniform),
    deployment = "main"
  ),
  tar_target(res_ets_uniform, get_res(out_ets_uniform),
    iteration = "list", pattern = map(out_ets_uniform),
    deployment = "main"
  ),
  #
  # tar_target(out_dfm, {
  #   lapply(seq_len(.forecast_h), \(.h)
  #          auto_dfm(sam_in, n_factor = 8, f_lag = 3, y_lag = 6, h = .h))
  # }, iteration = "list", pattern = map(sam_in)),
  # tar_target(fc_dfm,
  #            do.call(rbind, lapply(out_dfm, getElement, "fc")),
  #            iteration = "list", pattern = map(out_dfm),
  #            deployment = "main"),
  # tar_target(res_dfm,
  #            lapply(out_dfm, getElement, "res"),
  #            iteration = "list", pattern = map(out_dfm),
  #            deployment = "main"),
  #

  tar_target(W_ets_pca_normal, get_W(res_ets, res_ets_pca_normal),
    iteration = "list",
    pattern = map(res_ets, res_ets_pca_normal),
    resources = future_ram(4)
  ),
  tar_target(W_ets_pcacentred_normal, get_W(res_ets, res_ets_pcacentred_normal),
    iteration = "list",
    pattern = map(res_ets, res_ets_pcacentred_normal),
    resources = future_ram(4)
  ),
  # tar_target(W_dfm_pca_normal, get_W(res_dfm[[1]], res_ets_pca_normal),
  #            iteration = "list",
  #            pattern = map(res_dfm, res_ets_pca_normal),
  #            resources = future_ram(4)),
  tar_target(W_ets_normal, get_W(res_ets, res_ets_normal),
    iteration = "list",
    pattern = map(res_ets, res_ets_normal),
    resources = future_ram(4)
  ),
  tar_target(W_ets_uniform, get_W(res_ets, res_ets_uniform),
    iteration = "list",
    pattern = map(res_ets, res_ets_uniform),
    resources = future_ram(4)
  ),
  tar_target(proj_ets_pca_normal,
    project(
      cbind(fc_ets, fc_ets_pca_normal),
      W = W_ets_pca_normal,
      Phi = pca_normal$Phi
    ),
    iteration = "list",
    pattern = map(fc_ets, fc_ets_pca_normal, W_ets_pca_normal, pca_normal),
    resources = future_ram(4)
  ),
  tar_target(proj_ets_pcacentred_normal,
    project(
      cbind(fc_ets, fc_ets_pcacentred_normal),
      W = W_ets_pcacentred_normal,
      Phi = pcacentred_normal$Phi
    ),
    iteration = "list",
    pattern = map(fc_ets, fc_ets_pcacentred_normal, W_ets_pcacentred_normal, pcacentred_normal),
    resources = future_ram(4)
  ),
  # tar_target(proj_dfm_pca_normal,
  #            project(
  #              cbind(fc_dfm, fc_ets_pca_normal),
  #              W = W_dfm_pca_normal,
  #              Phi = pca_normal$Phi),
  #            iteration = "list",
  #            pattern = map(fc_dfm, fc_ets_pca_normal, W_dfm_pca_normal, pca_normal),
  #            resources = future_ram(4)),
  tar_target(proj_ets_normal,
    project(
      cbind(fc_ets, fc_ets_normal),
      W = W_ets_normal,
      Phi = normal$Phi
    ),
    iteration = "list",
    pattern = map(fc_ets, fc_ets_normal, W_ets_normal, normal),
    resources = future_ram(4)
  ),
  tar_target(proj_ets_uniform,
    project(
      cbind(fc_ets, fc_ets_uniform),
      W = W_ets_uniform,
      Phi = uniform$Phi
    ),
    iteration = "list",
    pattern = map(fc_ets, fc_ets_uniform, W_ets_uniform, uniform),
    resources = future_ram(4)
  ),
  tar_target(pca_normal_switch_from_sd,
    which(apply(pca_normal$x, 2, sd) < mean(apply(normal$x[, 1:100], 2, sd)))[[1]],
    pattern = map(pca_normal, normal),
    deployment = "main"
  ),
  tar_target(proj_ets_pca_normal_switch_sd,
    project_switch(
      fc = fc_ets,
      fc_comp1 = fc_ets_pca_normal,
      fc_comp2 = fc_ets_normal,
      Phi1 = pca_normal$Phi,
      Phi2 = normal$Phi,
      res = res_ets,
      res_comp1 = res_ets_pca_normal,
      res_comp2 = res_ets_normal,
      switch_from = pca_normal_switch_from_sd
    ),
    pattern = map(
      fc_ets, fc_ets_pca_normal, fc_ets_normal,
      pca_normal, normal,
      res_ets, res_ets_pca_normal, res_ets_normal,
      pca_normal_switch_from_sd
    ),
    resources = future_ram(3)
  ),
  tar_target(pca_normal_switch_from_rms,
    which(sqrt(colMeans(pca_normal$x^2)) < mean(sqrt(colMeans(normal$x[, 1:100]^2))))[[1]],
    pattern = map(pca_normal, normal),
    deployment = "main"
  ),
  tar_target(proj_ets_pca_normal_switch_rms,
    project_switch(
      fc = fc_ets,
      fc_comp1 = fc_ets_pca_normal,
      fc_comp2 = fc_ets_normal,
      Phi1 = pca_normal$Phi,
      Phi2 = normal$Phi,
      res = res_ets,
      res_comp1 = res_ets_pca_normal,
      res_comp2 = res_ets_normal,
      switch_from = pca_normal_switch_from_rms
    ),
    pattern = map(
      fc_ets, fc_ets_pca_normal, fc_ets_normal,
      pca_normal, normal,
      res_ets, res_ets_pca_normal, res_ets_normal,
      pca_normal_switch_from_rms
    ),
    resources = future_ram(3)
  ),
  tar_target(pcacentred_normal_switch_from_sd,
    which(apply(pcacentred_normal$x, 2, sd) < mean(apply(normal$x[, 1:100], 2, sd)))[[1]],
    pattern = map(pcacentred_normal, normal),
    deployment = "main"
  ),
  tar_target(proj_ets_pcacentred_normal_switch_sd,
    project_switch(
      fc = fc_ets,
      fc_comp1 = fc_ets_pcacentred_normal,
      fc_comp2 = fc_ets_normal,
      Phi1 = pcacentred_normal$Phi,
      Phi2 = normal$Phi,
      res = res_ets,
      res_comp1 = res_ets_pcacentred_normal,
      res_comp2 = res_ets_normal,
      switch_from = pcacentred_normal_switch_from_sd
    ),
    pattern = map(
      fc_ets, fc_ets_pcacentred_normal, fc_ets_normal,
      pcacentred_normal, normal,
      res_ets, res_ets_pcacentred_normal, res_ets_normal,
      pcacentred_normal_switch_from_sd
    ),
    resources = future_ram(3)
  ),
  tar_target(se_ets, (sam_out - fc_ets)^2,
    iteration = "list",
    pattern = map(sam_out, fc_ets),
    deployment = "main"
  ),
  # tar_target(se_dfm, (sam_out - fc_dfm)^2,
  #            iteration = "list",
  #            pattern = map(sam_out, fc_dfm),
  #            deployment = "main"),
  tar_target(se_proj_ets_pca_normal, lapply(proj_ets_pca_normal, \(pa) (sam_out - pa)^2),
    iteration = "list",
    pattern = map(sam_out, proj_ets_pca_normal),
    deployment = "main"
  ),
  tar_target(se_proj_ets_pcacentred_normal, lapply(proj_ets_pcacentred_normal, \(pa) (sam_out - pa)^2),
    iteration = "list",
    pattern = map(sam_out, proj_ets_pcacentred_normal),
    deployment = "main"
  ),
  # tar_target(se_proj_dfm_pca_normal, lapply(proj_dfm_pca_normal, \(pa) (sam_out - pa)^2),
  #            iteration = "list",
  #            pattern = map(sam_out, proj_dfm_pca_normal),
  #            deployment = "main"),
  tar_target(se_proj_ets_normal, lapply(proj_ets_normal, \(pa) (sam_out - pa)^2),
    iteration = "list",
    pattern = map(sam_out, proj_ets_normal),
    deployment = "main"
  ),
  tar_target(se_proj_ets_uniform, lapply(proj_ets_uniform, \(pa) (sam_out - pa)^2),
    iteration = "list",
    pattern = map(sam_out, proj_ets_uniform),
    deployment = "main"
  ),
  tar_target(se_proj_ets_pca_normal_switch_sd, lapply(proj_ets_pca_normal_switch_sd, \(pa) (sam_out - pa)^2),
    iteration = "list",
    pattern = map(sam_out, proj_ets_pca_normal_switch_sd),
    deployment = "main"
  ),
  tar_target(se_proj_ets_pca_normal_switch_rms, lapply(proj_ets_pca_normal_switch_rms, \(pa) (sam_out - pa)^2),
    iteration = "list",
    pattern = map(sam_out, proj_ets_pca_normal_switch_rms),
    deployment = "main"
  ),
  tar_target(se_proj_ets_pcacentred_normal_switch_sd, lapply(proj_ets_pcacentred_normal_switch_sd, \(pa) (sam_out - pa)^2),
    iteration = "list",
    pattern = map(sam_out, proj_ets_pcacentred_normal_switch_sd),
    deployment = "main"
  ),
  tar_target(mse_ets, get_mse(se_ets)),
  # tar_target(mse_dfm, get_mse(se_dfm)),
  tar_target(mse_proj_ets_pca_normal, get_mse_proj(se_proj_ets_pca_normal),
    resources = future_ram(5)
  ),
  tar_target(mse_proj_ets_pcacentred_normal, get_mse_proj(se_proj_ets_pcacentred_normal),
    resources = future_ram(5)
  ),
  # tar_target(mse_proj_dfm_pca_normal, get_mse_proj(se_proj_dfm_pca_normal),
  #            resources = future_ram(5)),
  tar_target(mse_proj_ets_normal, get_mse_proj(se_proj_ets_normal),
    resources = future_ram(5)
  ),
  tar_target(mse_proj_ets_uniform, get_mse_proj(se_proj_ets_uniform),
    resources = future_ram(5)
  ),
  tar_target(mse_proj_ets_pca_normal_switch_sd, get_mse_proj(se_proj_ets_pca_normal_switch_sd),
    resources = future_ram(5)
  ),
  tar_target(mse_proj_ets_pca_normal_switch_rms, get_mse_proj(se_proj_ets_pca_normal_switch_rms),
    resources = future_ram(5)
  ),
  tar_target(mse_proj_ets_pcacentred_normal_switch_sd, get_mse_proj(se_proj_ets_pcacentred_normal_switch_sd),
    resources = future_ram(5)
  ),
  tar_target(mse_ets_series, get_mse_series(se_ets)),
  tar_target(mse_proj_ets_pca_normal_series, get_mse_proj_series(se_proj_ets_pca_normal),
    resources = future_ram(5)
  ),
  tar_target(mse_proj_ets_pcacentred_normal_series, get_mse_proj_series(se_proj_ets_pcacentred_normal),
    resources = future_ram(5)
  ),
  tar_target(mse_proj_ets_normal_series, get_mse_proj_series(se_proj_ets_normal),
    resources = future_ram(5)
  ),
  tar_target(mse_proj_ets_uniform_series, get_mse_proj_series(se_proj_ets_uniform),
    resources = future_ram(5)
  ),
  tar_target(mse_proj_ets_pca_normal_switch_sd_series, get_mse_proj_series(se_proj_ets_pca_normal_switch_sd),
    resources = future_ram(5)
  ),
  tar_target(mse_proj_ets_pca_normal_switch_rms_series, get_mse_proj_series(se_proj_ets_pca_normal_switch_rms),
    resources = future_ram(5)
  ),
  tar_target(mse_proj_ets_pcacentred_normal_switch_sd_series, get_mse_proj_series(se_proj_ets_pcacentred_normal_switch_sd),
    resources = future_ram(5)
  ),
  tar_target(mse_ets_cv, get_mse_cv(se_ets)),
  tar_target(mse_proj_ets_pca_normal_cv, get_mse_proj_cv(se_proj_ets_pca_normal),
    resources = future_ram(5)
  ),
  tar_target(mse_proj_ets_normal_cv, get_mse_proj_cv(se_proj_ets_normal),
    resources = future_ram(5)
  ),
  tar_target(mse_proj_ets_uniform_cv, get_mse_proj_cv(se_proj_ets_uniform),
    resources = future_ram(5)
  ),
  tar_target(mse,
    bind_rows(
      tibble(mse_ets) %>%
        mutate(
          h = row_number(),
          p = 0,
          proj = FALSE
        ) %>%
        pivot_longer(!c(h, p, proj), names_to = "model") %>%
        mutate(
          model = gsub("mse_", "", model, fixed = TRUE),
          Phi = "NA"
        ),
      get_df_mse_proj(mse_proj_ets_pca_normal) %>%
        mutate(model = "ets", proj = TRUE, Phi = "PCA_normal"),
      get_df_mse_proj(mse_proj_ets_pcacentred_normal) %>%
        mutate(model = "ets", proj = TRUE, Phi = "PCAcentred_normal"),
      # get_df_mse_proj(mse_proj_dfm_pca_normal) %>%
      #   mutate(model = "dfm", proj = TRUE, Phi = "PCA_normal"),
      get_df_mse_proj(mse_proj_ets_normal) %>%
        mutate(model = "ets", proj = TRUE, Phi = "normal"),
      get_df_mse_proj(mse_proj_ets_uniform) %>%
        mutate(model = "ets", proj = TRUE, Phi = "uniform"),
      get_df_mse_proj(mse_proj_ets_pca_normal_switch_sd) %>%
        mutate(model = "ets", proj = TRUE, Phi = "PCA_normal_switch_sd"),
      get_df_mse_proj(mse_proj_ets_pca_normal_switch_rms) %>%
        mutate(model = "ets", proj = TRUE, Phi = "PCA_normal_switch_rms"),
      get_df_mse_proj(mse_proj_ets_pcacentred_normal_switch_sd) %>%
        mutate(model = "ets", proj = TRUE, Phi = "PCAcentred_normal_switch_sd"),
    ),
    resources = future_ram(5)
  ),
  tar_target(
    plot_mse,
    ggplot(mse, aes(
      x = p, y = value,
      colour = paste(proj, Phi, sep = "."),
      linetype = paste(proj, Phi, sep = ".")
    )) +
      geom_line() +
      geom_hline(
        data = \(df) filter(df, !proj),
        aes(
          yintercept = value,
          colour = paste(proj, Phi, sep = "."),
          linetype = paste(proj, Phi, sep = ".")
        )
      ) +
      facet_wrap("h", scales = "free", labeller = label_both) +
      ylab("MSE") +
      scale_colour_discrete(
        name = "Constraint",
        labels = c(
          "TRUE.PCA_normal" = "PCA+Norm.",
          "TRUE.PCAcentred_normal" = "PCAcentred+Norm.",
          "TRUE.PCA_normal_switch_sd" = "PCA->Norm. by sd",
          "TRUE.PCA_normal_switch_rms" = "PCA->Norm. by rms",
          "TRUE.PCAcentred_normal_switch_sd" = "PCAcentred->Norm. by sd",
          "FALSE.NA" = "No Proj.",
          "TRUE.normal" = "Norm.",
          "TRUE.uniform" = "Unif."
        )
      ) +
      scale_linetype_discrete(
        name = "Constraint",
        labels = c(
          "TRUE.PCA_normal" = "PCA+Norm.",
          "TRUE.PCAcentred_normal" = "PCAcentred+Norm.",
          "TRUE.PCA_normal_switch_sd" = "PCA->Norm. by sd",
          "TRUE.PCA_normal_switch_rms" = "PCA->Norm. by rms",
          "TRUE.PCAcentred_normal_switch_sd" = "PCAcentred->Norm. by sd",
          "FALSE.NA" = "No Proj.",
          "TRUE.normal" = "Norm.",
          "TRUE.uniform" = "Unif."
        )
      ) +
      geom_vline(xintercept = m)
  )
)
