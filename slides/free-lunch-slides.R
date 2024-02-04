## ---- library ----
library(tidyverse)
library(tsibble)
cb_palette_grey <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
cb_palette_black <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
theme_set(theme(
  plot.background = element_blank(),
  legend.background = element_blank(),
  text = element_text(size = 21)))
## ---- data ----
visnights <- read_csv("../data-raw/visnights_monthly.csv") %>%
  mutate(Month = yearmonth(Month)) %>%
  group_by(Month, Region) %>%
  summarise(Nights = sum(Nights), .groups = "drop")

## ---- p_aus_mel ----
p_aus_mel <- bind_rows(
  visnights %>%
    group_by(Month) %>%
    summarise(Nights = sum(Nights), .groups = "drop") %>%
    mutate(Region = "Australia"),
  visnights %>%
    filter(Region == "Melbourne")
) %>%
  ggplot() +
  geom_line(aes(x = Month, y = Nights)) +
  facet_grid("Region", scales = "free") +
  scale_y_log10()
p_aus_mel

## ---- p_syd_mel ----
p_syd_mel <- visnights %>%
  filter(Region %in% c("Sydney", "Melbourne")) %>%
  ggplot() +
  geom_line(aes(x = Month, y = Nights, colour = Region)) +
  # facet_grid("Region", scales = "free") +
  scale_y_log10()  +
  theme(legend.position = "bottom") +
  scale_color_manual(
    values = cb_palette_grey[c(6, 7)]
  )
p_syd_mel

## ---- series ----
regions <- c("Melbourne", "Canberra", "Fraser Coast", "Central Highlands")

visnights %>%
  filter(Region %in% regions) %>%
  ggplot() +
  geom_line(aes(x = Month, y = Nights)) +
  facet_grid("Region", scales = "free")

## ---- components ----

source("../monarch/tourism/projection/R/component.R")
visnights_wide <- visnights %>%
  pivot_wider(names_from = Region, values_from = Nights)
col_month <- select(visnights_wide, Month)
visnights_wide %>%
  select(-Month) %>%
  as.matrix() %>%
  component() %>%
  getElement("x") %>%
  bind_cols(col_month, .) %>%
  pivot_longer(-Month, names_to = "Component") %>%
  filter(Component %in% unique(Component)[seq_len(4)]) %>%
  ggplot() +
  geom_line(aes(x = Month, y = value)) +
  facet_grid("Component", scales = "free")

## ---- series-fc ----

proj_ets_pca_normal <- qs::qread("../monarch/tourism/projection/output/proj_ets_pca_normal.qs")[[1]]
fc_ets <- qs::qread("../monarch/tourism/projection/output/fc_ets.qs")[[1]]

n_comp <- length(proj_ets_pca_normal)


# regions <- unique(visnights$Region)

region_idx <- match(regions, visnights$Region)
last_periods <- visnights %>%
  distinct(Month) %>%
  slice_tail(n=12) %>%
  pull(Month)

proj_fc <- proj_ets_pca_normal %>%
  lapply(\(x) x[,region_idx]) %>%
  do.call(rbind, .) %>%
  `colnames<-`(regions) %>%
  as_tibble() %>%
  mutate(Month = rep(last_periods, n_comp),
         p = rep(seq_len(n_comp), each = 12))
fc <- fc_ets[,region_idx] %>%
`colnames<-`(regions) %>%
  as_tibble() %>%
  mutate(Month = last_periods,
         p = 0)
fcs <- bind_rows(fc, proj_fc) %>%
  pivot_longer(all_of(regions), names_to = "Region", values_to = "Nights")

visnights %>%
  filter(Region %in% regions) %>%
  group_by(Region) %>%
  slice_tail(n = 36) %>%
  ungroup() %>%
  bind_rows(fcs) %>%
  ggplot() +
  geom_line(aes(x = Month, y = Nights,
                colour = p, group = p)) +
  facet_grid("Region", scales = "free") +
  scale_colour_gradient(low = "#56B1F7",
                        high = "#132B43")

## ---- visnights ----
m <- 77
qs::qread("../monarch/tourism/projection/output/mse.qs") %>%
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

## ---- fred-md ----
m <- 122
qs::qread("../monarch/fred-md/projection/output/mse.qs") %>%
  filter(
    !model %in% c("ets","arima"),
    !grepl("ets", Phi),
    h %in% c(1, 6, 12)) %>%
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

## ---- simulation ----

m <- 70

pa_simulation <- function(...)
  file.path("../monarch/simulation/projection/output/", ...)
mse <- bind_rows(
  qs::qread(pa_simulation("mse.qs")),
  qs::qread(pa_simulation("mse_exp.qs"))
)

mse %>%
  filter(model %in% c("arima", "dfm", "var", "true"),
         Phi %in% c("PCA_normal") | is.na(Phi),
         h %in% c(1, 6)) %>%
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

