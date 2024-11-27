## ---- library ----
library(tidyverse)
library(tsibble)
library(fable)
cb_palette_grey <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
cb_palette_black <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

theme_set(
  theme_bw(base_size = 24) +
    theme(
      plot.background = element_blank(),
      panel.background = element_rect(fill = "#faf8f6"),
      legend.background = element_blank()
    )
)
## ---- data ----
visnights <- read_csv("../data-raw/visnights_monthly.csv") %>%
  mutate(Month = yearmonth(Month)) %>%
  group_by(Month, Region) %>%
  summarise(Nights = sum(Nights), .groups = "drop")

## ---- p_hist ----
syd_mel <- visnights %>%
  filter(Region %in% c("Sydney", "Melbourne")) %>%
  mutate(scale = "Regions")
aus <- visnights %>%
  group_by(Month) %>%
  summarise(Nights = sum(Nights), .groups = "drop") %>%
  mutate(
    scale = "Australia",
    Region = "Australia"
  )
df_hist <- bind_rows(syd_mel, aus)
df_hist %>%
  ggplot() +
  geom_line(aes(x = Month, y = Nights, colour = Region)) +
  # facet_grid("Region", scales = "free") +
  scale_y_log10() +
  theme(legend.position = "bottom") +
  scale_color_manual(
    values = `names<-`(
      c(cb_palette_grey[c(6, 7)], "black"),
      c("Melbourne", "Sydney", "Australia")
    ),
    limits = c("Melbourne", "Sydney")
  ) +
  facet_grid("scale", scales = "free") +
  ggtitle("Total number of nights spent by Australians away from home") +
  theme(
    legend.position = "inside",
    legend.position.inside = c(.9, .1)
  )


## ---- series ----
regions <- c("Melbourne", "Canberra", "Fraser Coast", "Central Highlands")

visnights_hist <- visnights %>%
  filter(Region %in% regions)


# visnights_hist %>%
#   ggplot() +
#   geom_line(aes(x = Month, y = Nights)) +
#   facet_grid("Region", scales = "free") +
#   theme(
#     axis.text.y = element_blank(),
#     axis.ticks.y = element_blank()
#   )

source("../monarch/tourism/projection/R/component.R")
visnights_wide <- visnights %>%
  pivot_wider(names_from = Region, values_from = Nights)
col_month <- select(visnights_wide, Month)
components <- visnights_wide %>%
  select(-Month) %>%
  as.matrix() %>%
  component() %>%
  getElement("x") %>%
  bind_cols(col_month, .) %>%
  pivot_longer(-Month,
    names_to = "Component",
    values_to = "Value"
  )
components_hist <- components %>%
  filter(Component %in% unique(Component)[seq_len(4)])


hist_tsb <- bind_rows(
  rename(visnights_hist,
    Series = Region,
    Value = Nights
  ) %>%
    mutate(type = "Series"),
  rename(components_hist, Series = Component) %>%
    mutate(type = "Components")
) %>%
  mutate(Series = factor(Series, levels = c(
    "Canberra",
    "PC1",
    "Central Highlands",
    "PC2",
    "Fraser Coast",
    "PC3",
    "Melbourne",
    "PC4"
  ))) %>%
  as_tsibble(index = Month, key = c(Series, type))

mdl <- hist_tsb %>%
  filter(year(Month) < 2019) %>%
  model(ETS(Value))
fbl <- mdl %>%
  forecast(h = 12)
hist_tsb %>%
  filter(year(Month) >= 2015) %>%
  autoplot(fbl, ., level = NULL, color = cb_palette_grey[[7]]) +
  # facet_grid(Series ~ type, scales = "free_y")
  facet_wrap(vars(Series),
    scales = "free_y",
    nrow = 4, ncol = 2,
    strip.position = "right"
  ) +
  theme(
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    panel.spacing = unit(.1, "lines")
  ) +
  ggtitle(
    "History and Base Forecast",
    subtitle = "LEFT: Visitor nights; RIGHT: Principal Components"
  )


## ---- series-fc ----

proj_ets_pca_normal <- qs::qread("../monarch/tourism/projection/output/proj_ets_pca_normal.qs")[[1]]
fc_ets <- qs::qread("../monarch/tourism/projection/output/fc_ets.qs")[[1]]

n_comp <- length(proj_ets_pca_normal)


# regions <- unique(visnights$Region)

region_idx <- match(regions, visnights$Region)
last_periods <- visnights %>%
  distinct(Month) %>%
  slice_tail(n = 12) %>%
  pull(Month)

proj_fc <- proj_ets_pca_normal %>%
  lapply(\(x) x[, region_idx]) %>%
  do.call(rbind, .) %>%
  `colnames<-`(regions) %>%
  as_tibble() %>%
  mutate(
    Month = rep(last_periods, n_comp),
    p = rep(seq_len(n_comp), each = 12)
  )
fc <- fc_ets[, region_idx] %>%
  `colnames<-`(regions) %>%
  as_tibble() %>%
  mutate(
    Month = last_periods,
    p = 0
  )
fcs <- bind_rows(fc, proj_fc) %>%
  pivot_longer(all_of(regions), names_to = "Region", values_to = "Nights")

visnights %>%
  filter(Region %in% regions) %>%
  group_by(Region) %>%
  slice_tail(n = 36) %>%
  ungroup() %>%
  bind_rows(fcs) %>%
  filter(Region %in% c("Canberra", "Central Highlands")) %>%
  ggplot() +
  geom_line(aes(
    x = Month, y = Nights,
    colour = p, group = p
  )) +
  facet_grid("Region", scales = "free") +
  scale_colour_gradient(
    low = "#56B1F7",
    high = "#132B43",
    breaks = c(0, 100, 200),
    na.value = "black"
  ) +
  theme(
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    panel.spacing = unit(.1, "lines"),
    legend.position = "inside",
    legend.position.inside = c(.1, .8),
    legend.key.height = unit(4, "mm")
  ) +
  ggtitle("FLAP forecasts with number of components p")

## ---- application ----
m <- 77
p1 <- qs::qread("../monarch/tourism/projection/output/mse.qs") %>%
  filter(
    h %in% c(1, 12),
    model != "ets_h"
  ) %>%
  ggplot(aes(
    x = p, y = value,
    linetype = paste(proj, Phi, sep = ".")
  )) +
  geom_vline(xintercept = m) +
  geom_line() +
  geom_hline(
    data = \(df) filter(df, !proj),
    aes(
      yintercept = value,
      linetype = paste(proj, Phi, sep = ".")
    )
  ) +
  # facet_wrap("h", scales = "free", labeller = label_both) +
  facet_grid(rows = "h", scales = "free", labeller = label_both) +
  scale_linetype_manual(
    name = "Component",
    values = c(
      "TRUE.PCA_normal" = "longdash",
      "FALSE.NA" = "solid",
      "TRUE.normal" = "dotdash"
    ),
    labels = c(
      "TRUE.PCA_normal" = "PCA+Norm.",
      "FALSE.NA" = "No Proj.",
      "TRUE.normal" = "Norm."
    )
  ) +
  theme(
    # axis.text.y = element_blank(),
    # axis.ticks.y = element_blank(),
    axis.title.x = element_blank(),
    panel.spacing = unit(.1, "lines"),
    legend.position = "none",
    legend.position.inside = c(.8, .8)
  ) +
  labs(
    y = "MSE",
    subtitle = "FLAP visitor nights ETS",
    x = "Number of components p"
  )

m <- 122
p2 <- qs::qread("../monarch/fred-md/projection/output/mse.qs") %>%
  filter(
    !model %in% c("ets", "arima"),
    !grepl("ets", Phi),
    h %in% c(1, 12)
  ) %>%
  ggplot(aes(
    x = p, y = value,
    linetype = paste(proj, Phi, sep = ".")
  )) +
  geom_vline(xintercept = m) +
  geom_line() +
  geom_hline(
    data = \(df) filter(df, !proj),
    aes(
      yintercept = value,
      linetype = paste(proj, Phi, sep = ".")
    )
  ) +
  # facet_wrap("h", scales = "free", labeller = label_both) +
  facet_grid(rows = "h", scales = "free", labeller = label_both) +
  scale_linetype_manual(
    name = "Component",
    values = c(
      "TRUE.PCA_normal" = "longdash",
      "FALSE.NA" = "solid",
      "TRUE.normal" = "dotdash"
    ),
    labels = c(
      "TRUE.PCA_normal" = "PCA+Norm.",
      "FALSE.NA" = "No Proj.",
      "TRUE.normal" = "Norm."
    )
  ) +
  theme(
    # axis.text.y = element_blank(),
    # axis.ticks.y = element_blank(),
    axis.title.y = element_blank(),
    # axis.title.x = element_blank(),
    panel.spacing = unit(.1, "lines"),
    legend.position = "none",
    legend.position.inside = c(.8, .8)
  ) +
  labs(
    subtitle = "FLAP FRED-MD DFM",
    x = "Number of components p"
  ) +
  guides(linetype = "none")


m <- 70

pa_simulation <- function(...) {
  file.path("../monarch/simulation/projection/output/", ...)
}
mse <- qs::qread(pa_simulation("mse.qs"))

p3 <- mse %>%
  filter(
    model %in% c("arima", "dfm", "var", "true"),
    Phi %in% c("PCA_normal") | is.na(Phi),
    h %in% c(1, 6)
  ) %>%
  # {print(distinct(., model, Phi))}
  ggplot(aes(
    x = p, y = value,
    colour = model,
    linetype = paste(proj, Phi, sep = ".")
  )) +
  geom_vline(xintercept = m) +
  geom_line() +
  geom_hline(
    data = \(df) filter(df, !proj),
    aes(
      yintercept = value,
      colour = model,
      linetype = paste(proj, Phi, sep = ".")
    )
  ) +
  # facet_wrap("h", scales = "free", labeller = label_both) +
  facet_grid(rows = "h", scales = "free", labeller = label_both) +
  ylab("MSE") +
  scale_linetype_manual(
    name = "Component",
    values = c(
      "TRUE.PCA_normal" = "longdash",
      "FALSE.NA" = "solid",
      "TRUE.normal" = "dotdash"
    ),
    labels = c(
      "TRUE.PCA_normal" = "PCA+Norm.",
      "FALSE.NA" = "No Proj.",
      "TRUE.normal" = "Norm."
    )
  ) +
  scale_color_manual(
    name = "Model",
    values = cb_palette_grey[c(7, 6, 4, 2)],
    labels = c(
      "arima" = "ARIMA",
      "dfm" = "DFM",
      "true" = "VAR - DGP",
      "var" = "VAR - Est."
    )
  ) +
  theme(
    # axis.text.y = element_blank(),
    # axis.ticks.y = element_blank(),
    axis.title.y = element_blank(),
    # axis.title.x = element_blank(),
    panel.spacing = unit(.1, "lines"),
    legend.position = "inside",
    legend.position.inside = c(.1, .6)
  ) +
  labs(
    subtitle = "VAR Simulation",
    x = "Number of components p"
  ) +
  guides(linetype = "none")
if (FALSE) {
  ggpubr::ggarrange(p1, p2, p3,
    nrow = 1,
    common.legend = TRUE,
    legend = "bottom"
  )
  # gridExtra::grid.arrange(
  egg::ggarrange(
    p1, p2, p3,
    nrow = 1,
    bottom = grid::textGrob("Number of components p",
      gp = grid::gpar(fontsize = 24)
    )
  )
}
library(patchwork)
p1 + p2 + p3 +
  plot_layout(
    guides = "collect",
    axis_titles = "collect"
  ) &
  theme(legend.position = "right")
