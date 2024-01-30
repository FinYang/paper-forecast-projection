## ---- library ----
library(tidyverse)
library(tsibble)
cb_palette_grey <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
cb_palette_black <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

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
  scale_y_log10()  +
  theme(plot.background = element_rect(fill = "#FAFAFA", colour = "#FAFAFA"))
p_aus_mel

## ---- p_syd_mel ----
p_syd_mel <- visnights %>%
  filter(Region %in% c("Sydney", "Melbourne")) %>%
  ggplot() +
  geom_line(aes(x = Month, y = Nights, colour = Region)) +
  # facet_grid("Region", scales = "free") +
  scale_y_log10()  +
  theme(plot.background = element_rect(fill = "#FAFAFA", colour = "#FAFAFA"),
        legend.background = element_rect(fill = "#FAFAFA", colour = "#FAFAFA"),
        legend.position = "bottom") +
  scale_color_manual(
    values = cb_palette_grey[c(6, 7)]
  )
p_syd_mel

## ---- series ----

visnights %>%
  filter(Region %in% unique(Region)[seq_len(4)]) %>%
  ggplot() +
  geom_line(aes(x = Month, y = Nights)) +
  facet_grid("Region", scales = "free") +
  theme(plot.background = element_rect(fill = "#FAFAFA", colour = "#FAFAFA"))

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
  facet_grid("Component", scales = "free") +
  theme(plot.background = element_rect(fill = "#FAFAFA", colour = "#FAFAFA"))



