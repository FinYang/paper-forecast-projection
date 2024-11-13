library(tidyverse)
library(targets)
tar_load(fred_raw)
tar_load(fred)

naniar::miss_var_summary(fred_raw) %>%
  print(n = Inf)

fred_raw %>%
  as_tibble() %>%
  pivot_longer(-date) %>%
  mutate(
    missing = is.na(value),
    date = yearmonth(date)
  ) %>%
  select(-value) %>%
  right_join(pivot_longer(fred, -date)) %>%
  group_by(name) %>%
  filter(any(missing)) %>%
  mutate(value = scale(value)) %>%
  as_tsibble(index = date, key = name) %>%
  filter(name == "S&P div yield") %>%
  autoplot() +
  geom_point(
    data = \(x) filter(as_tibble(x), missing),
    color = "pink", alpha = 0.7
  ) +
  guides(colour = "none")
