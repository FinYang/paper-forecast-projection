visnights_region <- read_csv("data-raw/visnights_monthly.csv") %>%
  group_by(Month, Region) %>%
  summarise(Nights = sum(Nights), .groups = "drop") %>%
  mutate(Month = yearmonth(Month)) %>%
  mutate(Region = make.names(Region)) %>%
  rename(key = Region)
visnights_mat <- visnights_region %>%
  pivot_wider(names_from = key, values_from = Nights) %>%
  arrange(Month) %>%
  {
    `rownames<-`(as.matrix(select(., -Month)), as.character(.$Month))
  }
qs::qsave(visnights_mat, "monarch/simulation/projection/data/visnights_mat.qs")
