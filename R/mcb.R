as.data.frame.nemenyi <- function(x, ...) {
  as_tibble(x$means, rownames = "name") %>%
    mutate(
      cd = x$cd,
      l = value - cd / 2,
      u = value + cd / 2,
    ) %>%
    arrange(value) %>%
    # mutate(
    #   name = paste0(
    #     name,
    #     " - ",
    #     format(round(value, 2), width = 5, nsmall = 2))) %>%
    mutate(
      fpval = x$fpval,
      fH = x$fH
    )
}

plot_mcb <- function(object, base_model_pattern) {
  object %>%
    arrange(value) %>%
    ggplot() +
    geom_rect(
      aes(xmin = l, xmax = u, fill = col),
      ymin = -Inf, ymax = Inf, alpha = 0.2,
      data = function(x) {
        summarise(
          x,
          col = TRUE,
          l = l[[which.min(value)]],
          u = u[[which.min(value)]]
        )
      }
    ) +
    geom_segment(aes(x = l, xend = u, yend = name, y = name)) +
    geom_point(aes(x = l, y = name), pch = "|", size = 2) +
    geom_point(aes(x = u, y = name), pch = "|", size = 2) +
    geom_point(aes(
      x = value, fill = col, y = name,
      pch = grepl(base_model_pattern, name)
    ), size = 3) +
    scale_shape_manual(values = c(21, 24)) +
    geom_label(
      data = function(x) {
        mutate(x, text = paste0(
          "Friedman test p-value ",
          ifelse(fpval < 0.001,
            " < 0.001", round(fpval, 3)
          )
        ))
      },
      aes(x = Inf, y = -Inf, label = text),
      vjust = "inward", hjust = "inward", size = 2.5, label.size = NA
    ) +
    labs(y = NULL, x = NULL) +
    # theme_minimal()+
    # facet_grid("h", scales = "free", labeller = label_both) +
    theme(
      legend.position = "none",
      text = element_text(size = 10),
      strip.text = element_text(size = 9),
      legend.margin = margin()
    )
}
