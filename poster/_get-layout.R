# Adopted from https://github.com/baptiste/quarto-flowfram

# devtools::install_github("coolbutuseless/minisvg")
library(glue)
library(minisvg)
library(dplyr)
library(tidyr)
library(purrr)


rm_chr <- function(x) {
  x[!vapply(x, is.character, FUN.VALUE = logical(1L))]
}

svg_layout <- function(input = "_layout.svg") {
  doc <- minisvg::parse_svg_doc(input)
  children <- rm_chr(doc$children)
  svg_children <- rm_chr(children[[2]]$children)
  layout <- svg_children %>%
    rm_chr() %>%
    map_df(`$`, "attribs") |>
    select(x, y, width, height, id) |>
    drop_na() |>
    mutate(across(c(x, y, width, height), as.numeric))
  layout
}

tex_layout <- function(layout, output = "_layout.tex") {
  tpl_frame <- "
\\newstaticframe[1]{@width@mm}{@height@mm}{@x@mm}{@y@mm}[@id@]
"
  page_height <- layout[layout$id == "page", ]$height

  core <- layout |>
    filter(id != "page") |>
    mutate(y = page_height - height - y)

  .frames <- paste(glue_data(core, tpl_frame,
    .open = "@", .close = "@"
  ), collapse = "\n")

  cat(.frames, file = output)
}

layout <- svg_layout()
