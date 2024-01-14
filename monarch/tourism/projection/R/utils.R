get_fc <- function(obj) {
  obj %>%
    lapply(getElement, "fc") %>%
    do.call(cbind, .) %>%
    unname()
}
get_res <- function(obj) {
  obj %>%
    lapply(getElement, "res") %>%
    do.call(cbind, .) %>%
    unname()
}

list2array <- function(xlist){
  d1 <- unique(vapply(xlist, NROW, numeric(1)))
  if(length(d1) != 1) stop("Different row number")
  d2 <- unique(vapply(xlist, NCOL, numeric(1)))
  if(length(d2) != 1) stop("Different col number")
  xlist %>%
    unlist() %>%
    array(dim = (c(d1, d2, length(xlist))))
}

array2list <- function(xarray){
  as.list(plyr:::splitter_a(xarray,3, .id = NULL))
}

get_mse <- function(se) {
  se %>%
    list2array() %>%
    aperm(c(3, 2, 1)) %>%
    colMeans() %>%
    colMeans()
}

get_mse_series <- function(se) {
  se %>%
    list2array() %>%
    aperm(c(3, 2, 1)) %>%
    colMeans()
}
get_mse_cv <- function(se) {
  se %>%
    list2array() %>%
    aperm(c(2, 3, 1)) %>%
    colMeans()
}

get_mse_proj <- function(se_proj){
  lapply(seq_len(p), \(pp) lapply(se_proj, `[[`, pp)) %>%
    lapply(get_mse) %>%
    do.call(cbind, .)
}

get_mse_proj_series <- function(se_proj){
  lapply(seq_len(p), \(pp) lapply(se_proj, `[[`, pp)) %>%
    lapply(get_mse_series)
}
get_mse_proj_cv <- function(se_proj){
  lapply(seq_len(p), \(pp) lapply(se_proj, `[[`, pp)) %>%
    lapply(get_mse_cv)
}

get_df_mse_proj <- function(mse_proj){
  as_tibble(
    mse_proj,
    .name_repair = \(x) make.names(x, unique = TRUE)) %>%
    mutate(h =row_number()) %>%
    pivot_longer(-h, names_to = "p") %>%
    group_by(h) %>%
    mutate(p = row_number()) %>%
    ungroup()
}
