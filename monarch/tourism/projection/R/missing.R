em_fill_df <- function(df) {
  date <- select(df, date)
  mat <- as.matrix(select(df, -date))
  filled_mat <- em_fill(mat)
  bind_cols(date, filled_mat)
}

# McCracken, Michael W., and Serena Ng. 2016. “FRED-MD: A Monthly Database for
# Macroeconomic Research.” Journal of Business & Economic Statistics: A
# Publication of the American Statistical Association 34 (4): 574–89. Stock,
# Section 3
#
# James H., and Mark W. Watson. 2002. “Macroeconomic Forecasting Using Diffusion
# Indexes.” Journal of Business & Economic Statistics: A Publication of the
# American Statistical Association 20 (2): 147–62.
# Appendix A A
em_fill <- function(mat, p = 8, maxit = 50) {
  err <- 999
  for (i in seq_len(maxit)) {
    vec_mean <- colMeans(mat, na.rm = TRUE)
    vec_sd <- apply(mat, 2, sd, na.rm = TRUE)
    mat <- sweep(mat, 2, vec_mean)
    mat <- sweep(mat, 2, vec_sd, FUN = "/")
    miss_mat <- is.na(mat)
    mat[miss_mat] <- 0
    pca <- prcomp(mat, center = FALSE, scale. = FALSE)
    # scale or not doesn't change prediction
    lambda <- pca$rotation[, seq_len(p)] # * sqrt(nrow(mat))
    f <- pca$x[, seq_len(p)]
    pred_mat <- tcrossprod(f, lambda)
    if (i > 1) {
      dif <- pred_mat - last_mat
      err <- crossprod(c(dif)) / crossprod(c(last_mat))
    }
    last_mat <- pred_mat

    mat[miss_mat] <- pred_mat[miss_mat]
    mat <- sweep(mat, 2, vec_sd, FUN = "*")
    mat <- sweep(mat, 2, vec_mean, FUN = "+")
    if (err < 1e-6) break
  }
  mat
}
