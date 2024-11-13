# model_true <- tar_read(model_true)
# B_true <- tar_read(B_true)
# mat <- sim_in <- tar_read(sim_in, branches = 1)[[1]]
#
# .n_lag <- 3
# .forecast_h <- 12
# sam <- matrix(nrow = .n_lag + .forecast_h, ncol= ncol(sim_in))
# sam[seq_len(.n_lag), ] <- sim_in[seq(nrow(sim_in)-.n_lag+1, nrow(sim_in)),]
# fc <- matrix(nrow = .forecast_h, ncol= ncol(sim_in))
# for(i in seq_len(.forecast_h)) {
#   predict(model_true, )
#   fc[i,] <- sam[i+.n_lag, ] <- t(B_true[,-1] %*% c(t(sam[seq(i+.n_lag-1, i, by = -1),])) + B_true[,1])
# }
# fc
#
# predict(model_true, tail(sim_in, .n_lag), n.ahead = .forecast_h) %>%
#   unname() %>%
#   all.equal(fc)

var_est <- function(mat, .n_lag, .forecast_h) {
  obj <- lineVar(mat, lag = .n_lag)
  fc <- predict(obj, tail(mat, .n_lag), n.ahead = .forecast_h)
  # fitted(obj)
  # head(residuals(obj))
  resid <- vector("list", nrow(mat) - .n_lag)
  mat_na <- rbind(mat, matrix(NA, nrow = .forecast_h, ncol = NCOL(mat)))
  for (i in seq(1, nrow(mat) - .n_lag)) {
    resid[[i]] <- mat_na[seq(i + .n_lag, length.out = .forecast_h), ] -
      predict(obj, mat[seq(i, length.out = .n_lag), ], n.ahead = .forecast_h)
  }
  res <- resid %>%
    list2array() %>%
    aperm(c(3, 2, 1)) %>%
    array2list() %>%
    map2(
      seq_along(.),
      \(x, i) {
        nrow_na <- i - 1
        if (nrow_na == 0) {
          return(x)
        }
        rbind(
          matrix(nrow = nrow_na, ncol = ncol(x)),
          head(x, -nrow_na)
        )
      }
    ) %>%
    lapply(\(x) rbind(matrix(nrow = .n_lag, ncol = ncol(x)), x))
  # all.equal(res[[1]][-seq_len(.n_lag),], residuals(obj))
  list(fc = fc, res = res)
}
