ets_mat <- function(mat, .h, start, frequency) {
  fit_ls <- apply(
    mat, 2,
    \(x) forecast::ets(ts(x, start = start, frequency = frequency)),
    simplify = FALSE
  )
  out <- lapply(
    fit_ls,
    \(fit) list(
      fc = forecast(fit, h = .h)$mean,
      res = residuals(fit, type = "response")
    )
  )
  out
}

ets_mat2 <- function(mat, .h, start, frequency) {
  fit_ls <- apply(
    mat, 2,
    \(x) forecast::ets(ts(x, start = start, frequency = frequency)),
    simplify = FALSE
  )
  out <- lapply(
    fit_ls,
    \(fit) list(
      fc = forecast(fit, h = .h)$mean,
      res = lapply(seq_len(.h), \(.h) residuals(fit, h = .h, type = "response"))
    )
  )
  out
}
