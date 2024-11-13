# mat <- sam_in <- tar_read(sam_in, branches = 1)
# start <- tar_read(fred_start)
# frequency <- tar_read(fred_freq)
# .h <- .forecast_h <- 12

arima_mat <- function(mat, .h, start, frequency) {
  fit_ls <- apply(
    mat, 2,
    \(x) forecast::auto.arima(ts(x, start = start, frequency = frequency),
      d = 0, D = 0
    ),
    simplify = FALSE
  )
  out <- lapply(
    fit_ls,
    \(fit) list(
      fc = forecast(fit, h = .h)$mean,
      res = residuals_fast(fit, h = 1)
    )
  )
  out
}

residuals_fast <- function(object, h = 1, ...) {
  if (h == 1) {
    return(object$residuals)
  }
  y <- object$fitted + residuals(object, "innovation")
  yx <- residuals(object, "regression")
  # Get fitted model
  mod <- object$model
  # Reset model to initial state
  mod <- stats::makeARIMA(mod$phi, mod$theta, mod$Delta)
  # Calculate regression component
  xm <- y - yx
  # mod_seq <- mod
  fits <- rep_len(NA_real_, length(y))

  start <- length(mod$Delta) + 1
  end <- length(yx) - h
  idx <- if (start > end) integer(0L) else start:end
  for (i in idx) {
    fc_mod <- attr(stats::KalmanRun(yx[seq_len(i)], mod, update = TRUE), "mod")
    fits[i + h] <- stats::KalmanForecast(h, fc_mod)$pred[h] + xm[i + h]
  }
  tsp(fits) <- tsp(object$x)
  getResponse(object) - fits
}
