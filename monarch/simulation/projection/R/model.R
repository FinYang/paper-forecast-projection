arima <- function(mat, .h) {
  fit_ls <- mat %>%
    apply(2, \(x) forecast::auto.arima(x), simplify = FALSE)
  out <- fit_ls %>%
    lapply(\(fit) list(
      fc = forecast(fit, h = .h)$mean,
      res = lapply(seq_len(.h), \(.h) residuals_fast(fit, type = "response", h = .h))
    ))
  out
}

arima2 <- function(mat, .h) {
  fit_ls <- mat %>%
    apply(2, \(x) forecast::auto.arima(x), simplify = FALSE)
  out <- fit_ls %>%
    lapply(\(fit) list(
      fc = forecast(fit, h = .h)$mean,
      fitted = lapply(seq_len(.h), \(.h) fitted_fast(fit, h = .h))
    ) %>%
      c(list(res = lapply(.$fitted, \(fits)getResponse(fit) - fits))))
  out
}



get_fc <- function(obj) {
  obj %>%
    lapply(getElement, "fc") %>%
    do.call(cbind, .) %>%
    unname()
}
get_res <- function(obj) {
  obj %>%
    lapply(getElement, "res") %>%
    lapply(\(x) do.call(cbind, x)) %>%
    list2array() %>%
    aperm(c(1, 3, 2)) %>%
    array2list()
}
get_fitted <- function(obj) {
  obj %>%
    lapply(getElement, "fitted") %>%
    lapply(\(x) do.call(cbind, x)) %>%
    list2array() %>%
    aperm(c(1, 3, 2)) %>%
    array2list()
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

fitted_fast <- function(object, h = 1, ...) {
  if (h == 1) {
    return(object$fitted)
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
  fits
}


# fit <- forecast::auto.arima(tar_read(sim_in, branches = 1)[[1]][,1])
# # identical(fitted(fit), fit$fitted)
# identical(residuals(fit), fit$residuals)
# bench::mark(residuals(fit, type = "response", h = 2),
#             residuals_fast(fit, h = 2))
# bench::mark(
#   # residuals(fit, type = "response", h = 1)
#   # getResponse(fit)
#   fitted(fit, h = 2)
# )
#
# bench::mark(fitted(fit) * NA,
#             numeric(length(fitted(fit))),
#             check = FALSE)
# nrow(sim_in)
# length(residuals(fit))
# length(residuals(fit, h = 1))
# fitted(fit, h = 2) %>% head()
# residuals(fit, h = 2) %>% head()
# head(getResponse(fit) - fitted(fit, h = 2))
# head(residuals(fit, type = "response", h = 2))
# class(fit)
#
# residuals_insample <- function(object, ...) UseMethod("residuals_insample")
# residuals_insample.Arima <- function(object, h = 1) {
#   predict(fit)
# }
#
# sloop::s3_dispatch(residuals(fit))
# object <- fit
# forecast:::hfitted(object, h =2, FUN = "Arima")
