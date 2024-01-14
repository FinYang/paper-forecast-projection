
#' Calculate BIC from output of lm.fit
#'
#' Assign equal weights on observations to speed up
BIC_lmfit <- function(object) {
  nos <- length(object$residuals)
  # if (is.null(w <- object$weights)) {
  #   w <- rep.int(1, nos)
  # }
  # lls <- 0.5 * (sum(log(w)) - nos * (log(2 * pi) + 1 - log(nos) +
  #                                    log(sum(w * object$residuals^2))))
  lls <- 0.5 * (- nos * (log(2 * pi) + 1 - log(nos) + log(sum(object$residuals^2))))
  -2 * as.numeric(lls) + log(nos) * (object$rank + 1)
}


predict_dfm <- function(sim_in, factor, f_lag = 1, y_lag = 3, h = 1) {
  y_lagh <- y_lag + h - 1
  f_lagh <- f_lag + h - 1
  n <- NROW(sim_in)
  X_list <- lapply(seq(h, y_lagh), \(l) dplyr::lag(sim_in, n=l)) %>%
    list2array() %>%
    aperm(c(1, 3, 2)) %>%
    array2list()
  f <- lapply(seq(h, f_lagh), \(l) dplyr::lag(factor, n=l)) %>%
    do.call(cbind, .)

  fc <- map2(asplit(sim_in, 2), X_list,
             \(y, X, f) {
               XX <- cbind(1, f, X)
               # browser()
               fit <- lm.fit(x = XX[-seq_len(y_lagh),],
                             y = y[-seq_len(y_lagh)])
               # all.equal(as.numeric(fit$fitted.values), fitted[-c(1:4)])
               # all.equal(fit$residuals, (y - fitted)[-c(1:4)])
               # bench::mark(fit$residuals, (y - fitted)[-c(1:4)])
               # bench::mark(
               #   lm.fit(x = XX[-seq_len(y_lagh),], y = y[-seq_len(y_lagh)]),
               #   lm(y~ ., data = as.data.frame(cbind(y, XX[,-1]))),
               #   check = FALSE
               # )[,-1]

               XX[is.na(XX)] <- 0
               fitted <- c(XX %*% fit$coefficients)
               fc <- c(crossprod(fit$coefficients, c(1, factor[n,], rev(tail(y, ncol(X))))))
               # tail(c(rbind(c(1, rep(0, NCOL(XX)-1)), XX) %*% fit$coefficients))
               c(fitted, fc)
             },
             f = f)
  out <- do.call(cbind, fc)
  list(fc = out[NROW(out), ],
       res = sim_in - out[seq_len(NROW(sim_in)), ])
}

dfm <- function(y, factor, n_factor, f_lag, y_lag, h = 1) {
  y_lagh <- y_lag + h - 1
  f_lagh <- f_lag + h - 1
  if(y_lag == 0)
    X <- NULL
  else
    X <- vapply(seq(h, y_lagh, by = 1), \(l) dplyr::lag(y, n=l), numeric(length(y)))
  f <- do.call(cbind, lapply(seq(h, f_lagh), \(l) dplyr::lag(factor[,seq_len(n_factor)], n=l)))
  XX <- cbind(1, f, X)
  out <- lm.fit(x = XX[-seq_len(max(y_lagh, f_lagh)),],
                y = y[-seq_len(max(y_lagh, f_lagh))])
  # bench::mark(
  #   fit <<- lm.fit(x = XX[-seq_len(max(y_lagh, f_lagh)),],
  #                  y = y[-seq_len(max(y_lagh, f_lagh))]),
  #   fit2 <<- lm( y~., data = tibble(x = XX[-seq_len(max(y_lagh, f_lagh)),],
  #                                   y = y[-seq_len(max(y_lagh, f_lagh))])), check = FALSE)
  # bench::mark(BIC_lmfit(fit), BIC(fit2))
  attr(out, "X") <- XX
  out
}

# mat <- tar_read(sim_in, branches = 1)[[1]]
# n_factor <- 6
# y_lag <- 3
# f_lag <- 3
# h <- 2

auto_dfm <- function(mat, n_factor, f_lag, y_lag, h = 1) {
  meta <- lapply(c(n_factor, f_lag, y_lag), seq_len) %>%
    expand.grid() %>%
    `colnames<-`(c("n_factor", "f_lag", "y_lag")) %>%
    split(seq_len(nrow(.)))
  factor <- stats::prcomp(mat, center = TRUE, scale. = TRUE)$x[,seq_len(n_factor)]

  # dfm(mat[,1], factor, 6, 3, 3, h= 2)
  out_ls <- lapply(seq_len(NCOL(mat)), \(i){
    y <- mat[,i]
    fit_ls <- lapply(meta, \(mm){
      dfm(y, factor, mm[["n_factor"]], mm[["f_lag"]], mm[["y_lag"]], h = h)
    })
    j_min <- which.min(vapply(fit_ls, BIC_lmfit, numeric(1)))
    fit <- fit_ls[[j_min]]
    mm <- meta[[j_min]]
    XX <- attr(fit, "X")
    XX[is.na(XX)] <- 0
    fitted <- c(XX %*% fit$coefficients)
    sam_f <- factor[seq(length(y),by = -1, length.out = mm[["f_lag"]]), seq_len(mm[["n_factor"]])]
    sam_y <- y[seq(length(y), by = -1, length.out = mm[["y_lag"]])]
    fc <- c(crossprod(fit$coefficients, c(1, t(sam_f), sam_y)))
    # temp <- predict_dfm(matrix(y), factor[,seq_len(mm[["n_factor"]])],
    #                     f_lag = mm[["f_lag"]], y_lag =  mm[["y_lag"]], h = h)
    # str(temp)
    # str(y-fitted)
    # identical(y-fitted, c(temp$res))
    list(fc = fc, res = y-fitted)
  })

  fc <- vapply(out_ls, `[[`, "fc", FUN.VALUE = numeric(1))
  res <- vapply(out_ls, `[[`, "res", FUN.VALUE = numeric(NROW(mat)))
  list(fc = fc, res = res)
}


