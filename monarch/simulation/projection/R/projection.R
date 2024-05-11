project_1 <- function(fc, W, Phi){
  C_all <- cbind(-Phi, diag(nrow(Phi)))
  p_max <- nrow(Phi)
  m <- ncol(fc) - nrow(Phi)
  mapply(\(p, W){
    C <- block(C_all, p, m+p)
    WtC <- tcrossprod(W, C)
    tbf <- fc[,seq_len(m+p)]
    t((t(tbf)-tcrossprod(tcrossprod(WtC, t(solve(C %*% WtC, C))), tbf))[seq_len(m),])
  },
  p = seq_len(p_max),
  W = W,
  SIMPLIFY = FALSE)
}

# fc <- cbind(tar_read(fc_dfm, branches = 1)[[1]], tar_read(fc_arima_pca, branches = 1)[[1]])
# W <- tar_read(W_dfm, branches = 1)[[1]]
# Phi <- tar_read(pca, branches = 1)[[1]]$Phi
#' Having different W for different h
project <- function(fc, W, Phi) {
  C_all <- cbind(-Phi, diag(nrow(Phi)))
  p_max <- nrow(Phi)
  m <- ncol(fc) - nrow(Phi)
  pbapply::pbmapply(\(fc, W){
    mapply(\(p, W){
      C <- block(C_all, p, m+p)
      WtC <- tcrossprod(W, C)
      bf <- c(fc[seq_len(m+p)])
      (bf -tcrossprod(WtC, t(solve(C %*% WtC, C))) %*% bf)[seq_len(m),]
    },
    p = seq_len(p_max),
    W = W,
    SIMPLIFY = FALSE)
  },
  fc = asplit(fc, 1),
  W = W,
  SIMPLIFY = FALSE
  ) %>%
    lapply(\(x) do.call(cbind, x)) %>%
    list2array() %>%
    aperm(c(3, 1, 2)) %>%
    array2list()
}

block <- function(mat, m, n = m){
  mat[seq_len(m), seq_len(n), drop = FALSE]
}

# res_ori <- tar_read(res_dfm, branches = 1)[[1]]
# res_com <- tar_read(res_arima_pca, branches = 1)[[1]]
#' Having different W for different h
get_W <- function(res_ori, res_com) {
  m <- NCOL(res_ori[[1]])
  p <- NCOL(res_com[[1]])
  mapply(\(ro, rc) {
    res <- cbind(ro, rc)
    res <- res[!apply(res, 1, anyNA),]
    lapply(
      seq_len(p),
      \(pp){
        out <- try(corpcor::cov.shrink(res[,seq_len(m+pp)], verbose = FALSE))
        # # trying to avoid singularity issue with svd
        while(class(out) == "try-error")
          out <- try(corpcor::cov.shrink((res <- res[-1,seq_len(m+pp)]), verbose = FALSE))
        out
      })
  },
  ro = res_ori,
  rc = res_com,
  SIMPLIFY = FALSE)
}

# sim_in <- tar_read(sim_in, branches = 1)[[1]]
# fitted_ori <- tar_read(fitted_arima, branches = 1)[[1]]
# fitted_com <- tar_read(fitted_arima_pca, branches = 1)[[1]]
# res_com <- tar_read(res_arima_pca, branches = 1)[[1]]
# fc <- tar_read(fc_arima, branches = 1)[[1]]
# fc_c <- tar_read(fc_arima_pca, branches = 1)[[1]]
# Phi <- tar_read(pca, branches = 1)[[1]]$Phi

project_iter <-  function(sim_in, fitted_ori, fitted_com, res_com,
                          fc, fc_c, Phi) {
  m <- NCOL(fitted_ori[[1]])
  p <- NCOL(fitted_com[[1]])
  out <- mapply(
    \(fit_ori, fit_com, res_com, fc, fc_c, sim_in, Phi) {
      rec_fit <- vector("list", p + 1)
      rec_res <- vector("list", p)
      W_ls <- vector("list", p)
      rec_fc <- vector("list", p)

      rec_fit[[1]] <- fit_ori

      proj_inner <- function(bf, WtC){
        (bf -tcrossprod(WtC, t(solve(C %*% WtC, C))) %*% bf)[seq_len(m),]
      }
      for(i in seq_len(p)) {
        rec_res[[i]] <- sim_in - rec_fit[[i]]
        res <- cbind(rec_res[[i]], res_com[,i])
        res <- res[!apply(res, 1, anyNA),]
        W_ls[[i]] <- corpcor::cov.shrink(res, verbose = FALSE)
        C <- cbind(-Phi[i,,drop = FALSE], 1)
        WtC <- tcrossprod(W_ls[[i]], C)
        rec_fc[[i]] <- proj_inner(bf = c(fc, fc_c[[i]]), WtC)
        rec_fit[[i + 1]] <- t(proj_inner(bf = t(cbind(rec_fit[[i]], fit_com[,i])), WtC))
      }
      rec_fc
    },
    fit_ori = fitted_ori,
    fit_com = fitted_com,
    res_com = res_com,
    fc = asplit(fc, 1),
    fc_c = asplit(fc_c, 1),
    MoreArgs =list(sim_in = sim_in,
                   Phi = Phi),
    SIMPLIFY = FALSE
  )

  out %>%
    lapply(\(x) do.call(rbind, x)) %>%
    list2array() %>%
    aperm(c(3, 2, 1)) %>%
    array2list()
}
