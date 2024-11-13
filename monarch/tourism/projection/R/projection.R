# fc <- cbind(tar_read(fc_dfm, branches = 1)[[1]], tar_read(fc_arima_pca_normal, branches = 1)[[1]])
# W <- tar_read(W_dfm_pca_normal, branches = 1)[[1]]
# Phi <- tar_read(pca_normal, branches = 1)[[1]]$Phi
#' Having different W for different h
project <- function(fc, W, Phi) {
  fc <- unname(fc)
  C_all <- cbind(-Phi, diag(nrow(Phi)))
  p_max <- nrow(Phi)
  m <- ncol(fc) - nrow(Phi)

  mapply(
    \(p, W){
      C <- block(C_all, p, m + p)
      WtC <- tcrossprod(W, C)
      tbf <- fc[, seq_len(m + p)]
      t((t(tbf) - tcrossprod(WtC, t(solve(C %*% WtC, tcrossprod(C, tbf)))))[seq_len(m), ])
    },
    p = seq_len(p_max),
    W = W[seq_len(p_max)],
    SIMPLIFY = FALSE
  )
}

block <- function(mat, m, n = m) {
  mat[seq_len(m), seq_len(n), drop = FALSE]
}

# res_ori <- tar_read(res_dfm, branches = 1)[[1]][[1]]
# res_com <- tar_read(res_arima_pca_normal, branches = 1)[[1]]
#' Having different W for different h
get_W <- function(res_ori, res_com) {
  m <- NCOL(res_ori)
  p <- NCOL(res_com)
  res <- cbind(res_ori, res_com)
  res <- res[!apply(res, 1, anyNA), ]
  lapply(
    seq_len(p),
    \(pp) corpcor::cov.shrink(res[, seq_len(m + pp)], verbose = FALSE)
  )
}


project_switch <- function(fc, fc_comp1, fc_comp2,
                           Phi1, Phi2,
                           res, res_comp1, res_comp2,
                           switch_from) {
  fc <- unname(fc)
  max_p <- ncol(fc_comp1)
  m <- ncol(fc)
  h <- nrow(fc)
  stopifnot(all.equal(ncol(fc_comp1), ncol(fc_comp2)))
  class(res) <- class(fc_comp2) <- class(fc_comp1) <- class(matrix())

  stopifnot(length(switch_from) == 1)
  out <- lapply(seq_len(max_p), \(p){
    if (p < switch_from) {
      fc_comp <- fc_comp1[, seq_len(p), drop = FALSE]
      Phi <- Phi1[seq_len(p), , drop = FALSE]
      res_comp <- res_comp1[, seq_len(p), drop = FALSE]
    } else {
      fc_comp <- cbind(
        fc_comp1[, seq_len(switch_from - 1), drop = FALSE],
        fc_comp2[, seq(switch_from, p), drop = FALSE]
      )
      Phi <- rbind(
        Phi1[seq_len(switch_from - 1), , drop = FALSE],
        Phi2[seq(switch_from, p), , drop = FALSE]
      )
      res_comp <- cbind(
        res_comp1[, seq_len(switch_from - 1), drop = FALSE],
        res_comp2[, seq(switch_from, p), drop = FALSE]
      )
    }
    flap::flap(fc, fc_comp, Phi, res, res_comp, p = p)[[1]]
  })
  out
}
