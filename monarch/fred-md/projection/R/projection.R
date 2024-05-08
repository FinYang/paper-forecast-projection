

# fc <- cbind(tar_read(fc_dfm, branches = 1)[[1]], tar_read(fc_arima_pca_normal, branches = 1)[[1]])
# W <- tar_read(W_dfm_pca_normal, branches = 1)[[1]]
# Phi <- tar_read(pca_normal, branches = 1)[[1]]$Phi
#' Having different W for different h
project <- function(fc, W, Phi) {
  fc <- unname(fc)
  C_all <- cbind(-Phi, diag(nrow(Phi)))
  p_max <- nrow(Phi)
  m <- ncol(fc) - nrow(Phi)

  mapply(\(p, W){
    C <- block(C_all, p, m+p)
    WtC <- tcrossprod(W, C)
    tbf <- fc[,seq_len(m+p)]
    t((t(tbf)-tcrossprod(WtC, t(solve(C %*% WtC, tcrossprod(C, tbf)))))[seq_len(m),])
  },
  p = seq_len(p_max),
  W = W[seq_len(p_max)],
  SIMPLIFY = FALSE)
}

block <- function(mat, m, n = m){
  mat[seq_len(m), seq_len(n), drop = FALSE]
}

# res_ori <- tar_read(res_dfm, branches = 1)[[1]][[1]]
# res_com <- tar_read(res_arima_pca_normal, branches = 1)[[1]]
#' Having different W for different h
get_W <- function(res_ori, res_com) {
  m <- NCOL(res_ori)
  p <- NCOL(res_com)
  res <- cbind(res_ori, res_com)
  res <- res[!apply(res, 1, anyNA),]
  lapply(
    seq_len(p),
    \(pp) corpcor::cov.shrink(res[,seq_len(m+pp)], verbose = FALSE))
}
