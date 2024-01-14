component <- function(mat,
                      component = c("PCA", "normal", "uniform", "PCA_normal", "PCA_uniform", "ortho_normal"),
                      p = ncol(mat)){
  component <- match.arg(component)
  rnorm_w_once <- function(n){
    a <- rnorm(n)
    a/sqrt(sum(a^2))
  }
  runif_w_once <- function(n){
    a <- runif(n, min = -1, max = 1)
    a/sqrt(sum(a^2))
  }
  comp_names <- paste0("Component", seq_len(p))
  comp_fn <- switch(
    component,
    "PCA_normal" = ,
    "PCA" = function(mat, p) {
      pca <- stats::prcomp(mat, center = FALSE, scale. = FALSE)
      rotation <- pca$rotation[,seq_len(min(ncol(mat),p))]
      if(p>ncol(mat)){

        rr <- replicate(p-ncol(mat), rnorm_w_once(ncol(mat)))
        # rownames(rr) <- colnames(mat)
        rotation <- cbind(rotation, rr)
      }
      rownames(rotation) <- colnames(mat)
      x <- mat %*% rotation
      list(x = x, Phi = t(rotation))
    },
    "PCA_uniform" = function(mat, p) {
      pca <- stats::prcomp(mat, center = FALSE, scale. = FALSE)
      rotation <- pca$rotation[,seq_len(min(ncol(mat),p))]
      if(p>ncol(mat)){

        rr <- replicate(p-ncol(mat), runif_w_once(ncol(mat)))
        # rownames(rr) <- colnames(mat)
        rotation <- cbind(rotation, rr)
      }
      rownames(rotation) <- colnames(mat)
      x <- mat %*% rotation
      list(x = x, Phi = t(rotation))
    },
    "normal" = function(mat, p){
      rotation <- replicate(p, rnorm_w_once(ncol(mat)))
      rownames(rotation) <- colnames(mat)
      x <- mat %*% rotation
      list(x = x, Phi = t(rotation))
    },
    "uniform" = function(mat, p){
      rotation <- replicate(p, runif_w_once(ncol(mat)))
      rownames(rotation) <- colnames(mat)
      x <- mat %*% rotation
      list(x = x, Phi = t(rotation))
    },
    "ortho_normal" = function(mat, p){
      rotation <- pracma::randortho(ncol(mat))[,seq_len(min(ncol(mat),p))]
      if(p>ncol(mat)){

        rr <- replicate(p-ncol(mat), rnorm_w_once(ncol(mat)))
        # rownames(rr) <- colnames(mat)
        rotation <- cbind(rotation, rr)
      }
      rownames(rotation) <- colnames(mat)
      x <- mat %*% rotation
      list(x = x, Phi = t(rotation))
    },
  )
  comp_fn(mat, p)
}
