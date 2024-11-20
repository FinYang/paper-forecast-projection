## ---- geometry-load ----

library(tikzDevice)
tikzLatexPackages_opt <- getOption("tikzLatexPackages")
tikzLatexPackages_opt <- tikzLatexPackages_opt[!tikzLatexPackages_opt == "\\usepackage{amsfonts}"]
tikzLatexPackages_opt <- tikzLatexPackages_opt[!tikzLatexPackages_opt == "\\usepackage{bm}"]
options(
  #   tikzFooter = "\\caption{a caption}",
  tikzLatexPackages = c(
    tikzLatexPackages_opt,
    "\\usepackage{amsfonts}",
    "\\usepackage{bm}"
  )
)


# functions
point <- function(p, pch = 20, ...) {
  if (!is.matrix(p) || ncol(p) == 1) p <- t(p)
  points(p[, 1], p[, 2], pch = pch, ...)
}
line <- function(start, end, ...) {
  if (!is.matrix(start) || ncol(start) == 1) start <- t(start)
  if (!is.matrix(end) || ncol(end) == 1) end <- t(end)
  for (i in seq_len(nrow(start))) {
    lines(c(start[i, 1], end[i, 1]), c(start[i, 2], end[i, 2]), ...)
  }
}
arrow <- function(start, end, length = 0.05, ...) {
  arrows(start[[1]], start[[2]], end[[1]], end[[2]], length = length, ...)
}
tex <- function(p, ...) {
  text(p[[1]], p[[2]], ...)
}
#' @param p point
#' @param l line
project <- function(p, l, W = diag(2)) {
  if (!is.matrix(p) || ncol(p) == 1) p <- t(p)
  W_inv <- solve(W)
  SP <- l %*% solve(t(l) %*% W_inv %*% l) %*% t(l) %*% W_inv
  t(SP %*% t(p))
}

# colours
col_hat <- "blue"
col_til <- "red"
col_proj <- "blue"


insert_figure <- function(file) {
  content <- readLines(file)
  output <- c(
    r'(\begin{figure})',
    r'(\centering)',
    r'(\resizebox{\textwidth}{!}{%)',
    content,
    r'(})',
    # r'(\caption{})',
    r'(\end{figure})'
  )
  cat(output, sep = "\n")
}



height <- 2.5
width <- 4
window_ylim_max <- 9
pos_1_offset <- 0.8

## ---- geom-out ----
# run when called
insert_figure(file_geom)

## ---- ortho-1 ----

file_geom <- tempfile(fileext = ".tex")
tikz(file_geom, height = height, width = width)
par(mar = rep(0, 4))
plot.new()
# if (FALSE) {
plot.window(xlim = c(-0.5, 10.5), ylim = c(-1, window_ylim_max))
# x-axis
arrow(c(-0.5, 0), c(10.5, 0))
# y-axis
ylim <- 8

tex(c(10, 0), r'($\bm{y} \in \mathbb{R}^m$)', pos = 3)

# coordinates
# y and (y, c)
y <- c(5, 0)
# yhat
yhat <- c(3, 0)


# points
point(y)
point(yhat, col = col_hat)

tex(y, r'($\bm{y}$)', pos = 1, offset = pos_1_offset)
tex(yhat, r'($\bm{\hat{y}}$)', pos = 1, offset = pos_1_offset, col = col_hat)
# }
dev.off()
# tools::texi2dvi("flap-ortho.tex", pdf = TRUE, clean = TRUE)


## ---- ortho-2 ----

file_geom <- tempfile(fileext = ".tex")
tikz(file_geom, height = height, width = width)
par(mar = rep(0, 4))
plot.new()
# if (FALSE) {
plot.window(xlim = c(-0.5, 10.5), ylim = c(-1, window_ylim_max))
# x-axis
arrow(c(-0.5, 0), c(10.5, 0))
# y-axis
ylim <- 8
arrow(c(0, -1), c(0, ylim))
# coherent space
s_space <- c(10, 4.5)

tex(c(0, ylim), r'($\bm{c} \in \mathbb{R}^p$)', pos = 3)
tex(c(10, 0), r'($\bm{y} \in \mathbb{R}^m$)', pos = 3)

# coordinates
# y and (y, c)
c <- y <- c(5, 0)
c[[2]] <- y[[1]] * s_space[[2]] / s_space[[1]]

# yhat
yhat <- c(3, 0)
# chat
chat <- yhat + c(0, 3)

# points
point(y)
point(c)
point(yhat, col = col_hat)
point(chat, col = col_hat)

# vertical dashed lines
line(c(3, -0.8), c(3, 6), lty = "dashed", col = col_hat)
line(y, c, lty = "dashed")

tex(y, r'($\bm{y}$)', pos = 1, offset = pos_1_offset)
tex(c, r'($(\bm{y}, \bm{c})$)', pos = 4)
tex(yhat, r'($\bm{\hat{y}}$)', pos = 1, offset = pos_1_offset, col = col_hat)
tex(chat, r'($(\bm{\hat{y}}, \bm{\hat{c}})$)', pos = 2, col = col_hat)
# }
dev.off()

## ---- ortho-3 ----

file_geom <- tempfile(fileext = ".tex")
tikz(file_geom, height = height, width = width)
par(mar = rep(0, 4))
plot.new()
# if (FALSE) {
plot.window(xlim = c(-0.5, 10.5), ylim = c(-1, window_ylim_max))
# x-axis
arrow(c(-0.5, 0), c(10.5, 0))
# y-axis
ylim <- 8
arrow(c(0, -1), c(0, ylim))
# coherent space
s_space <- c(10, 4.5)
arrow(c(0, 0), s_space, lwd = 2)

tex(s_space, r'($\mathfrak{s}$)', pos = 3)
tex(c(0, ylim), r'($\bm{c} \in \mathbb{R}^p$)', pos = 3)
tex(c(10, 0), r'($\bm{y} \in \mathbb{R}^m$)', pos = 3)

# coordinates
# y and (y, c)
c <- y <- c(5, 0)
c[[2]] <- y[[1]] * s_space[[2]] / s_space[[1]]

# yhat
yhat <- c(3, 0)
# chat
chat <- yhat + c(0, 3)

# points
point(y)
point(c)
point(yhat, col = col_hat)
point(chat, col = col_hat)

# vertical dashed lines
line(c(3, -0.8), c(3, 6), lty = "dashed", col = col_hat)
line(y, c, lty = "dashed")

tex(y, r'($\bm{y}$)', pos = 1, offset = pos_1_offset)
tex(c, r'($(\bm{y}, \bm{c})$)', pos = 4)
tex(yhat, r'($\bm{\hat{y}}$)', pos = 1, offset = pos_1_offset, col = col_hat)
tex(chat, r'($(\bm{\hat{y}}, \bm{\hat{c}})$)', pos = 2, col = col_hat)
# }
dev.off()

## ---- ortho-4 ----

file_geom <- tempfile(fileext = ".tex")
tikz(file_geom, height = height, width = width)
par(mar = rep(0, 4))
plot.new()
# if (FALSE) {
plot.window(xlim = c(-0.5, 10.5), ylim = c(-1, window_ylim_max))
# x-axis
arrow(c(-0.5, 0), c(10.5, 0))
# y-axis
ylim <- 8
arrow(c(0, -1), c(0, ylim))
# coherent space
s_space <- c(10, 4.5)
arrow(c(0, 0), s_space, lwd = 2)

tex(s_space, r'($\mathfrak{s}$)', pos = 3)
tex(c(0, ylim), r'($\bm{c} \in \mathbb{R}^p$)', pos = 3)
tex(c(10, 0), r'($\bm{y} \in \mathbb{R}^m$)', pos = 3)

# coordinates
# y and (y, c)
c <- y <- c(5, 0)
c[[2]] <- y[[1]] * s_space[[2]] / s_space[[1]]

# yhat
yhat <- c(3, 0)
# chat
chat <- yhat + c(0, 3)

# ytil and (ytil, ctil)
ytil <- ctil <- project(chat, s_space)
ytil[[2]] <- 0


# points
point(y)
point(c)
point(yhat, col = col_hat)
point(chat, col = col_hat)
point(ctil, col = col_til)

# vertical dashed lines
line(c(3, -0.8), c(3, 6), lty = "dashed", col = col_hat)
line(y, c, lty = "dashed")

arrow(chat, ctil, col = col_proj)
arrow(ctil, ytil, col = col_til)



tex(y, r'($\bm{y}$)', pos = 1, offset = pos_1_offset)
tex(c, r'($(\bm{y}, \bm{c})$)', pos = 4)
tex(yhat, r'($\bm{\hat{y}}$)', pos = 1, offset = pos_1_offset, col = col_hat)
tex(chat, r'($(\bm{\hat{y}}, \bm{\hat{c}})$)', pos = 2, col = col_hat)
tex(ctil, r'($(\bm{\tilde{y}}, \bm{\tilde{c}})$)', pos = 3, col = col_til)
# }
dev.off()

## ---- ortho-5 ----

file_geom <- tempfile(fileext = ".tex")
tikz(file_geom, height = height, width = width)
par(mar = rep(0, 4))
plot.new()
# if (FALSE) {
plot.window(xlim = c(-0.5, 10.5), ylim = c(-1, window_ylim_max))
# x-axis
arrow(c(-0.5, 0), c(10.5, 0))
# y-axis
ylim <- 8
arrow(c(0, -1), c(0, ylim))
# coherent space
s_space <- c(10, 4.5)
arrow(c(0, 0), s_space, lwd = 2)

tex(s_space, r'($\mathfrak{s}$)', pos = 3)
tex(c(0, ylim), r'($\bm{c} \in \mathbb{R}^p$)', pos = 3)
tex(c(10, 0), r'($\bm{y} \in \mathbb{R}^m$)', pos = 3)

# coordinates
# y and (y, c)
c <- y <- c(5, 0)
c[[2]] <- y[[1]] * s_space[[2]] / s_space[[1]]

# yhat
yhat <- c(3, 0)
# chat
chat <- yhat + c(0, 3)

# ytil and (ytil, ctil)
ytil <- ctil <- project(chat, s_space)
ytil[[2]] <- 0


# points
point(y)
point(c)
point(yhat, col = col_hat)
point(chat, col = col_hat)
point(ytil, col = col_til)
point(ctil, col = col_til)

# vertical dashed lines
line(c(3, -0.8), c(3, 6), lty = "dashed", col = col_hat)
line(y, c, lty = "dashed")

arrow(chat, ctil, col = col_proj)
arrow(ctil, ytil, col = col_til)

tex(y, r'($\bm{y}$)', pos = 1, offset = pos_1_offset)
tex(c, r'($(\bm{y}, \bm{c})$)', pos = 4)
tex(yhat, r'($\bm{\hat{y}}$)', pos = 1, offset = pos_1_offset, col = col_hat)
tex(chat, r'($(\bm{\hat{y}}, \bm{\hat{c}})$)', pos = 2, col = col_hat)
tex(ytil, r'($\bm{\tilde{y}}$)', pos = 1, offset = pos_1_offset, col = col_til)
tex(ctil, r'($(\bm{\tilde{y}}, \bm{\tilde{c}})$)', pos = 3, col = col_til)
# }
dev.off()


## ---- obliq-1 ----
file_geom <- tempfile(fileext = ".tex")
tikz(file_geom, height = height, width = width)
par(mar = rep(0, 4))
plot.new()
plot.window(xlim = c(-0.5, 10.5), ylim = c(-1, window_ylim_max))

# x-axis
arrow(c(-0.5, 0), c(10.5, 0))
# y-axis
ylim <- 8
arrow(c(0, -1), c(0, ylim))
# coherent space
s_space <- c(10, 4.5)
arrow(c(0, 0), s_space, lwd = 2)
# direction of the residuals
r_space <- c(7, 8)
arrow(c(0, 0), r_space)

# y and (y, c)
c <- y <- c(5, 0)
c[[2]] <- y[[1]] * s_space[[2]] / s_space[[1]]

set.seed(2222)
n <- 20
rmat <- cbind(r_space, c(-1, 1))
e <- t(rmat %*% matrix(rnorm(2 * n, 0, 0.15), 2, n))
yhat <- chat <- t(t(e) + c)
yhat[, 2] <- 0


# points
point(chat, col = col_hat)
point(yhat, col = col_hat)
line(yhat, chat, lty = "dashed", col = col_hat)
point(y)
point(c)

tex(s_space, r'($\mathfrak{s}$)', pos = 3)
tex(r_space, r'($\mathfrak{R}$)', pos = 3)
tex(c(0, ylim), r'($\bm{c} \in \mathbb{R}^p$)', pos = 3)
tex(c(10, 0), r'($\bm{y} \in \mathbb{R}^m$)', pos = 3)


tex(y, r'($\bm{y}$)', pos = 1, offset = pos_1_offset)
tex(c, r'($(\bm{y}, \bm{c})$)', pos = 4)
tex(yhat[which.max(yhat[, 1]), ], r'($\bm{\hat{y}}$)', pos = 1, offset = pos_1_offset, col = col_hat)
tex(chat[which.max(chat[, 2]), ], r'($(\bm{\hat{y}}, \bm{\hat{c}})$)', pos = 3, col = col_hat)


dev.off()
## ---- obliq-2 ----

file_geom <- tempfile(fileext = ".tex")
tikz(file_geom, height = height, width = width)
par(mar = rep(0, 4))
plot.new()
plot.window(xlim = c(-0.5, 10.5), ylim = c(-1, window_ylim_max))

# x-axis
arrow(c(-0.5, 0), c(10.5, 0))
# y-axis
ylim <- 8
arrow(c(0, -1), c(0, ylim))
# coherent space
s_space <- c(10, 4.5)
arrow(c(0, 0), s_space, lwd = 2)
# direction of the residuals
r_space <- c(7, 8)
arrow(c(0, 0), r_space)

tex(s_space, r'($\mathfrak{s}$)', pos = 3)
tex(r_space, r'($\mathfrak{R}$)', pos = 3)
tex(c(0, ylim), r'($\bm{c} \in \mathbb{R}^p$)', pos = 3)
tex(c(10, 0), r'($\bm{y} \in \mathbb{R}^m$)', pos = 3)

# y and (y, c)
c <- y <- c(5, 0)
c[[2]] <- y[[1]] * s_space[[2]] / s_space[[1]]

set.seed(2222)
n <- 20
rmat <- cbind(r_space, c(-1, 1))
e <- t(rmat %*% matrix(rnorm(2 * n, 0, 0.15), 2, n))
yhat <- chat <- t(t(e) + c)
yhat[, 2] <- 0

ctil <- project(chat, s_space, W = cov(chat))

line(chat, ctil, lty = "dashed", col = col_proj)
# points
point(chat, col = col_hat)
point(yhat, col = col_hat)
point(ctil, col = col_til)
point(y)
point(c)

tex(y, r'($\bm{y}$)', pos = 1, offset = pos_1_offset)
tex(c, r'($(\bm{y}, \bm{c})$)', pos = 4)
tex(yhat[which.max(yhat[, 1]), ], r'($\bm{\hat{y}}$)', pos = 1, offset = pos_1_offset, col = col_hat)
tex(chat[which.max(chat[, 2]), ], r'($(\bm{\hat{y}}, \bm{\hat{c}})$)', pos = 3, col = col_hat)
tex(ctil[which.max(ctil[, 2]), ], r'($(\bm{\tilde{y}}, \bm{\tilde{c}})$)', pos = 4, col = col_til)


dev.off()
## ---- obliq-3 ----

file_geom <- tempfile(fileext = ".tex")
tikz(file_geom, height = height, width = width)
par(mar = rep(0, 4))
plot.new()
plot.window(xlim = c(-0.5, 10.5), ylim = c(-1, window_ylim_max))

# x-axis
arrow(c(-0.5, 0), c(10.5, 0))
# y-axis
ylim <- 8
arrow(c(0, -1), c(0, ylim))
# coherent space
s_space <- c(10, 4.5)
arrow(c(0, 0), s_space, lwd = 2)
# direction of the residuals
r_space <- c(7, 8)
arrow(c(0, 0), r_space)

tex(s_space, r'($\mathfrak{s}$)', pos = 3)
tex(r_space, r'($\mathfrak{R}$)', pos = 3)
tex(c(0, ylim), r'($\bm{c} \in \mathbb{R}^p$)', pos = 3)
tex(c(10, 0), r'($\bm{y} \in \mathbb{R}^m$)', pos = 3)

# y and (y, c)
c <- y <- c(5, 0)
c[[2]] <- y[[1]] * s_space[[2]] / s_space[[1]]

set.seed(2222)
n <- 20
rmat <- cbind(r_space, c(-1, 1))
e <- t(rmat %*% matrix(rnorm(2 * n, 0, 0.15), 2, n))
yhat <- chat <- t(t(e) + c)
yhat[, 2] <- 0

ytil <- ctil <- project(chat, s_space, W = cov(chat))
ytil[, 2] <- 0



# line(chat, ctil, lty = "dashed", col = col_hat)
line(ctil, ytil, lty = "dashed", col = col_til)
# points
# point(chat, col = col_hat)
point(yhat, col = col_hat)
point(ctil, col = col_til)
point(ytil, col = col_til)
point(y)
point(c)


tex(y, r'($\bm{y}$)', pos = 1, offset = pos_1_offset)
tex(c, r'($(\bm{y}, \bm{c})$)', pos = 4)
tex(yhat[which.max(yhat[, 1]), ], r'($\bm{\hat{y}}$)', pos = 1, offset = pos_1_offset, col = col_hat)
# tex(chat[which.max(chat[, 2]), ], r'($(\bm{\hat{y}}, \bm{\hat{c}})$)', pos = 3, col = col_hat)
tex(ctil[which.max(ctil[, 2]), ], r'($(\bm{\tilde{y}}, \bm{\tilde{c}})$)', pos = 4, col = col_til)
tex(ytil[which.max(ytil[, 1]), ], r'($\bm{\tilde{y}}$)', pos = 1, offset = pos_1_offset, col = col_til)


dev.off()
