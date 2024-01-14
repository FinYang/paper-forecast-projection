list2array <- function(xlist){
  d1 <- unique(vapply(xlist, NROW, numeric(1)))
  if(length(d1) != 1) stop("Different row number")
  d2 <- unique(vapply(xlist, NCOL, numeric(1)))
  if(length(d2) != 1) stop("Different col number")
  xlist %>%
    unlist() %>%
    array(dim = (c(d1, d2, length(xlist))),
          dimnames = list(NULL, NULL, names(xlist)))
}

array2list <- function(xarray){
  as.list(plyr:::splitter_a(xarray,3, .id = NULL))
}


