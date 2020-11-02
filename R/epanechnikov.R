#' Epanechnikov Kernel
#'
#' The Epanechnikov kernel
#' @param x The x value of the function
#' @param h The one-half bandwidth (i.e. x+h,x-h)
#' @export

epanechnikov <- function(x, h) {
  pos <- ( x > -h & x < h)
  epi <- pos * (3 / (4 * h^3)) * (h^2 - x^2)
  return(epi)
}
