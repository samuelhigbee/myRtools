#' Bootstrap
#'
#' This performs a bootstrap for a given function
#' @param dt The data to be used in the given function
#' @param func The function to be repeated.  The output of func should be the
#' estimate of interest.
#' @param S The number of samples to draw

bootstrap <- function(dt,func,S) {
  estimates <- NULL
  for (s in 1:S) {
    dt.new <- sample(dt,nrow(dt),replace=TRUE)
    estimates <- rbind(estimates,func(dt.new))
  }
  V <- 1/S * t(estimates) %*% estimates - 1/S^2 * colSums(estimates)
  return(V)
}
