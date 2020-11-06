#' Weighted leas squares regression
#'
#' Estimates linear regression coefficients. Standard errors should be computed
#' with a different function depending on various assumptions.
#' @param Y The outcome variables
#' @param X The explanatory variables
#' @param W The weighting matrix
#' @export

wls <- function(Y, X, W) {
  beta <- solve(t(X)%*%W%*%X)  %*% t(X) %*% W %*% Y
  return(beta)
}
