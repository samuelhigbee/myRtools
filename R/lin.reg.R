#' Basic linear regression
#'
#' Estimates linear regression coefficients. Standard errors should be computed
#' with a different function depending on various assumptions.
#' @param Y The outcome variables
#' @param X The explanatory variables
#' @param const If TRUE, adds a constant column to the data
#' @param tol Tolerance level for matrix inversion
#' @export

lin.reg <- function(Y, X, const=TRUE, tol=.Machine$double.eps) {
  n.X <- as.matrix(X)
  n.Y <- as.matrix(Y)
  if (const==TRUE) {
    n.X <- cbind(1,n.X)
  }
  beta <- solve( t(n.X) %*% n.X, tol=tol) %*% t(n.X) %*% n.Y
  return(beta)
}
