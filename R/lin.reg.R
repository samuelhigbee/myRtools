#' Basic linear regression
#'
#' Estimates linear regression coefficients. Standard errors should be computed
#' with a different function depending on various assumptions.
#' @param Y The outcome variables
#' @param X The explanatory variables
#' @export

lin.reg <- function(Y, X, const=TRUE) {
  n.X <- as.matrix(X)
  n.Y <- as.matrix(Y)
  if (const==TRUE) {
    n.X <- cbind(1,n.X)
  }
  beta <- solve( t(n.X) %*% n.X) %*% t(n.X) %*% n.Y
  return(beta)
}
