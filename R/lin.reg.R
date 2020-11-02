#' Basic linear regression
#'
#' Estimates linear regression coefficients. Standard errors should be computed
#' with a different function depending on various assumptions.
#' @param Y The outcome variables
#' @param X The explanatory variables
#' @export

lin.reg <- function(Y, X) {
  beta <- solve( t(X) %*% X) %*% t(X) %*% Y
  return(beta)
}
