#' Heteroskedasticity robust tsls covariance matrix
#'
#' Computes tsls covariance matrix of estimates
#' @param Y The Y data used in estimation
#' @param X The X data used in estimation
#' @param Z The Z used in estimation
#' @param W The W used in estimation
#' @param beta The vector of regression estimates
#' @export

tsls.var <- function(Y, X, Z, W, beta) {
  n.Z <- cbind(Z,W)
  n.X <- cbind(X,W)
  resid <- as.vector(Y - n.X %*% beta)
  denom <- t(n.X) %*% n.Z %*% solve(t(n.Z)%*%n.Z) %*% t(n.Z) %*% n.X
  num <- t(n.X) %*% n.Z %*% solve(t(n.Z)%*%n.Z) %*% t(n.Z) %*% diag(resid)
  var <- solve(denom) %*% num %*% t(num) %*% solve(denom)
  return(var)
}
