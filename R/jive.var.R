#' Jackknife IV covariance matrix
#'
#' Computes jackknife IV covariance matrix of estimates
#' @param Y The Y data used in estimation
#' @param X The X data used in estimation
#' @param Z The Z used in estimation
#' @param W The W used in estimation
#' @param beta The vector of regression estimates
#' @export

jive.var <- function(Y, X, Z, W, beta) {
  n.Z <- cbind(Z,W)
  n.X <- cbind(X,W)
  resid <- as.vector(Y - n.X %*% beta)
  h <- diag(n.Z %*% solve(t(n.Z)%*%n.Z) %*% t(n.Z)) # compute leverage
  P <- t(n.X) %*% n.Z %*% solve(t(n.Z)%*%n.Z) # Pi-hat
  j.X <- (n.Z %*% t(P) - h * n.X) / (1 - h) # jackknifed X
  denom <- t(j.X) %*% n.X
  num <- t(j.X) %*% diag(resid)
  var <- solve(denom) %*% num %*% t(num) %*% solve(denom)
  return(var)
}