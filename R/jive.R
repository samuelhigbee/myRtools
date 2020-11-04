#' Jackknife IV estimator
#'
#' Estimates jackknife IV coefficients.
#' @param Y The outcome variables
#' @param X The endogenous variables
#' @param Z The excluded instruments
#' @param W The included instruments or controls
#' @export

jive <- function(Y, X, Z, W) {
  n.Z <- cbind(Z,W)
  n.X <- cbind(X,W)
  h <- diag(n.Z %*% solve(t(n.Z)%*%n.Z) %*% t(n.Z)) # compute leverage
  P <- t(n.X) %*% n.Z %*% solve(t(n.Z)%*%n.Z) # Pi-hat
  j.X <- (n.Z %*% t(P) - h * n.X) / (1 - h) # jackknifed X
  beta <- solve(t(j.X)%*%n.X) %*% t(j.X) %*% Y
  return(beta)
}
