#' Two-stage least squares
#'
#' Estimates TSLS coefficients.
#' @param Y The outcome variables
#' @param X The endogenous variables
#' @param Z The excluded instruments
#' @param W The included instruments or controls
#' @export

tsls <- function(Y, X, Z, W, const=TRUE) {
  n.Z <- cbind(Z,W)
  n.X <- cbind(X,W)
  if (const==TRUE) {
    n.Z <- cbind(1,n.Z)
    n.X <- cbind(1,n.X)
  }
  beta <- solve( t(n.X) %*% n.Z %*% solve(t(n.Z)%*%n.Z) %*% t(n.Z) %*% n.X) %*%
    t(n.X) %*% n.Z %*% solve(t(n.Z)%*%n.Z) %*% t(n.Z) %*% Y
  return(beta)
}
