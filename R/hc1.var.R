#' Heteroskedasticity robust HC1 covariance matrix
#'
#' Computes HC1 covariance matrix of estimates
#' @param Y The Y data used in estimation
#' @param X The X data used in estimation
#' @param beta The vector of linear regression estimates
#' @param const If TRUE, adds a constant column to the data
#' @export

hc1.var <- function(Y, X, beta, const=TRUE) {
  n.X <- as.matrix(X)
  n.Y <- as.matrix(Y)
  if (const==TRUE) {
    n.X <- cbind(1,n.X)
  }
  resid <- as.vector(n.Y - n.X %*% beta)
  df.correct <- nrow(n.Y) / (nrow(n.Y) - length(beta))
  var <- df.correct *solve(t(n.X)%*%n.X) %*%
          t(n.X)%*%diag(resid^2)%*%n.X %*%
          solve(t(n.X)%*%n.X)
  return(var)
}
