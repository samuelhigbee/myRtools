#' Heteroskedasticity robust HC1 covariance matrix
#'
#' Computes HC1 covariance matrix of estimates
#' @param Y The Y data used in estimation
#' @param X The X data used in estimation
#' @param beta The vector of linear regression estimates
#' @export

hc0.var <- function(Y, X, beta) {
  resid <- as.vector(Y - X %*% beta)
  df.correct <- length(Y) / (length(Y) - length(beta))
  var <- solve(t(X)%*%X) %*% t(X)%*%diag(resid^2)%*%X %*% solve(t(X)%*%X)
  return(var)
}
