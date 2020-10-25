#' Sieve
#'
#' This performs a sive estimation
#' @param Y A vector of outcomes
#' @param X A vector of predictors
#' @param order The order of the polynomial
#' @param space The values at which to predict
#'
sieve <- function(Y, X, order=1, space) {
  Z <- NULL
  Z.predict <- NULL
  # construct data for least squares estimation
  for (j in 0:order) {
    Z <- cbind(Z,X^j)
    Z.predict <- cbind(Z.predict,space^j)
  }
  # construct estimator
  beta <- solve(t(Z) %*% Z) %*% t(Z) %*% Y
  predicted.vals <- Z.predict %*% beta
  return(predicted.vals)
}
