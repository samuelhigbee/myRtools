#' Local polynomial regression
#'
#' This performs local polynomial regression in one dimension
#' @param Y A vector of outcomes
#' @param X A vector of predictors
#' @param h The one-half bandwidth to use in the Epanechnikov kernel
#' @param order The order of the polynomial
#' @param space The values at which to predict

local.polynomial <- function(Y, X, h, order=0, space) {
  # empty vector of predicted vals
  predicted.vals <- rep(NA,length(space))
  # construct data for least squares estimation
  for (i in 1:length(space)) {
    Z <- NULL
    Z.predict <- NULL
    for (j in 0:order) {
      Z <- cbind(Z,X^j)
      Z.predict <- cbind(Z.predict,space[i]^j)
    }
    # construct weighting matrix
    sqrt.W <- epanechnikov(X - space[i], h)
    W <- diag(nrow(Z)) * sqrt.W^2
    # construct estimator
    beta <- solve(t(Z) %*% W %*% Z) %*%  t(Z) %*% W %*% Y
    # construct estimate
    predicted.vals[i] <- Z.predict %*% beta
  }
  return(predicted.vals)
}
