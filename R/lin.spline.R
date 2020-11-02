#' Linear Spline
#'
#' This function fits a linear spline of Y on X and returns predicted values
#' @param Y A vector of outcomes
#' @param X A vector of predictors
#' @param knots The number of knots in the spline
#' @param space A vector of X values at which to predict
#' @export

lin.spline <- function(Y, X, knots=4, space){
  # construct data for least squares estimation
  Z <- cbind(1,X)
  Z.predict <- cbind(1,space)
  r <- quantile(X, probs = seq(0,1,1/(knots+1)))
  for (j in 2:(knots+1)) {
    Z <- cbind(Z,(X >= r[j])*(X - r[j]))
    Z.predict <- cbind(Z.predict,(space >= r[j])*(space - r[j]))
  }
  # construct estimator
  beta <- solve(t(Z) %*% Z) %*% t(Z) %*% Y
  predicted.vals <- Z.predict %*% beta
  return(predicted.vals)
}
