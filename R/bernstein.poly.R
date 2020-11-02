#' Bernstein polynomial estimation
#'
#' This performs Bernstein polynomial estimation on a given space
#' @param Y A vector of outcome variables
#' @param X A vector of predictors
#' @param K The order of the polynomial
#' @param space The values at which to predict
#' @export

bernstein.poly <- function(Y, X, K=1, space) {
  Z <- NULL
  Z.predict <- NULL
  # construct data for least squares estimation
  for (k in 0:K) {
    z <- (X - min(space))/(max(space) - min(space))
    b <- factorial(K)/(factorial(K-k) * factorial(k)) * z^k * (1-z)^(K-k)
    Z <- cbind(Z,b)
    z.predict <- (space - min(space))/(max(space) - min(space))
    b.predict <- factorial(K)/(factorial(K-k) * factorial(k)) *
      z.predict^k * (1-z.predict)^(K-k)
    Z.predict <- cbind(Z.predict,b.predict)
  }
  # construct estimator
  beta <- solve(t(Z) %*% Z) %*% t(Z) %*% Y
  predicted.vals <- Z.predict %*% beta
  return(predicted.vals)
}
