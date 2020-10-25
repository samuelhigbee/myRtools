#' Nearest neighbors in one dimension
#'
#' This performs nearest neighbors estimation in one dimension. For higher
#' dimensional analysis, this can be the propensity score.
#' @param Y A vector of outcomes
#' @param X A vector of predictors
#' @param k The number of neighbors to use
#' @param space The values at which to predict

nearest.neighbors.1d <- function(Y, X, k=1, space) {
  predicted.vals <- rep(NA,length(space))
  for (i in 1:length(space)) {
    dist <- abs(X - space[i])
    data <- cbind(Y, X, dist)
    predicted.vals[i] <- mean(data[order(data[,"dist"])[1:k],"Y"])
  }
  return(predicted.vals)
}
