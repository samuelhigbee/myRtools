#' Cluster corrected covariance matrix
#'
#' Computes the cluster corrected covariance matrix of estimates
#' @param Y The Y data used in estimation
#' @param X The X data used in estimation
#' @param beta The vector of linear regression estimates
#' @param groupvar A vector of group categorical variables assigning each
#' element of Y to a group
#' @export

cluster.var <- function(Y, X, beta, groupvar) {
  groups <- unique(groupvar)
  k <- ncol(as.matrix(X))
  denom <- matrix(0,nrow=k,ncol=k)
  num <- matrix(0,nrow=k,ncol=k)
  for (g in 1:length(groups)) {
    X.g <- as.matrix(X)[which(groupvar==groups[g]),]
    Y.g <- Y[which(groupvar==groups[g])]
    resid.g <- Y.g - X.g %*% beta
    denom <- denom + t(X.g) %*% X.g
    num <- num + t(X.g) %*% resid.g %*% t(resid.g) %*% X.g
  }
  return(solve(denom) %*% num %*% solve(denom))
}
