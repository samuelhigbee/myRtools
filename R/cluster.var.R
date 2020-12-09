#' Cluster corrected covariance matrix
#'
#' Computes the cluster corrected covariance matrix of estimates
#' @param Y The Y data used in estimation
#' @param X The X data used in estimation
#' @param beta The vector of linear regression estimates
#' @param groupvar A vector of group categorical variables assigning each
#' element of Y to a group
#' @param const If TRUE, adds a constant column to the data
#' @export

cluster.var <- function(Y, X, beta, groupvar, const=TRUE) {
  groups <- unique(groupvar)
  G <- length(groups)
  n.X <- as.matrix(X)
  n.Y <- as.matrix(Y)
  if (const==TRUE) {
    n.X <- cbind(1,n.X)
  }
  k <- ncol(n.X)
  N <- nrow(n.X)
  denom <- matrix(0,nrow=k,ncol=k)
  num <- matrix(0,nrow=k,ncol=k)
  for (g in 1:G) {
    X.g <- as.matrix(n.X)[which(groupvar==groups[g]),]
    Y.g <- n.Y[which(groupvar==groups[g]),]
    resid.g <- Y.g - X.g %*% beta
    denom <- denom + t(X.g) %*% X.g
    num <- num + t(X.g) %*% resid.g %*% t(resid.g) %*% X.g
  }
  correction <- (G/(G-1)) * ((N-1)/(N-k))
  return(correction * solve(denom) %*% num %*% solve(denom))
}
