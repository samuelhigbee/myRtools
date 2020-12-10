#' Classical homoskedastic variance estimation
#'
#' @param Y The Y data used in estimation
#' @param X The X data used in estimation
#' @param beta The vector of linear regression estimates
#' @param const If TRUE, adds a constant column to the data
#' @export

classic.var <- function(Y, X, beta, const=TRUE) {
  n.X <- as.matrix(X)
  n.Y <- as.matrix(Y)
  if (const==TRUE) {
    n.X <- cbind(1,n.X)
  }
  resid <- as.vector(n.Y - n.X %*% beta)
  sigma.2 <- mean(resid^2)
  var <- solve(t(n.X)%*%n.X) * sigma.2
  return(var)
}
