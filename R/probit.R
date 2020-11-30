#' Probit estimation
#'
#' Estimates probit parameters and fitted values
#' @param Y The outcome variables
#' @param X The explanatory variables
#' NOTE: Add variance later
#' @export

loglik <- function(beta, Y, X) {
  sum( Y*log(stats::pnorm(X%*%beta)) + (1-Y)*log(1-stats::pnorm(X%*%beta)))
}

probit <- function(Y, X, const=TRUE) {
  n.X <- as.matrix(X)
  n.Y <- as.matrix(Y)
  if (const==TRUE) {
    n.X <- cbind(1,n.X)
  }
  minfunc <- function(beta){
    -loglik(beta,n.Y,n.X)
  }
  opt <- stats::optim(rep(0,ncol(n.X)),minfunc)
  fit <- stats::pnorm(n.X%*%opt$par)
  return(list(beta=opt$par,fit=fit))
}


