#' Probit estimation
#'
#' Estimates probit parameters and fitted values
#' @param Y The outcome variables
#' @param X The explanatory variables
#' 
#' NOTE: Add variance later. documentation is funny
#' @export

probit <- function(Y, X, const=TRUE) {
  n.X <- as.matrix(X)
  n.Y <- as.matrix(Y)
  if (const==TRUE) {
    n.X <- cbind(1,n.X)
  }
  minfunc <- function(beta){
    loglik <- sum( n.Y*log(stats::pnorm(n.X%*%beta)) + 
                     (1-n.Y)*log(1-stats::pnorm(n.X%*%beta)))
    return(-loglik)
  }
  opt <- stats::optim(rep(0,ncol(n.X)),minfunc,method="BFGS")
  fit <- stats::pnorm(n.X%*%opt$par)
  return(list(beta=opt$par,fit=fit,opt=opt))
}
