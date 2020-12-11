#' Matching estimation for multivariable independent and dependent variables
#'
#' Requires Gurobi optimizer
#' @param Y The outcome variables (can be multi-valued as in panel data)
#' @param X The explanatory variables
#' @param space The space on whic to predict, for example treated group's X
#' @param K Number of matches to use
#' @export

matching <- function(Y, X, space, K=1) {
  n.X <- as.matrix(X)
  n.Y <- as.matrix(Y)
  n.S <- as.matrix(space)
  S.mat <- matrix(n.S, nrow=nrow(n.X), ncol=ncol(n.S), byrow=TRUE)
  
  model <- list()
  model$modelsense <- "min"
  model$obj <- sqrt(rowSums((n.X - S.mat)^2) )
  model$A   <- matrix(rep(1,nrow(n.X)),nrow=1)
  model$rhs <- c(1)
  model$lb <- rep(0,nrow(n.X))
  model$ub <- rep(1/K,nrow(n.X))
  model$sense <- c("=")
  
  soln <- gurobi::gurobi(model)
  weights <- soln$x
  
  pred.Y <- t(weights)%*%cbind(n.X,n.Y)
  return(pred.Y)
}
