#' Synthetic control estimation
#'
#' Estimates synthetic control predicted values.  Gurobi optimizer required.
#' @param Y The outcome (post-treatment) variables for the control group
#' @param X The features on which to match, usually pre-treatment outcomes
#' @param space Space on which to predict, typically pre-treatment for treated group
#' @param penalty The penalty for penalized synthetic control. Defaults to 0
#' @export
#'

synthetic.control <- function(Y, X, space, penalty=0) {
  n.X <- as.matrix(X)
  n.Y <- as.matrix(Y)
  n.S <- as.matrix(space)
  S.mat <- matrix(n.S, nrow=nrow(n.X), ncol=ncol(n.S), byrow=TRUE)

  obj <- (1-penalty)*( -2* n.X %*% t(n.S)   ) +
    penalty*( rowSums((n.X - S.mat)^2)  )
  Q   <- (1-penalty)* n.X %*% t(n.X)

  model <- list()
  model$modelsense <- "min"
  model$obj <- obj
  model$Q   <- Q
  model$A   <- matrix(rep(1,nrow(n.X)),nrow=1)
  model$rhs <- c(1)
  model$lb <- rep(0,nrow(n.X))
  model$ub <- rep(1,nrow(n.X))
  model$sense <- c("=")

  soln <- gurobi::gurobi(model)
  weights <- soln$x

  pred.Y <- t(weights)%*%cbind(n.X,n.Y)
  return(pred.Y)
}
