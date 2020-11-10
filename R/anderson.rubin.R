#' Anderson-Rubin test
#'
#' Calculates an Anderson-Rubin test statistic and computes the test for a 
#' hypothesized value.
#' @param Y The outcome variables
#' @param X The endogenous variables
#' @param Z The instruments
#' @param b The hypothesized value
#' @param a The level of the test
#' @export

anderson.rubin <- function(Y, X, Z, b, a) {
  resid <- as.matrix(Y-as.matrix(X)%*%b)
  g <- myRtools::lin.reg(resid, Z)
  V <- myRtools::hc0.var(resid, Z, g)
  ar <- g %*% solve(V) %*% g
  test <- ar > qchisq(1-a,length(g))
  return(list(ar,test))
}