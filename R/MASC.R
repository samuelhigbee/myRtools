#' Matching and syntehtic control
#'
#' Estimates mixture of matching and synthetic control estimates
#' @param Y The outcome (post-treatment) variables for the control group
#' @param X The features on which to match, usually pre-treatment outcomes
#' @param space Space on which to predict, typically pre-treatment for treated group
#' @param K The number of matches to use
#' @param phi The mixture tuning parameter
#' @export
#'

MASC <- function(Y, X, space, K, phi) {
  ma.Y <- myRtools::matching(Y, X, space, K)
  sc.Y <- myRtools::synthetic.control(Y, X, space, penalty=0)
  pred.Y <- phi*ma.Y + (1-phi)*sc.Y
}



