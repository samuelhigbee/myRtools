% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/wls.R
\name{wls}
\alias{wls}
\title{Weighted leas squares regression}
\usage{
wls(Y, X, W)
}
\arguments{
\item{Y}{The outcome variables}

\item{X}{The explanatory variables}

\item{W}{The weighting matrix}
}
\description{
Estimates linear regression coefficients. Standard errors should be computed
with a different function depending on various assumptions.
}
