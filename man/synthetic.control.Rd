% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/synthetic.control.R
\name{synthetic.control}
\alias{synthetic.control}
\title{Synthetic control estimation}
\usage{
synthetic.control(Y, X, space, penalty = 0)
}
\arguments{
\item{Y}{The outcome (post-treatment) variables for the control group}

\item{X}{The features on which to match, usually pre-treatment outcomes}

\item{space}{Space on which to predict, typically pre-treatment for treated group}

\item{penalty}{The penalty for penalized synthetic control. Defaults to 0}
}
\description{
Estimates synthetic control predicted values.  Gurobi optimizer required.
}
