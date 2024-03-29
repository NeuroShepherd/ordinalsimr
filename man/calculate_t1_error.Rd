% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils.R
\name{calculate_t1_error}
\alias{calculate_t1_error}
\title{Calculate Type 1 Error}
\usage{
calculate_t1_error(
  df,
  alpha = 0.05,
  t1_error_confidence_int = 95,
  n = NA_real_
)
}
\arguments{
\item{df}{data frame}

\item{alpha}{significance level}

\item{t1_error_confidence_int}{confidence interval}

\item{n}{optional numeric input of}
}
\value{
data frame
}
\description{
Calculate Type 1 error for a distribution, and the confidence interval around this estimate. Type I error and confidence intervals calculated using `stats::binom.test()` which implements the Newcombe method.
}
