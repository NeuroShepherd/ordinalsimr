% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ordinal_tests.R
\name{ordinal_tests}
\alias{ordinal_tests}
\title{Ordinal outcome tests}
\usage{
ordinal_tests(x, y, included = "all", ...)
}
\arguments{
\item{x}{Group one}

\item{y}{Group two}

\item{included}{a character vector of the tests to be included. Default is "all"}

\item{...}{Placeholder for additional arguments to functions}
}
\value{
A named matrix of probabilities for each test

The function is designed to run all 6 tests by default. If you want to run only a subset of the tests, you can specify them in the `included` argument. The following values are possible:

\itemize{
\item{"Wilcoxon"}
\item{"Fisher"}
\item{"Chi Squared (No Correction)"}
\item{"Chi Squared (Correction)"}
\item{"Prop. Odds"}
\item{"Coin Indep. Test"}
}

This option is primarily for use in the Shiny application.
}
\description{
A metafunction that runs the statistical tests listed below, and returns the p-values as a named vector.
}
\details{
\itemize{
 \item{stats::wilcox.test()}
 \item{stats::fisher.test(simulate.p.value = TRUE)}
 \item{stats::chisq.test(correct = FALSE)}
 \item{stats::chisq.test(correct = TRUE)}
 \item{rms::lrm()}
 \item{coin::independence_test(ytrafo = coin::rank_trafo)}
 }
}
