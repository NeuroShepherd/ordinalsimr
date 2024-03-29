% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/run_simulations.R
\name{run_simulations}
\alias{run_simulations}
\title{Run Simulations}
\usage{
run_simulations(sample_size, sample_prob, prob0, prob1, niter)
}
\arguments{
\item{sample_size}{Total number of trial participants}

\item{sample_prob}{a vector of probability weights for obtaining the elements of the vector being sampled.}

\item{prob0}{Vector of probabilities for control group}

\item{prob1}{Vector of probabilities for intervention group}

\item{niter}{Number of simulation iterations to complete#'}
}
\value{
list with elements `p_values` which is a matrix of p values for tests at each iteration, and `initial_groups` which is the group assignment information for each iteration
}
\description{
Run Simulations
}
