% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/assign_groups.R
\name{assign_groups}
\alias{assign_groups}
\title{Randomly assign groups}
\usage{
assign_groups(
  sample_size,
  sample_prob,
  prob0,
  prob1,
  seed,
  .rng_kind = NULL,
  .rng_normal_kind = NULL,
  .rng_sample_kind = NULL
)
}
\arguments{
\item{sample_size}{total number of people under observation.}

\item{sample_prob}{a vector of probability weights for obtaining the elements of the vector being sampled.}

\item{prob0}{vector probability of each possible outcome for the null group}

\item{prob1}{vector probability of each possible outcome for the intervention group}

\item{seed}{integer specifying the seed number}

\item{.rng_kind}{seeding info passed to withr::with_seed}

\item{.rng_normal_kind}{seeding info passed to withr::with_seed}

\item{.rng_sample_kind}{seeding info passed to withr::with_seed}
}
\value{
list of group assignments
}
\description{
(Brief description of the function here.)
}
