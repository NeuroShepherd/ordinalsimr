
<!-- README.md is generated from README.Rmd. Please edit that file -->

# ordinalsimr

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![R-CMD-check](https://github.com/NeuroShepherd/ordinalsimr/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/NeuroShepherd/ordinalsimr/actions/workflows/R-CMD-check.yaml)
[![CRAN
status](https://www.r-pkg.org/badges/version/ordinalsimr)](https://CRAN.R-project.org/package=ordinalsimr)
[![Codecov test
coverage](https://codecov.io/gh/NeuroShepherd/ordinalsimr/branch/master/graph/badge.svg)](https://app.codecov.io/gh/NeuroShepherd/ordinalsimr?branch=master)
<!-- badges: end -->

The {ordinalsimr} package assists in constructing simulation studies of
ordinal data comparing two groups. It is intended to facilitate
translation of methodological advances into practical settings for
e.g. applied statisticians and data analysts who want to determine an
appropriate statistical test to apply on their data or a proposed
distribution of data.

This package is primarily developed as a Shiny application which
abstracts away the heavier coding aspect of setting up simulation
studies. Instead, users can simply enter parameters and data
distributions into the application, and save the results as an `.rds`
file. The structure of the Shiny application only allows for one
simulation to be specified at a time as opposed to a grid of parameters.
However, the underlying functions for running the simulations are
accessible. See `vignette("ordinalsimr")` for template code on setting
up your own simulations manually.

## Installation

You can install the development version of ordinalsimr from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github(
  "NeuroShepherd/ordinalsimr",
  force = TRUE,
  build_vignettes = TRUE
)
```

## Recommendations

The application is available at \[link\], but may be down due to account
usage limitations on ShinyApps.io. There is not currently a plan to
increase the usage limits so it is *strongly recommended that you run
the application locally*.

Informative progressive bars have not been implemented in the Shiny
application, but a simulation that fails to run will almost always fail
at the beginning rather than in the middle of the run. Simulations with
1000s of iterations *will* take minutes to hours to run.
