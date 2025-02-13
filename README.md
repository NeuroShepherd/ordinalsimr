
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
[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.14697215.svg)](https://doi.org/10.5281/zenodo.14697215)
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

You can install the development version of ordinalsimr from GitHub with:

``` r
# install.packages("devtools")
devtools::install_github(
  "NeuroShepherd/ordinalsimr",
  build_vignettes = TRUE
)
```

## Running the App

The app can be started with the following code:

``` r
ordinalsimr::run_app()
```

If using the app repeatedly, it may be useful to change some of the
options in the application to suit your needs. See the vignette
“ordinalsimr-options” for more information,
`vignette("ordinalsimr-options", package = "ordinalsimr")`.

## Recommendations

Simulations with 1000s of iterations *will* take minutes to hours to
run. This should generally be ok on the Shiny app, but if you encounter
issues, consider running the simulations in a separate R session using
the functions provided in this package (rather than the Shiny app). See
the vignette “coding-simulations” for more information,
`vignette("coding-simulations", package = "ordinalsimr")`.
