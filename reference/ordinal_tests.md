# Ordinal outcome tests

A metafunction that runs the statistical tests listed below, and returns
the p-values as a named vector.

## Usage

``` r
ordinal_tests(x, y, included = "all", ...)
```

## Arguments

- x:

  Group one

- y:

  Group two

- included:

  a character vector of the tests to be included. Default is "all"

- ...:

  Placeholder for additional arguments to functions

## Value

A named matrix of probabilities for each test

The function is designed to run all 6 tests by default. If you want to
run only a subset of the tests, you can specify them in the \`included\`
argument. The following values are possible:

- "Wilcoxon"

- "Fisher"

- "Chi Squared (No Correction)"

- "Chi Squared (Correction)"

- "Prop. Odds"

- "Coin Indep. Test"

This option is primarily for use in the Shiny application.

## Details

- stats::wilcox.test()

- stats::fisher.test(simulate.p.value = TRUE)

- stats::chisq.test(correct = FALSE)

- stats::chisq.test(correct = TRUE)

- rms::lrm()

- coin::independence_test(ytrafo = coin::rank_trafo)
