# Calculate Hypothesis Test Parameters

This function calculates the power, Type II error, and Type I error of
tests given p-values. Power, Type II error, and confidence intervals
calculated using \`stats::binom.test()\` which implements the Newcombe
method.

## Usage

``` r
calculate_power_t2error(
  df,
  alpha = 0.05,
  power_confidence_int = 95,
  n = NA_real_
)
```

## Arguments

- df:

  Data frame where each column is a vector of p-values from a
  statistical test

- alpha:

  Numeric significance level; defaults to 0.05

- power_confidence_int:

  confidence interval

- n:

  Numeric value of sample size; repeated for convenience

## Value

A data frame with columns for Type 1 error, Type 2 error, and power as
well as rows for each test
