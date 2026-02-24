# Calculate Type 1 Error

Calculate Type 1 error for a distribution, and the confidence interval
around this estimate. Type I error and confidence intervals calculated
using \`stats::binom.test()\` which implements the Newcombe method.

## Usage

``` r
calculate_t1_error(
  df,
  alpha = 0.05,
  t1_error_confidence_int = 95,
  n = NA_real_
)
```

## Arguments

- df:

  data frame

- alpha:

  significance level

- t1_error_confidence_int:

  confidence interval

- n:

  optional numeric input of

## Value

data frame
