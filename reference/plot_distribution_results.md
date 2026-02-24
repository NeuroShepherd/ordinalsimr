# Plot Distribution

This function takes a wide table of p-values (i.e. one column for each
statistical test), converts it to long format, and creates a density
plot of the p-values by each test.

## Usage

``` r
plot_distribution_results(df, alpha = 0.05, outlier_removal = 0.1)
```

## Arguments

- df:

  data frame where each column is a set of p-values for a different
  statistical test

- alpha:

  numeric. significance level

- outlier_removal:

  numeric. set x-axis scale maximum by proportion

## Value

ggplot object
