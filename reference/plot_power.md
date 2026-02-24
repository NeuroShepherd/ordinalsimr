# Plot Test Power

Plot Test Power

## Usage

``` r
plot_power(df, power_threshold = 0.8, ci_band = TRUE)
```

## Arguments

- df:

  a dataframe with p-values and a sample_size column

- power_threshold:

  numeric. desired power threshold

- ci_band:

  logical. whether to include a confidence interval band around the
  power estimate

## Value

ggplot object
