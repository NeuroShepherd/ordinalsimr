# Run Simulations

Run Simulations

## Usage

``` r
run_simulations(
  sample_size,
  sample_prob,
  prob0,
  prob1,
  niter,
  included = "all",
  .rng_kind = NULL,
  .rng_normal_kind = NULL,
  .rng_sample_kind = NULL
)
```

## Arguments

- sample_size:

  Total number of trial participants

- sample_prob:

  a vector of probability weights for obtaining the elements of the
  vector being sampled.

- prob0:

  Vector of probabilities for control group

- prob1:

  Vector of probabilities for intervention group

- niter:

  Number of simulation iterations to complete#'

- included:

  a character vector of the tests to be included. Default is "all"

- .rng_kind:

  seeding info passed to withr::with_seed

- .rng_normal_kind:

  seeding info passed to withr::with_seed

- .rng_sample_kind:

  seeding info passed to withr::with_seed

## Value

a list of lists; sub-list elements include \`p_values\` which is a
matrix of p values for tests at each iteration, and \`initial_groups\`
which is the group assignment information for each iteration

## Examples

``` r
run_simulations(
  sample_size = c(40, 50, 60),
  sample_prob = c(0.5, 0.5),
  prob0 = c(0.1, 0.2, 0.3, 0.4),
  prob1 = c(0.6, 0.2, 0.1, 0.1),
  niter = 40
)
#> $sample_size_40
#> # A tibble: 40 × 13
#>     Wilcoxon   Fisher Chi Squared (No Corr…¹ Chi Squared (Correct…² `Prop. Odds`
#>        <dbl>    <dbl>                  <dbl>                  <dbl>        <dbl>
#>  1   6.95e-4 0.001000              0.00134                0.00134   0.000310    
#>  2   2.50e-3 0.00700               0.0107                 0.0107    0.00157     
#>  3   6.18e-4 0.00450               0.00684                0.00684   0.000278    
#>  4   5.96e-3 0.0275                0.0231                 0.0231    0.00355     
#>  5   3.07e-1 0.495                 0.512                  0.512     0.297       
#>  6   4.63e-2 0.140                 0.136                  0.136     0.0364      
#>  7   4.86e-4 0.00200               0.00540                0.00540   0.000229    
#>  8   7.24e-3 0.0295                0.0315                 0.0315    0.00507     
#>  9   4.20e-3 0.0230                0.0235                 0.0235    0.00261     
#> 10   1.69e-6 0.000500              0.0000144              0.0000144 0.0000000743
#> # ℹ 30 more rows
#> # ℹ abbreviated names: ¹​`Chi Squared (No Correction)`,
#> #   ²​`Chi Squared (Correction)`
#> # ℹ 8 more variables: `Coin Indep. Test` <dbl>, run <int>, y <list>, x <list>,
#> #   n_null <int>, n_intervene <dbl>, sample_size <dbl>, K <int>
#> 
#> $sample_size_50
#> # A tibble: 40 × 13
#>      Wilcoxon  Fisher Chi Squared (No Corr…¹ Chi Squared (Correct…² `Prop. Odds`
#>         <dbl>   <dbl>                  <dbl>                  <dbl>        <dbl>
#>  1    4.73e-6 5.00e-4             0.0000592              0.0000592       4.87e-7
#>  2    7.29e-6 5.00e-4             0.0000585              0.0000585       1.42e-6
#>  3    1.54e-3 7.50e-3             0.00734                0.00734         1.04e-3
#>  4    1.67e-3 5.00e-4             0.000481               0.000481        5.13e-4
#>  5    7.58e-3 3.55e-2             0.0501                 0.0501          5.95e-3
#>  6    2.09e-5 5.00e-4             0.000113               0.000113        3.03e-6
#>  7    4.61e-6 5.00e-4             0.0000425              0.0000425       7.76e-7
#>  8    2.69e-5 5.00e-4             0.000328               0.000328        6.86e-6
#>  9    2.78e-5 5.00e-4             0.000262               0.000262        8.94e-6
#> 10    1.46e-7 5.00e-4             0.00000138             0.00000138      4.25e-9
#> # ℹ 30 more rows
#> # ℹ abbreviated names: ¹​`Chi Squared (No Correction)`,
#> #   ²​`Chi Squared (Correction)`
#> # ℹ 8 more variables: `Coin Indep. Test` <dbl>, run <int>, y <list>, x <list>,
#> #   n_null <int>, n_intervene <dbl>, sample_size <dbl>, K <int>
#> 
#> $sample_size_60
#> # A tibble: 40 × 13
#>     Wilcoxon   Fisher Chi Squared (No Corr…¹ Chi Squared (Correct…² `Prop. Odds`
#>        <dbl>    <dbl>                  <dbl>                  <dbl>        <dbl>
#>  1   1.74e-5 0.000500            0.000238               0.000238    0.00000511  
#>  2   1.37e-6 0.000500            0.00000118             0.00000118  0.000000256 
#>  3   4.37e-5 0.000500            0.000474               0.000474    0.0000191   
#>  4   6.83e-5 0.00150             0.000460               0.000460    0.0000232   
#>  5   1.19e-4 0.00200             0.00109                0.00109     0.0000631   
#>  6   1.10e-5 0.001000            0.000104               0.000104    0.00000243  
#>  7   2.77e-7 0.000500            0.00000160             0.00000160  0.0000000174
#>  8   1.41e-5 0.000500            0.000214               0.000214    0.00000392  
#>  9   2.09e-7 0.000500            0.000000823            0.000000823 0.0000000137
#> 10   5.22e-7 0.000500            0.00000421             0.00000421  0.0000000516
#> # ℹ 30 more rows
#> # ℹ abbreviated names: ¹​`Chi Squared (No Correction)`,
#> #   ²​`Chi Squared (Correction)`
#> # ℹ 8 more variables: `Coin Indep. Test` <dbl>, run <int>, y <list>, x <list>,
#> #   n_null <int>, n_intervene <dbl>, sample_size <dbl>, K <int>
#> 
```
