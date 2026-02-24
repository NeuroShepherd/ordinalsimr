# Coding Simulations

``` r
library(ordinalsimr)
```

## Coding Your Own Simulations

This guide will provide a rough overview of how to code your own
simulations using the components of this package should you find the
Shiny application too limiting for your own purposes. Key information on
functions:

- [`run_simulations()`](https://neuroshepherd.github.io/ordinalsimr/reference/run_simulations.md)
  will take simulation input parameters and return a named list of
  tibbles. Each tibble represents the results of a simulation at a given
  sample size, with names e.g. “sample_size_32”.
- `bind_rows()` is used to combine the tibbles into a single tibble.
- [`calculate_power_t2error()`](https://neuroshepherd.github.io/ordinalsimr/reference/calculate_power_t2error.md)
  and
  [`calculate_t1_error()`](https://neuroshepherd.github.io/ordinalsimr/reference/calculate_t1_error.md)
  can receive the p-value data frames for performing T1 Error, Power,
  and T2 Error calculations with confidence intervals. See function
  documentation for additional arguments.

### Power and Type II Error

``` r
sim_results <- run_simulations(
  sample_size = 80,
  sample_prob = c(0.5, 0.5),
  prob0 = c(0.1, 0.2, 0.3, 0.4),
  prob1 = c(0.6, 0.2, 0.1, 0.1),
  niter = 20
)

formatted_results <- dplyr::bind_rows(sim_results)
names(formatted_results)
#>  [1] "Wilcoxon"                    "Fisher"                     
#>  [3] "Chi Squared (No Correction)" "Chi Squared (Correction)"   
#>  [5] "Prop. Odds"                  "Coin Indep. Test"           
#>  [7] "run"                         "y"                          
#>  [9] "x"                           "n_null"                     
#> [11] "n_intervene"                 "sample_size"                
#> [13] "K"

formatted_results %>%
  dplyr::select(
    Wilcoxon, Fisher, `Chi Squared (No Correction)`,
    `Chi Squared (Correction)`, `Prop. Odds`,
    `Coin Indep. Test`,
    sample_size
  ) %>%
  calculate_power_t2error(alpha = 0.05, power_confidence_int = 95)
#> # A tibble: 6 × 10
#> # Groups:   Sample Size [1]
#>   `Sample Size` test    lower_power_bound upper_power_bound power `Power 95% CI`
#>           <dbl> <chr>               <dbl>             <dbl> <dbl> <chr>         
#> 1            80 Wilcox…             0.832                 1     1 [0.832, 1]    
#> 2            80 Fisher              0.832                 1     1 [0.832, 1]    
#> 3            80 Chi Sq…             0.832                 1     1 [0.832, 1]    
#> 4            80 Chi Sq…             0.832                 1     1 [0.832, 1]    
#> 5            80 Prop. …             0.832                 1     1 [0.832, 1]    
#> 6            80 Coin I…             0.832                 1     1 [0.832, 1]    
#> # ℹ 4 more variables: lower_t2error_bound <dbl>, upper_t2error_bound <dbl>,
#> #   t2_error <dbl>, `TII Error 95% CI` <chr>
```

### Type I Error

To find the Type I error of a distribution, the code from before is
largely unchanged except for the fact that the probability vectors set
in `run_simulations` must now be equivalent and the
[`calculate_t1_error()`](https://neuroshepherd.github.io/ordinalsimr/reference/calculate_t1_error.md)
function is now applied.

``` r
sim_results <- run_simulations(
  sample_size = 30:35,
  sample_prob = c(0.5, 0.5),
  prob0 = c(.4, .3, .3),
  prob1 = c(.8, .1, .1), # note the matching probabilities between groups
  niter = 50
)

formatted_results <- dplyr::bind_rows(sim_results)
names(formatted_results)
#>  [1] "Wilcoxon"                    "Fisher"                     
#>  [3] "Chi Squared (No Correction)" "Chi Squared (Correction)"   
#>  [5] "Prop. Odds"                  "Coin Indep. Test"           
#>  [7] "run"                         "y"                          
#>  [9] "x"                           "n_null"                     
#> [11] "n_intervene"                 "sample_size"                
#> [13] "K"


formatted_results %>%
  dplyr::select(
    Wilcoxon, Fisher, `Chi Squared (No Correction)`,
    `Chi Squared (Correction)`, `Prop. Odds`,
    `Coin Indep. Test`,
    sample_size
  ) %>%
  calculate_t1_error(alpha = 0.05, t1_error_confidence_int = 95)
#> # A tibble: 36 × 6
#> # Groups:   Sample Size [6]
#>    `Sample Size` test            lower_t1_bound upper_t1_bound t1_error `95% CI`
#>            <int> <chr>                    <dbl>          <dbl>    <dbl> <chr>   
#>  1            30 Wilcoxon                 0.452          0.736     0.6  [0.452,…
#>  2            30 Fisher                   0.355          0.645     0.5  [0.355,…
#>  3            30 Chi Squared (N…          0.374          0.663     0.52 [0.374,…
#>  4            30 Chi Squared (C…          0.374          0.663     0.52 [0.374,…
#>  5            30 Prop. Odds               0.472          0.753     0.62 [0.472,…
#>  6            30 Coin Indep. Te…          0.472          0.753     0.62 [0.472,…
#>  7            31 Wilcoxon                 0.512          0.788     0.66 [0.512,…
#>  8            31 Fisher                   0.337          0.626     0.48 [0.337,…
#>  9            31 Chi Squared (N…          0.374          0.663     0.52 [0.374,…
#> 10            31 Chi Squared (C…          0.374          0.663     0.52 [0.374,…
#> # ℹ 26 more rows
```

### Mapping Over Many Sample Sizes

The current version of the Shiny application can only accept a range of
sample sizes. However, you may find it useful to iterate over a discrete
set of sample sizes and calculate the Power and Type II error for each
rather than a range. This can save time and computation power when you
are only interested in a few specific sample sizes.

Depending on how big the number of iterations per sample sizes (`niter`)
and the actual number of sample sizes being checked, it may only be
practical to do this in a parallelized manner with e.g. {furrr} or
{parallel}. In any case, an example of such code is included below:

``` r
# Create a vector of sample sizes
sample_sizes <- c(30, 50, 100)

# Map over the sample sizes
lapply(sample_sizes, function(x) {
  run_simulations(
    sample_size = x,
    sample_prob = c(0.5, 0.5),
    prob0 = c(0.1, 0.2, 0.3, 0.4),
    prob1 = c(0.6, 0.2, 0.1, 0.1),
    niter = 100
  ) %>%
    dplyr::bind_rows() %>%
    dplyr::select(
      Wilcoxon, Fisher, `Chi Squared (No Correction)`,
      `Chi Squared (Correction)`, `Prop. Odds`,
      `Coin Indep. Test`, sample_size
    ) %>%
    calculate_power_t2error()
})
#> [[1]]
#> # A tibble: 6 × 10
#> # Groups:   Sample Size [1]
#>   `Sample Size` test    lower_power_bound upper_power_bound power `Power 95% CI`
#>           <dbl> <chr>               <dbl>             <dbl> <dbl> <chr>         
#> 1            30 Wilcox…             0.765             0.914  0.85 [0.765, 0.914]
#> 2            30 Fisher              0.643             0.823  0.74 [0.643, 0.823]
#> 3            30 Chi Sq…             0.600             0.788  0.7  [0.6, 0.788]  
#> 4            30 Chi Sq…             0.600             0.788  0.7  [0.6, 0.788]  
#> 5            30 Prop. …             0.788             0.929  0.87 [0.788, 0.929]
#> 6            30 Coin I…             0.765             0.914  0.85 [0.765, 0.914]
#> # ℹ 4 more variables: lower_t2error_bound <dbl>, upper_t2error_bound <dbl>,
#> #   t2_error <dbl>, `TII Error 95% CI` <chr>
#> 
#> [[2]]
#> # A tibble: 6 × 10
#> # Groups:   Sample Size [1]
#>   `Sample Size` test    lower_power_bound upper_power_bound power `Power 95% CI`
#>           <dbl> <chr>               <dbl>             <dbl> <dbl> <chr>         
#> 1            50 Wilcox…             0.930             0.998  0.98 [0.93, 0.998] 
#> 2            50 Fisher              0.887             0.984  0.95 [0.887, 0.984]
#> 3            50 Chi Sq…             0.874             0.978  0.94 [0.874, 0.978]
#> 4            50 Chi Sq…             0.874             0.978  0.94 [0.874, 0.978]
#> 5            50 Prop. …             0.930             0.998  0.98 [0.93, 0.998] 
#> 6            50 Coin I…             0.930             0.998  0.98 [0.93, 0.998] 
#> # ℹ 4 more variables: lower_t2error_bound <dbl>, upper_t2error_bound <dbl>,
#> #   t2_error <dbl>, `TII Error 95% CI` <chr>
#> 
#> [[3]]
#> # A tibble: 6 × 10
#> # Groups:   Sample Size [1]
#>   `Sample Size` test    lower_power_bound upper_power_bound power `Power 95% CI`
#>           <dbl> <chr>               <dbl>             <dbl> <dbl> <chr>         
#> 1           100 Wilcox…             0.964                 1     1 [0.964, 1]    
#> 2           100 Fisher              0.964                 1     1 [0.964, 1]    
#> 3           100 Chi Sq…             0.964                 1     1 [0.964, 1]    
#> 4           100 Chi Sq…             0.964                 1     1 [0.964, 1]    
#> 5           100 Prop. …             0.964                 1     1 [0.964, 1]    
#> 6           100 Coin I…             0.964                 1     1 [0.964, 1]    
#> # ℹ 4 more variables: lower_t2error_bound <dbl>, upper_t2error_bound <dbl>,
#> #   t2_error <dbl>, `TII Error 95% CI` <chr>
```

And if you’re more of a {purrr} person:

``` r
sample_sizes %>%
  purrr::map(
    ~run_simulations(
      sample_size = .x,
      sample_prob = c(0.5, 0.5),
      prob0 = c(0.1, 0.2, 0.3, 0.4),
      prob1 = c(0.6, 0.2, 0.1, 0.1),
      niter = 100
      ) %>%
      bind_rows() %>%
      select(Wilcoxon, Fisher, `Chi Squared\n(No Correction)`, 
           `Chi Squared\n(Correction)`, `Prop. Odds`, 
           `Coin Indep. Test`, sample_size) %>%
      calculate_power_t2error()
)
```

### Adjust Multiple Parameters

It is perhaps more likely that analysts will want to iterate simulations
over a variety of different parameters at once. The code below provides
a structure for creating a combination grid based on the 5 input
parameters that can be altered; this example can easily be altered to
include desired parameters by replacing/removing/expanding the listed
parameters.

#### Set Parameters

Note that the `prob0_list` and `prob1_list` must always be of the same
length **and** the corresponding sub-elements of the list must also be
of the same length. Put in terms of the application, there must be a
Group 2 if there is a Group 1, and the vector representing the number of
possible outcomes must be the same length for these 2 groups.

If performing simulations on one distribution to evaluate Type I error,
it is only necessary to form one probability list. This object can be
recycled for the probabilities of both groups.

``` r
# Choose sample sizes
sample_size <- c(50, 100)
# Set sample distributions as a proportion c(group1, group2)
sample_prob <- list(c(0.5, 0.5), c(0.75, 0.25))
# Trial 1 has matching probabilities between the 2 groups. Trial 2 has non-matching probabilities
prob0_list <- list(trial1_group1 = c(0.1, 0.2, 0.3, 0.4), trial2_group1 = c(0.1, 0.2, 0.3, 0.4))
prob1_list <- list(trial1_group2 = c(0.1, 0.2, 0.3, 0.4), trial2_group2 = c(0.2, 0.3, 0.3, 0.2))
# Number of iterations
niter <- c(20, 100)
```

#### Create Simulation Grid

Use
[`tidyr::expand_grid()`](https://tidyr.tidyverse.org/reference/expand_grid.html)
rather than `base:expand.grid()` because the former creates a tibble by
default, and this supports the nested tibble structure which I’m relying
on. (This can, of course, be approached in other ways if desired.)

``` r
# Use tidyr::expand_grid as it creates a tibble, supporting the nested tibble structure
info_table <- tidyr::expand_grid(
  sample_size,
  sample_prob,
  prob0_list,
  prob1_list,
  niter
)

info_table
#> # A tibble: 32 × 5
#>    sample_size sample_prob prob0_list   prob1_list   niter
#>          <dbl> <list>      <named list> <named list> <dbl>
#>  1          50 <dbl [2]>   <dbl [4]>    <dbl [4]>       20
#>  2          50 <dbl [2]>   <dbl [4]>    <dbl [4]>      100
#>  3          50 <dbl [2]>   <dbl [4]>    <dbl [4]>       20
#>  4          50 <dbl [2]>   <dbl [4]>    <dbl [4]>      100
#>  5          50 <dbl [2]>   <dbl [4]>    <dbl [4]>       20
#>  6          50 <dbl [2]>   <dbl [4]>    <dbl [4]>      100
#>  7          50 <dbl [2]>   <dbl [4]>    <dbl [4]>       20
#>  8          50 <dbl [2]>   <dbl [4]>    <dbl [4]>      100
#>  9          50 <dbl [2]>   <dbl [4]>    <dbl [4]>       20
#> 10          50 <dbl [2]>   <dbl [4]>    <dbl [4]>      100
#> # ℹ 22 more rows
```

#### Run Simulation

The example below is complete in running the simulations and calculating
the Power and Type II error. However, the same code can be applied to
either calculate the Type I error or use the p-values for other
purposes.

``` r
# Calculate either Power/T2 error or T1 error depending on your specific needs
many_sims <- mapply(
  ordinalsimr::run_simulations,
  sample_size = info_table$sample_size,
  sample_prob = info_table$sample_prob,
  prob0 = info_table$prob0_list,
  prob1 = info_table$prob1_list,
  niter = info_table$niter
)


length(many_sims)
#> [1] 32
many_sims[1]
#> $sample_size_50
#> # A tibble: 20 × 13
#>    Wilcoxon Fisher Chi Squared (No Correct…¹ Chi Squared (Correct…² `Prop. Odds`
#>       <dbl>  <dbl>                     <dbl>                  <dbl>        <dbl>
#>  1  0.585   0.788                     0.725                  0.725       0.572  
#>  2  0.967   0.796                     0.768                  0.768       0.958  
#>  3  0.719   0.454                     0.422                  0.422       0.707  
#>  4  0.818   0.453                     0.407                  0.407       0.802  
#>  5  0.205   0.308                     0.297                  0.297       0.193  
#>  6  0.346   0.446                     0.419                  0.419       0.340  
#>  7  0.626   0.490                     0.418                  0.418       0.617  
#>  8  0.852   0.311                     0.302                  0.302       0.841  
#>  9  0.859   0.946                     0.891                  0.891       0.849  
#> 10  0.857   0.824                     0.808                  0.808       0.848  
#> 11  0.637   0.844                     0.798                  0.798       0.629  
#> 12  0.331   0.519                     0.485                  0.485       0.323  
#> 13  0.00447 0.0370                    0.0330                 0.0330      0.00317
#> 14  0.777   0.896                     0.823                  0.823       0.765  
#> 15  0.301   0.0455                    0.0481                 0.0481      0.287  
#> 16  0.647   0.497                     0.447                  0.447       0.634  
#> 17  0.0548  0.149                     0.152                  0.152       0.0486 
#> 18  0.271   0.312                     0.292                  0.292       0.253  
#> 19  0.483   0.888                     0.820                  0.820       0.473  
#> 20  0.332   0.181                     0.189                  0.189       0.315  
#> # ℹ abbreviated names: ¹​`Chi Squared (No Correction)`,
#> #   ²​`Chi Squared (Correction)`
#> # ℹ 8 more variables: `Coin Indep. Test` <dbl>, run <int>, y <list>, x <list>,
#> #   n_null <int>, n_intervene <dbl>, sample_size <dbl>, K <int>
```

And again for the {purrr} folks:

``` r
info_table %>%
  purrr::pmap(
    ~ run_simulations(
      sample_size = ..1,
      sample_prob = ..2,
      prob0 = ..3,
      prob1 = ..4,
      niter = ..5
    ) %>%
      bind_rows() %>%
      magrittr::extract2("p_values") %>%
      calculate_power_t2error(),
    .progress = TRUE
  )
```

Note that even with relatively small sample sizes and a number of
iterations 1-3 magnitudes less than normally specified for simulation
studies, processing 32 different simulations took ~20-30 seconds.
