# Using {ordinalsimr}

``` r
library(ordinalsimr)
```

## Shiny App

The {ordinalsimr} package combines a Shiny interface with simulation
functions for two-group ordinal outcomes. In practice, the app is
designed to help users compare test performance under user-defined
assumptions and to summarize Type I error, Type II error, and power.

In the app, users can set:

- Number of simulation iterations
- One or more sample sizes
- Allocation between groups (for example, 1:1)
- Outcome category probabilities for each group
- Which statistical tests to include

The app also provides progress tracking for longer simulation runs,
optional Type I error runs for each group, plots, and downloadable
outputs.

## Data Generation Process

Data generation is handled by
[`assign_groups()`](https://neuroshepherd.github.io/ordinalsimr/reference/assign_groups.md)
and orchestrated in repeated runs by
[`run_simulations()`](https://neuroshepherd.github.io/ordinalsimr/reference/run_simulations.md).

At each iteration:

1.  [`assign_groups()`](https://neuroshepherd.github.io/ordinalsimr/reference/assign_groups.md)
    samples group membership (`y`) using the specified allocation
    probabilities.
2.  It then samples ordinal outcomes (`x`) within each group using
    `prob0` (group 0) and `prob1` (group 1).
3.  [`run_simulations()`](https://neuroshepherd.github.io/ordinalsimr/reference/run_simulations.md)
    repeats this process for each requested sample size and iteration
    count.

This design keeps the data-generating mechanism explicit and directly
tied to user-entered assumptions.

## Statistical Tests Used

For each simulated dataset,
[`ordinal_tests()`](https://neuroshepherd.github.io/ordinalsimr/reference/ordinal_tests.md)
computes p-values for the selected methods. By default, all implemented
methods are run:

- Wilcoxon rank-sum test
  ([`stats::wilcox.test`](https://rdrr.io/r/stats/wilcox.test.html))
- Fisher’s exact test (simulation-based p-value;
  [`stats::fisher.test`](https://rdrr.io/r/stats/fisher.test.html))
- Chi-squared test without continuity correction
  (`stats::chisq.test(correct = FALSE)`)
- Chi-squared test with continuity correction
  (`stats::chisq.test(correct = TRUE)`)
- Proportional odds model
  ([`rms::lrm`](https://rdrr.io/pkg/rms/man/lrm.html))
- Coin independence test
  ([`coin::independence_test`](https://rdrr.io/pkg/coin/man/IndependenceTest.html)
  to fit the test, then
  [`coin::pvalue`](https://rdrr.io/pkg/coin/man/pvalue-methods.html) to
  extract the p-value)

The test set can be restricted in both the app and function calls, which
is useful for targeted method comparisons.

These methods provide complementary views of group differences for
ordinal endpoints: rank-based tests focus on distributional shift,
contingency-table tests assess association between group and category
counts, and the proportional-odds model summarizes effects in an ordered
logistic framework.

Using them side-by-side is helpful in simulation studies because Type I
error and power can change with sample size, allocation imbalance, and
outcome distribution shape.

## Practical Script-Based Workflow

An explanation of how to use the core simulation functions in a
script-based workflow is provided in the [Coding
Simulations](https://neuroshepherd.github.io/ordinalsimr/articles/coding-simulations.html)
vignette. The same functions that power the app can be used in scripts
for more customized analyses, batch runs, or integration with other
workflows.

This workflow mirrors the app logic: generate group/outcome data
repeatedly, compute test p-values, then summarize results across
iterations.

## Package Architecture

The package is structured in three connected layers:

- **Simulation core:**
  [`assign_groups()`](https://neuroshepherd.github.io/ordinalsimr/reference/assign_groups.md),
  [`ordinal_tests()`](https://neuroshepherd.github.io/ordinalsimr/reference/ordinal_tests.md),
  and
  [`run_simulations()`](https://neuroshepherd.github.io/ordinalsimr/reference/run_simulations.md)
  define data generation, test evaluation, and iteration over simulation
  settings. The `run_simulations_in_background()` function wraps
  [`run_simulations()`](https://neuroshepherd.github.io/ordinalsimr/reference/run_simulations.md)
  to run simulation in background processes for the Shiny app.
- **Computation helpers:** functions such as
  [`calculate_power_t2error()`](https://neuroshepherd.github.io/ordinalsimr/reference/calculate_power_t2error.md)
  and
  [`calculate_t1_error()`](https://neuroshepherd.github.io/ordinalsimr/reference/calculate_t1_error.md)
  summarize simulation results.
- **Shiny modules:** the app server (`app_server`) wires together
  modular components for data entry, simulation triggers, background
  execution, progress updates, plotting, and export/report generation.

This separation helps keep methods transparent while allowing the app
and script-based workflows to use the same simulation engine.

Bug reports and feature requests can be submitted as issues at
<https://github.com/NeuroShepherd/ordinalsimr/issues>
