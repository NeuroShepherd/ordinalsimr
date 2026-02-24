# Randomly assign groups

(Brief description of the function here.)

## Usage

``` r
assign_groups(
  sample_size,
  sample_prob,
  prob0,
  prob1,
  seed,
  .rng_kind = NULL,
  .rng_normal_kind = NULL,
  .rng_sample_kind = NULL
)
```

## Arguments

- sample_size:

  total number of people under observation.

- sample_prob:

  a vector of probability weights for obtaining the elements of the
  vector being sampled.

- prob0:

  vector probability of each possible outcome for the null group

- prob1:

  vector probability of each possible outcome for the intervention group

- seed:

  integer specifying the seed number

- .rng_kind:

  seeding info passed to withr::with_seed

- .rng_normal_kind:

  seeding info passed to withr::with_seed

- .rng_sample_kind:

  seeding info passed to withr::with_seed

## Value

list of group assignments
