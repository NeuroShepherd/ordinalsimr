
# write tests for the ordinal_tests function

sample_size = 30
prob0 <- c(.4, .3, .3)
prob1 <- c(.9, .05, .05)
seed <- 10
sample_prob <- c(0.50, 0.50)

groups <- assign_groups(sample_size = sample_size,
              prob0 = prob0,
              prob1 = prob1,
              seed = 10,
              sample_prob = sample_prob)



test_that("ordinal_tests returns a vector", {
  suppressWarnings(
    expect_vector(ordinal_tests(groups[["x"]], groups[["y"]]))
  )
})

test_that("ordinal_tests returns a vector of length 3", {
  suppressWarnings(
    expect_length(ordinal_tests(groups[["x"]], groups[["y"]]), 6)
  )
})

test_that("ordinal_tests names the vector correctly", {
  suppressWarnings(
    expect_named(
      ordinal_tests(groups[["x"]], groups[["y"]]),
    c("Wilcoxon", "Fisher", "Chi Squared\n(No Correction)", "Chi Squared\n(Correction)", "Prop. Odds", "Coin Indep. Test")
  )
  )
})

