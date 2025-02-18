# write tests for the ordinal_tests function

sample_size <- 30
prob0 <- c(.4, .3, .3)
prob1 <- c(.9, .05, .05)
seed <- 10
sample_prob <- c(0.50, 0.50)

groups <- assign_groups(
  sample_size = sample_size,
  prob0 = prob0,
  prob1 = prob1,
  seed = 10,
  sample_prob = sample_prob
)



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
    expect_equal(
      colnames(ordinal_tests(groups[["x"]], groups[["y"]])),
      c("Wilcoxon", "Fisher", "Chi Squared (No Correction)", "Chi Squared (Correction)", "Prop. Odds", "Coin Indep. Test")
    )
  )
})


# write tests that check the assertthat messages in ordinal_tests

test_that("ordinal_tests throws an error if x and y are not the same length", {
  expect_error(ordinal_tests(c(1, 2, 3), c(1, 2, 3, 4)), "x and y must have the same length")
})

test_that("ordinal_tests throws an error if included is not a character vector", {
  expect_error(ordinal_tests(c(1, 2, 3), c(1, 2, 3), included = 1), "included must be a character vector")
})

test_that("ordinal_tests throws an error if included is not a subset of the possible tests", {
  expect_error(ordinal_tests(c(1, 2, 3), c(1, 2, 3), included = c("all", "Wilcoxon", "Fisher", "Chi Squared (No Correction)", "Chi Squared (Correction)", "Prop. Odds", "Coin Indep. Test", "Not a test")), "included must be a subset of the possible tests")
})

test_that("ordinal_tests functions correctly on a subset of tests", {
  subset_results <- suppressWarnings(
    ordinal_tests(
      groups[["x"]], groups[["y"]],
      included = c("Wilcoxon", "Fisher")
    )
  )
  expect_true(is.matrix(subset_results))
  expect_equal(colnames(subset_results), c("Wilcoxon", "Fisher"))
  expect_equal(as.vector(subset_results)[1],
    c(0.002205771),
    tolerance = 0.0000001
  )

  expect_length(subset_results, 2)
})

test_that("test each individual statistical test", {
  suppressWarnings({
    set.seed(1)
    expect_equal(ordinal_tests(groups[["x"]], groups[["y"]], included = "Wilcoxon")[[1]], 0.002205771, tolerance = 0.000001)
    expect_equal(ordinal_tests(groups[["x"]], groups[["y"]], included = "Fisher")[[1]], 0.00249875, tolerance = 0.000001)
    expect_equal(ordinal_tests(groups[["x"]], groups[["y"]], included = "Chi Squared (No Correction)")[[1]], 0.00364612, tolerance = 0.000001)
    expect_equal(ordinal_tests(groups[["x"]], groups[["y"]], included = "Chi Squared (Correction)")[[1]], 0.00364612, tolerance = 0.000001)
    expect_equal(ordinal_tests(groups[["x"]], groups[["y"]], included = "Prop. Odds")[[1]], 0.00101583, tolerance = 0.000001)
    expect_equal(ordinal_tests(groups[["x"]], groups[["y"]], included = "Coin Indep. Test")[[1]], 0.002022075, tolerance = 0.000001)
  })
})
