# write an example using the run_simulations() function

run_sims_output <- suppressWarnings(
  run_simulations(
    sample_size = 40:45,
    sample_prob = c(0.5, 0.5),
    prob0 = c(0.1, 0.2, 0.3, 0.4),
    prob1 = c(0.6, 0.2, 0.1, 0.1),
    niter = 100,
    included = "all"
  )
)


# write test that run_sims_output is a list

test_that("run_simulations returns a list", {
  expect_type(run_sims_output, "list")
})

# write test that run_sims_output has the correct number of elements

test_that("run_simulations returns a list of the correct length", {
  expect_length(run_sims_output, 6)
})

# write test that run_sims_output has the correct names

test_that("run_simulations returns a list with the correct names", {
  expect_named(
    run_sims_output,
    c(
      "sample_size_40", "sample_size_41", "sample_size_42",
      "sample_size_43", "sample_size_44", "sample_size_45"
    )
  )
})

# check that the list elements are data frames

test_that("run_simulations returns a list of data frames", {
  expect_s3_class(run_sims_output[["sample_size_40"]], "data.frame")
})

# check that the data frames have the correct number of rows

test_that("run_simulations returns data frames with the correct number of rows", {
  expect_equal(nrow(run_sims_output[["sample_size_40"]]), 100)
})

# check that the data frames have the correct number of columns

test_that("run_simulations returns data frames with the correct number of columns", {
  expect_length(colnames(run_sims_output[["sample_size_40"]]), 13)
})

# check that the data frames have the correct column names

test_that("run_simulations returns data frames with the correct column names", {
  expect_named(
    run_sims_output[["sample_size_40"]],
    c(
      "Wilcoxon", "Fisher", "Chi Squared (No Correction)",
      "Chi Squared (Correction)", "Prop. Odds", "Coin Indep. Test",
      "run", "y", "x", "n_null", "n_intervene", "sample_size", "K"
    )
  )
})
