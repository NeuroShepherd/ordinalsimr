# tests for parse_ratio_text()
test_that("ratio text parsing works", {
  expect_equal(parse_ratio_text("40:60"), c(0.4, 0.6))
  expect_error(parse_ratio_text("4:60"))
  expect_error(parse_ratio_text("4:6"))
  expect_error(parse_ratio_text("99:2"))
  expect_error(parse_ratio_text("400:6"))
})



# tests for calculate_power_t2error()
test_that("power and t2 error calculations work", {
  # column name checks? p-value range checks? other errors to check for in the fxn itself?
  t2_power_result <- simulation_data_two_groups %>%
    select(wilcox:coinasymp, sample_size) %>%
    calculate_power_t2error()
  expect_equal(
    names(t2_power_result),
    c(
      "Sample Size", "test", "lower_power_bound", "upper_power_bound", "power", "Power 95% CI",
      "lower_t2error_bound", "upper_t2error_bound", "t2_error", "TII Error 95% CI"
    )
  )
})



# tests for calculate_t1_error()
test_that("t1 error calculations work", {
  t1_error_result <- simulation_data_one_group %>%
    select(wilcox:coinasymp, sample_size) %>%
    calculate_t1_error()
  expect_equal(
    names(t1_error_result),
    c("Sample Size", "test", "lower_t1_bound", "upper_t1_bound", "t1_error", "95% CI")
  )
})



# test data
test_that("data object names are consistent", {
  expected_col_names <- c(
    "wilcox", "fisher", "chi_sq_false", "chi_sq_true", "lrm", "kruskal",
    "coinasymp", "run", "y", "x", "n_null", "n_intervene", "sample_size", "K"
  )

  expect_equal(names(simulation_data_two_groups), expected_col_names)
  expect_equal(names(simulation_data_one_group), expected_col_names)
})



# Tests for format_simulation_data
test_that("data formatting works", {
  results <- format_simulation_data(simulation_data_one_group)
  tbl_cols <- c(
    "wilcox", "fisher", "chi_sq_false", "chi_sq_true", "lrm", "kruskal",
    "coinasymp", "run", "y", "x", "n_null", "n_intervene", "sample_size", "K"
  )

  expect_equal(colnames(results), tbl_cols)
  expect_s3_class(results, "tbl")
  expect_equal(ncol(results), 14)
})
