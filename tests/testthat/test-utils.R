

# tests for parse_ratio_text()
test_that("ratio text parsing works", {
  expect_equal(parse_ratio_text("40:60"), c(0.4,0.6))
  expect_error(parse_ratio_text("4:60"))
  expect_error(parse_ratio_text("4:6"))
  expect_error(parse_ratio_text("99:2"))
  expect_error(parse_ratio_text("400:6"))
})



# tests for calculate_power_t2error()
test_that("power and t2 error calculations work", {
  # column name checks? p-value range checks? other errors to check for in the fxn itself?
  t2_power_result <- calculate_power_t2error(simulation_data_two_groups_formatted$p_values)
  expect_equal(names(t2_power_result),
               c('test', 'lower_power_bound', 'upper_power_bound', 'power', 'Power 95% CI Interval',
                 'lower_t2error_bound', 'upper_t2error_bound', 't2_error', 'TII Error 95% CI Interval', 'Sample Size'))
})



# tests for calculate_t1_error()
test_that("t1 error calculations work", {
  t1_error_result <- calculate_t1_error(simulation_data_one_group_formatted$p_values)
  expect_equal(names(t1_error_result),
               c('test', 'lower_t1_bound', 'upper_t1_bound', 't1_error', '95% CI Interval', 'Sample Size'))
})



# test data
test_that("data object names are consistent",{
  expect_equal(names(simulation_data_two_groups), c("p_values","initial_groups"))
  expect_equal(names(simulation_data_two_groups_formatted), c("p_values","run_info"))

  expect_equal(names(simulation_data_one_group), c("p_values","initial_groups"))
  expect_equal(names(simulation_data_one_group_formatted), c("p_values","run_info"))

})



# Tests for format_simulation_data
test_that("data formatting works", {
  results <- format_simulation_data(simulation_data_one_group)
  expect_named(results)
  expect_equal(length(results), 2)

  expect_type(results, "list")
  expect_type(results[[1]], "list")
  expect_type(results[[2]], "list")
  expect_s3_class(results[[1]], "tbl")
  expect_s3_class(results[[2]], "tbl")
})


