

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

})



# tests for calculate_t1_error()
test_that("t1 error calculations work", {

})
